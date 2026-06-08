"""Async part loader for ADC.

Uses coroutines for concurrent part discovery, validation, and deployment.
Works with the primitives-based part model.
"""

import asyncio
import importlib
import json
from pathlib import Path
from typing import Any

from .primitives import task_context


async def load_part(part_name: str):
    """Dynamically load a part by name.

    Args:
        part_name: Name of the part (e.g., "nvim", "systemd")

    Returns:
        The loaded module
    """
    # Run import in executor to avoid blocking
    loop = asyncio.get_event_loop()
    module_path = f"adc.parts.{part_name}"

    def _import():
        return importlib.import_module(module_path)

    return await loop.run_in_executor(None, _import)


async def load_profile_async(profile_path: str | Path) -> dict[str, Any]:
    """Load a profile.json file asynchronously.

    Args:
        profile_path: Path to profile.json

    Returns:
        Profile dictionary with 'name' and 'parts' keys
    """
    loop = asyncio.get_event_loop()
    profile_path = Path(profile_path)

    def _load():
        with open(profile_path) as f:
            return json.load(f)

    return await loop.run_in_executor(None, _load)


async def validate_part(part_name: str, part) -> tuple[str, bool]:
    """Validate a single part.

    Args:
        part_name: Name of the part
        part: Loaded part module

    Returns:
        Tuple of (part_name, is_valid)
    """
    # Run validation in executor to avoid blocking
    loop = asyncio.get_event_loop()

    def _validate():
        if hasattr(part, 'validate'):
            return part.validate()
        return True  # No validate function = assume valid

    is_valid = await loop.run_in_executor(None, _validate)
    return (part_name, is_valid)


async def validate_all_parts(parts: dict) -> dict[str, bool]:
    """Validate all parts concurrently.

    Args:
        parts: Dictionary mapping part names to part modules

    Returns:
        Dictionary mapping part names to validation results
    """
    tasks = [validate_part(name, part) for name, part in parts.items()]
    results = await asyncio.gather(*tasks, return_exceptions=True)

    validation_results = {}
    for result in results:
        if isinstance(result, Exception):
            print(f"  ⚠️  Validation error: {result}")
            continue
        name, is_valid = result
        validation_results[name] = is_valid

    return validation_results


async def configure_part(
    part_name: str,
    part,
    profile_data: dict[str, Any],
    dry_run: bool = True
) -> tuple[str, bool]:
    """Configure a single part.

    Calls the part's configure() function with proper context.

    Args:
        part_name: Name of the part
        part: Loaded part module
        profile_data: Profile dictionary
        dry_run: If True, only print what would be done

    Returns:
        Tuple of (part_name, success)
    """
    try:
        part_dir = Path(part.__file__).parent

        if dry_run:
            print(f"  [DRY RUN] Would configure {part_name}")
            return (part_name, True)

        # Check if part has async configure method
        if hasattr(part, 'configure'):
            if asyncio.iscoroutinefunction(part.configure):
                # Async configure
                await part.configure(profile_data)
            else:
                # Sync configure - run in executor
                loop = asyncio.get_event_loop()
                await loop.run_in_executor(
                    None,
                    lambda: part.configure(profile_data)
                )
        else:
            print(f"  ⚠️  Part {part_name} has no configure() function")
            return (part_name, False)

        return (part_name, True)

    except Exception as e:
        print(f"  ❌ Error configuring {part_name}: {e}")
        import traceback
        traceback.print_exc()
        return (part_name, False)


async def apply_profile_async(
    profile: dict[str, Any],
    dry_run: bool = True,
    parallel: bool = True
) -> dict[str, bool]:
    """Apply all parts from a profile asynchronously.

    Args:
        profile: Profile dictionary
        dry_run: If True, only print what would be done
        parallel: If True, process parts concurrently

    Returns:
        Dictionary mapping part names to success status
    """
    print(f"Applying profile: {profile.get('name', 'unnamed')}")
    print(f"Parts: {profile.get('parts', [])}")
    print()

    results = {}
    part_names = profile.get("parts", [])

    if parallel:
        # Load all parts concurrently
        print("Loading parts concurrently...")
        load_tasks = []
        for part_name in part_names:
            load_tasks.append(_load_part_safe(part_name))

        load_results = await asyncio.gather(*load_tasks, return_exceptions=True)

        parts_dict = {}
        for i, result in enumerate(load_results):
            part_name = part_names[i]
            if isinstance(result, Exception):
                print(f"  ❌ Failed to load {part_name}: {result}")
                results[part_name] = False
            else:
                parts_dict[part_name] = result

        # Validate all parts concurrently
        if parts_dict:
            print("Validating parts concurrently...")
            validation_results = await validate_all_parts(parts_dict)

            for part_name, is_valid in validation_results.items():
                if not is_valid:
                    print(f"  ⚠️  Part {part_name} is not compatible with this system")
                    results[part_name] = False
                    del parts_dict[part_name]

        # Configure remaining parts concurrently
        if parts_dict:
            print("Configuring parts concurrently...")
            config_tasks = []
            for part_name, part in parts_dict.items():
                config_tasks.append(
                    configure_part(part_name, part, profile, dry_run)
                )

            config_results = await asyncio.gather(*config_tasks)
            for part_name, success in config_results:
                results[part_name] = success
                status = "✓" if success else "❌"
                mode = "[DRY RUN]" if dry_run else "[APPLIED]"
                print(f"  {status} {mode} Configured {part_name}")
    else:
        # Sequential processing
        for part_name in part_names:
            print(f"Loading part: {part_name}")
            try:
                part = await load_part(part_name)

                # Validate
                loop = asyncio.get_event_loop()
                def _validate():
                    if hasattr(part, 'validate'):
                        return part.validate()
                    return True

                is_valid = await loop.run_in_executor(None, _validate)

                if not is_valid:
                    print(f"  ⚠️  Part {part_name} is not compatible")
                    results[part_name] = False
                    continue

                # Configure
                if dry_run:
                    print(f"  [DRY RUN] Would configure {part_name}")
                    results[part_name] = True
                else:
                    await configure_part(part_name, part, profile, dry_run=False)
                    results[part_name] = True

            except Exception as e:
                print(f"  ❌ Failed: {e}")
                results[part_name] = False

    return results


async def _load_part_safe(part_name: str):
    """Load a part, raising exception on failure."""
    return await load_part(part_name)


async def list_available_parts_async() -> list[str]:
    """List all available parts asynchronously."""
    loop = asyncio.get_event_loop()
    parts_dir = Path(__file__).parent / "parts"

    def _scan():
        return [
            d.name for d in parts_dir.iterdir()
            if d.is_dir() and not d.name.startswith("_")
        ]

    return await loop.run_in_executor(None, _scan)

