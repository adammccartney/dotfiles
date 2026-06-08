"""Async file deployment utilities.

Provides coroutine-based file operations with progress tracking.
"""

import asyncio
import shutil
from pathlib import Path
from typing import Callable, Optional


class ProgressTracker:
    """Track progress of async operations."""

    def __init__(self, total: int, description: str = ""):
        self.total = total
        self.current = 0
        self.description = description

    def update(self, n: int = 1):
        self.current += n

    def __str__(self):
        pct = (self.current / self.total * 100) if self.total > 0 else 0
        bar_len = 30
        filled = int(bar_len * self.current / self.total) if self.total > 0 else 0
        bar = "█" * filled + "░" * (bar_len - filled)
        return f"{self.description}: [{bar}] {self.current}/{self.total} ({pct:.1f}%)"


async def create_symlink_async(
    source: Path,
    target: Path,
    force: bool = False
) -> bool:
    """Create a symlink asynchronously.

    Args:
        source: Source file/directory path
        target: Target symlink path
        force: If True, remove existing target

    Returns:
        True if successful, False otherwise
    """
    loop = asyncio.get_event_loop()

    def _symlink():
        try:
            # Ensure parent directory exists
            target.parent.mkdir(parents=True, exist_ok=True)

            # Remove existing target if force
            if target.exists() or target.is_symlink():
                if not force:
                    print(f"    ⚠️  Target exists: {target}")
                    return False
                target.unlink()

            # Create symlink
            if source.is_dir():
                target.symlink_to(source, target_is_directory=True)
            else:
                target.symlink_to(source)

            return True
        except Exception as e:
            print(f"    ❌ Error creating symlink: {e}")
            return False

    return await loop.run_in_executor(None, _symlink)


async def copy_file_async(source: Path, target: Path) -> bool:
    """Copy a file asynchronously.

    Args:
        source: Source file path
        target: Target file path

    Returns:
        True if successful, False otherwise
    """
    loop = asyncio.get_event_loop()

    def _copy():
        try:
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(source, target)
            return True
        except Exception as e:
            print(f"    ❌ Error copying file: {e}")
            return False

    return await loop.run_in_executor(None, _copy)


async def deploy_files_async(
    files: list[tuple[Path, Path]],
    progress_callback: Optional[Callable[[str, int, int], None]] = None,
    use_symlinks: bool = True,
    force: bool = False
) -> dict[Path, bool]:
    """Deploy multiple files concurrently.

    Args:
        files: List of (source, target) tuples
        progress_callback: Optional callback(current_file, completed, total)
        use_symlinks: If True, create symlinks; otherwise copy files
        force: If True, overwrite existing files

    Returns:
        Dictionary mapping source paths to success status
    """
    results = {}
    total = len(files)

    async def deploy_single(source: Path, target: Path, index: int):
        if progress_callback:
            progress_callback(str(source), index, total)

        if use_symlinks:
            success = await create_symlink_async(source, target, force)
        else:
            success = await copy_file_async(source, target)

        results[source] = success

        if progress_callback:
            progress_callback(str(source), index + 1, total)

    tasks = [
        deploy_single(source, target, i)
        for i, (source, target) in enumerate(files)
    ]

    await asyncio.gather(*tasks)
    return results


async def remove_symlink_async(path: Path) -> bool:
    """Remove a symlink asynchronously (for rollback).

    Args:
        path: Path to symlink

    Returns:
        True if successful, False otherwise
    """
    loop = asyncio.get_event_loop()

    def _remove():
        try:
            if path.is_symlink():
                path.unlink()
                return True
            return False
        except Exception as e:
            print(f"  ❌ Error removing symlink: {e}")
            return False

    return await loop.run_in_executor(None, _remove)


async def rollback_deployment_async(
    deployed_files: list[Path]
) -> dict[Path, bool]:
    """Rollback deployed files by removing symlinks.

    Args:
        deployed_files: List of deployed symlink paths

    Returns:
        Dictionary mapping paths to success status
    """
    results = {}

    tasks = [remove_symlink_async(path) for path in deployed_files]
    task_results = await asyncio.gather(*tasks)

    for path, success in zip(deployed_files, task_results):
        results[path] = success

    return results


# Example usage with progress bar
async def deploy_with_progress(
    files: list[tuple[Path, Path]],
    description: str = "Deploying files"
):
    """Deploy files with a progress bar."""
    tracker = ProgressTracker(len(files), description)

    def progress_callback(file: str, completed: int, total: int):
        tracker.current = completed
        print(f"  {tracker}")

    results = await deploy_files_async(files, progress_callback=progress_callback)
    return results
