"""Async CLI for ADC with coroutine support."""

import argparse
import asyncio
from pathlib import Path

from .async_loader import (
    apply_profile_async,
    list_available_parts_async,
    load_profile_async,
)


async def async_main():
    """Async entry point for ADC CLI."""
    parser = argparse.ArgumentParser(
        prog="adc",
        description="Add Config - Configure machines via profiles (async version)"
    )

    parser.add_argument(
        "-p", "--profile",
        type=Path,
        required=False,
        help="Path to profile.json"
    )

    parser.add_argument(
        "--dry-run",
        action="store_true",
        default=True,
        help="Show what would be done without making changes (default: True)"
    )

    parser.add_argument(
        "--apply",
        action="store_true",
        help="Actually apply the configuration"
    )

    parser.add_argument(
        "--list-parts",
        action="store_true",
        help="List available parts"
    )

    parser.add_argument(
        "--sequential",
        action="store_true",
        help="Process parts sequentially instead of concurrently"
    )

    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="Verbose output"
    )

    args = parser.parse_args()

    if args.list_parts:
        print("Available parts:")
        parts = await list_available_parts_async()
        for part in sorted(parts):
            print(f"  - {part}")
        return

    if not args.profile:
        parser.error("--profile is required unless using --list-parts")
        return

    profile = await load_profile_async(args.profile)

    dry_run = not args.apply
    parallel = not args.sequential

    if dry_run:
        print("Running in dry-run mode (use --apply to make changes)")
        print()

    if args.verbose:
        print(f"Processing mode: {'parallel' if parallel else 'sequential'}")
        print()

    results = await apply_profile_async(
        profile,
        dry_run=dry_run,
        parallel=parallel
    )

    # Summary
    print()
    print("=" * 50)
    print("Summary:")
    successful = sum(1 for v in results.values() if v)
    total = len(results)
    print(f"  {successful}/{total} parts processed successfully")

    if successful < total:
        print("\nFailed parts:")
        for name, success in results.items():
            if not success:
                print(f"  - {name}")


def main():
    """Sync wrapper for async main."""
    asyncio.run(async_main())


if __name__ == "__main__":
    main()
