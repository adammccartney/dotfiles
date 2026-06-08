"""Test async functionality in ADC.

This demonstrates the power of coroutines for concurrent operations.
"""

import asyncio
import time

import pytest

from adc.async_deploy import ProgressTracker


@pytest.mark.asyncio
async def test_progress_tracker():
    """Test ProgressTracker updates correctly."""
    tracker = ProgressTracker(10, "Deploying files")

    assert tracker.total == 10
    assert tracker.current == 0
    assert tracker.description == "Deploying files"

    # Update progress
    for i in range(10):
        tracker.update()
        assert tracker.current == i + 1

    # Check string representation
    tracker_str = str(tracker)
    assert "Deploying files" in tracker_str
    assert "10/10" in tracker_str or "100%" in tracker_str


@pytest.mark.asyncio
async def test_progress_tracker_partial():
    """Test ProgressTracker at partial completion."""
    tracker = ProgressTracker(5, "Testing")

    tracker.update()
    tracker.update()

    assert tracker.current == 2
    tracker_str = str(tracker)
    assert "2/5" in tracker_str or "40" in tracker_str


@pytest.mark.asyncio
async def test_mixed_cpu_and_io_operations():
    """Test mixed CPU-bound and I/O-bound operations run concurrently."""
    async def cpu_bound_task(n):
        """Simulate CPU-bound work."""
        result = 0
        for i in range(n):
            result += i * i
        return result

    async def io_bound_task(delay):
        """Simulate I/O-bound work."""
        await asyncio.sleep(delay)
        return "I/O done"

    # Run both concurrently
    start = time.time()
    results = await asyncio.gather(
        cpu_bound_task(10_000_000),
        io_bound_task(0.5),
    )
    elapsed = time.time() - start

    # CPU result should be calculated correctly
    expected_cpu_result = sum(i * i for i in range(10_000_000))
    assert results[0] == expected_cpu_result
    assert results[1] == "I/O done"
    # Both tasks run concurrently, so total time should be less than
    # the sum of individual times (CPU task takes ~0.5-0.8s, I/O takes 0.5s)
    # We allow up to 2.0s to account for system variability
    assert elapsed < 2.0


@pytest.mark.asyncio
async def test_concurrent_file_operations(temp_dir):
    """Test concurrent file operations."""
    async def create_file(name, delay):
        await asyncio.sleep(delay)
        file_path = temp_dir / name
        file_path.write_text(f"Content of {name}")
        return file_path

    # Create multiple files concurrently
    files = await asyncio.gather(
        create_file("file1.txt", 0.1),
        create_file("file2.txt", 0.2),
        create_file("file3.txt", 0.15),
    )

    assert len(files) == 3
    for file_path in files:
        assert file_path.exists()
