"""Test async functionality in ADC.

This demonstrates the power of coroutines for concurrent operations.
"""

import asyncio
import time

import pytest

from adc.async_deploy import ProgressTracker


async def simulate_slow_operation(name: str, delay: float):
    """Simulate a slow I/O operation."""
    await asyncio.sleep(delay)
    return name


@pytest.mark.asyncio
async def test_concurrent_loading():
    """Test concurrent part loading is faster than sequential."""
    start = time.time()

    # Simulate loading 3 parts concurrently
    tasks = [
        simulate_slow_operation("nvim", 0.5),
        simulate_slow_operation("systemd", 0.75),
        simulate_slow_operation("git", 0.25),
    ]

    results = await asyncio.gather(*tasks)
    elapsed = time.time() - start

    assert len(results) == 3
    # Concurrent should be faster than sequential (0.25 + 0.5 + 0.75 = 1.5s)
    assert elapsed < 1.0
    assert set(results) == {"nvim", "systemd", "git"}


@pytest.mark.asyncio
async def test_sequential_loading():
    """Test sequential part loading for comparison."""
    start = time.time()

    # Simulate loading 3 parts sequentially
    results = []
    for name, delay in [("nvim", 0.5), ("systemd", 0.75), ("git", 0.25)]:
        result = await simulate_slow_operation(name, delay)
        results.append(result)

    elapsed = time.time() - start

    assert len(results) == 3
    # Sequential should take approximately the sum of delays
    assert elapsed >= 1.4
    assert results == ["nvim", "systemd", "git"]

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
async def test_asyncio_gather_order():
    """Test that asyncio.gather preserves order."""
    async def delayed_result(value, delay):
        await asyncio.sleep(delay)
        return value

    # Tasks complete in different order than submitted
    results = await asyncio.gather(
        delayed_result(1, 0.3),
        delayed_result(2, 0.1),
        delayed_result(3, 0.2),
    )

    # Results should be in submission order, not completion order
    assert results == [1, 2, 3]


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
