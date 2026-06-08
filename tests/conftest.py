"""Pytest fixtures and configuration for ADC tests."""

import asyncio
import tempfile
from pathlib import Path

import pytest


@pytest.fixture
def temp_dir():
    """Create a temporary directory for tests."""
    with tempfile.TemporaryDirectory() as tmpdir:
        yield Path(tmpdir)


@pytest.fixture
def sample_profile_data():
    """Sample profile data for testing."""
    return {
        "name": "test-profile",
        "custom_var": "custom_value",
        "systemd_services": ["service1", "service2"],
    }


@pytest.fixture
def part_context(sample_profile_data, temp_dir):
    """Create a part context with task_context."""
    from adc.primitives import task_context, variable

    with task_context(sample_profile_data, 'test', temp_dir):
        yield variable()


@pytest.fixture
def event_loop():
    """Create an instance of the default event loop for each test case."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()
