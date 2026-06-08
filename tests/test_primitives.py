"""Test the primitives-based part model.

This demonstrates how parts use the new primitives instead of implementing a Protocol.
"""

import asyncio
from pathlib import Path

import pytest

from adc.primitives import (
    file,
    command,
    variable,
    task,
    task_context,
    backup,
)


@pytest.mark.asyncio
async def test_file_primitive_create_directory(temp_dir, sample_profile_data):
    """Test the file() primitive can create directories."""
    test_dir = temp_dir / "test-dir"

    with task_context(sample_profile_data, 'test', temp_dir):
        success = await file(path=test_dir, state='directory')

    assert success is True
    assert test_dir.exists()
    assert test_dir.is_dir()


@pytest.mark.asyncio
async def test_file_primitive_create_symlink(temp_dir, sample_profile_data):
    """Test the file() primitive can create symlinks."""
    # Create source file
    src_file = temp_dir / "source.txt"
    src_file.write_text("Hello, World!")

    # Create symlink
    link_path = temp_dir / "link.txt"

    with task_context(sample_profile_data, 'test', temp_dir):
        success = await file(
            path=link_path,
            src=src_file,
            state='link',
            force=True
        )

    assert success is True
    assert link_path.is_symlink()
    assert link_path.read_text() == "Hello, World!"


@pytest.mark.asyncio
async def test_file_primitive_backup(temp_dir, sample_profile_data):
    """Test the backup() primitive."""
    # Create test file
    test_file = temp_dir / "test.txt"
    test_file.write_text("Original content")

    with task_context(sample_profile_data, 'test', temp_dir):
        success = await backup(src=test_file)

    assert success is True
    # Check backup was created
    backup_dir = Path.home() / '.backups'
    backup_files = list(backup_dir.glob('test.txt.backup*'))
    assert len(backup_files) > 0


@pytest.mark.asyncio
async def test_command_primitive_success(temp_dir, sample_profile_data):
    """Test the command() primitive executes successfully."""
    with task_context(sample_profile_data, 'test', temp_dir):
        returncode, stdout, stderr = await command('echo', ['Hello from ADC'])

    assert returncode == 0
    assert stdout.strip() == "Hello from ADC"


@pytest.mark.asyncio
async def test_command_primitive_idempotent(temp_dir, sample_profile_data):
    """Test command() primitive with creates parameter (idempotent)."""
    test_marker = temp_dir / "marker.txt"

    with task_context(sample_profile_data, 'test', temp_dir):
        # First run - should execute
        returncode1, _, _ = await command(
            'touch',
            [str(test_marker)],
            creates=str(test_marker)
        )

        # Second run - should skip (creates condition met)
        returncode2, stdout, stderr = await command(
            'touch',
            [str(test_marker)],
            creates=str(test_marker)
        )

    assert returncode1 == 0
    assert returncode2 == 0
    assert test_marker.exists()
    # When creates condition is met, command returns early with empty output
    assert stdout == ''
    assert stderr == ''


@pytest.mark.asyncio
async def test_variable_context_profile_name(part_context):
    """Test variable context provides profile name."""
    assert part_context.profile_name == "test-profile"


@pytest.mark.asyncio
async def test_variable_context_get_variable(part_context):
    """Test variable context can retrieve variables."""
    custom = part_context.get('custom_var')
    assert custom == "custom_value"


@pytest.mark.asyncio
async def test_variable_context_default_value(part_context):
    """Test variable context returns default for missing variables."""
    missing = part_context.get('missing_var', 'default')
    assert missing == 'default'


@pytest.mark.asyncio
async def test_variable_context_list_variable(part_context):
    """Test variable context can retrieve list variables."""
    services = part_context.get('systemd_services', [])
    assert services == ["service1", "service2"]


@pytest.mark.asyncio
async def test_variable_context_has_command(part_context):
    """Test variable context can check for available commands."""
    # Should have python available
    has_python = part_context.has_command('python')
    assert has_python is True


@pytest.mark.asyncio
async def test_variable_context_is_os(part_context):
    """Test variable context can check OS type."""
    # Should be on a Linux system (fedora, ubuntu, or debian)
    is_linux = part_context.is_os('fedora', 'ubuntu', 'debian')
    assert is_linux is True


@task('Example task with decorator')
async def example_task():
    """An example task using the @task decorator."""
    ctx = variable()
    return ctx.profile_name


@pytest.mark.asyncio
async def test_task_decorator_marks_function():
    """Test the @task decorator marks functions correctly."""
    assert hasattr(example_task, '_adc_task')
    assert getattr(example_task, '_adc_description') == 'Example task with decorator'


@pytest.mark.asyncio
async def test_task_decorator_execution(temp_dir, sample_profile_data):
    """Test the @task decorated function executes in context."""
    with task_context(sample_profile_data, 'test', temp_dir):
        result = await example_task()

    assert result == "test-profile"
