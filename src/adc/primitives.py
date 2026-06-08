import asyncio
import shutil
import subprocess
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any, Optional


# ============================================================================
# Variable Context
# ============================================================================

@dataclass
class VariableContext:
    """Context for accessing variables and paths during part execution."""

    profile_data: dict[str, Any]
    part_name: str
    part_dir: Path
    _cache: dict[str, Any] = field(default_factory=dict)

    def get(self, key: str, default: Any = None) -> Any:
        """Get a variable from profile or part metadata."""
        # Check part-specific variables first
        part_vars = self._cache.get('part_vars', {})
        if key in part_vars:
            return part_vars[key]

        # Fall back to profile variables
        return self.profile_data.get(key, default)

    def set(self, key: str, value: Any) -> None:
        """Set a variable in the current context."""
        self._cache[key] = value

    @property
    def profile_name(self) -> str:
        """Get the profile name."""
        return self.profile_data.get('name', 'unnamed')

    def part_dir(self) -> Path:
        """Get the part's directory."""
        return self.part_dir

    def files_dir(self) -> Path:
        """Get the part's files/ directory."""
        return self.part_dir / 'files'

    def is_os(self, *os_names: str) -> bool:
        """Check if current system matches any of the given OS names."""
        import platform

        system = platform.system().lower()
        if 'linux' in system:
            # Try to detect distro
            try:
                with open('/etc/os-release') as f:
                    content = f.read().lower()
                    for os_name in os_names:
                        if os_name.lower() in content:
                            return True
            except FileNotFoundError:
                pass
            return False

        return system in [os.lower() for os in os_names]

    def is_arch(self, arch: str) -> bool:
        """Check if current system architecture matches."""
        import platform
        return platform.machine().lower() == arch.lower()

    def has_command(self, command: str) -> bool:
        """Check if a command is available on the system."""
        return shutil.which(command) is not None


# Global context (set by loader during execution)
_current_context: Optional[VariableContext] = None


def _get_context() -> VariableContext:
    """Get the current variable context."""
    if _current_context is None:
        raise RuntimeError(
            "No active context. Variables can only be accessed during task execution."
        )
    return _current_context


# ============================================================================
# File Primitive
# ============================================================================

async def file(
    path: Path | str,
    state: str = 'file',
    src: Optional[Path | str] = None,
    force: bool = False,
    mode: Optional[str] = None,
    recurse: bool = False,
) -> bool:
    """Manage files, directories, and symlinks.

    Args:
        path: Target path to manage
        state: Desired state ('file', 'directory', 'link', 'absent')
        src: Source path (for links and copies)
        force: If True, overwrite/remove existing files
        mode: File mode (e.g., '0755')
        recurse: If True, apply recursively (for directories)

    Returns:
        True if successful, False otherwise

    Examples:
        # Create a symlink
        await file(
            path='~/.config/nvim',
            src='~/dotfiles/nvim',
            state='link'
        )

        # Create a directory
        await file(
            path='~/.backups',
            state='directory'
        )

        # Remove a file
        await file(
            path='~/.oldconfig',
            state='absent'
        )
    """
    path = Path(path).expanduser()
    if src:
        src = Path(src).expanduser()

    try:
        if state == 'absent':
            # Remove file/directory/link
            if path.exists() or path.is_symlink():
                if path.is_dir() and not path.is_symlink():
                    if recurse:
                        shutil.rmtree(path)
                    else:
                        path.rmdir()
                else:
                    path.unlink()
            return True

        elif state == 'directory':
            # Create directory
            path.mkdir(parents=True, exist_ok=True)
            if mode:
                path.chmod(int(mode, 8))
            return True

        elif state == 'link':
            # Create symlink
            if not src:
                raise ValueError("src is required for state='link'")

            # Ensure parent exists
            path.parent.mkdir(parents=True, exist_ok=True)

            # Handle existing target
            if path.exists() or path.is_symlink():
                if not force:
                    print(f"  ⚠️  Target exists: {path}")
                    return False
                path.unlink()

            # Create symlink
            target_is_dir = src.is_dir() if src.exists() else False
            path.symlink_to(src, target_is_directory=target_is_dir)
            return True

        elif state == 'file':
            # Copy file
            if not src:
                raise ValueError("src is required for state='file'")

            path.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(src, path)

            if mode:
                path.chmod(int(mode, 8))

            return True

        else:
            raise ValueError(f"Unknown state: {state}")

    except Exception as e:
        print(f"  ❌ File operation failed: {e}")
        return False


# ============================================================================
# Command Primitive
# ============================================================================

async def command(
    cmd: str,
    args: list[str],
    chdir: Optional[Path | str] = None,
    creates: Optional[Path | str] = None,
    sudo: bool = False,
    raw: bool = False,
    **kwargs
) -> tuple[int, str, str]:
    """Run a command.

    Args:
        cmd: Command to run
        args: Command arguments
        chdir: Working directory
        creates: If this path exists, skip the command
        sudo: Run with sudo
        raw: If True, return raw output; otherwise print on error
        **kwargs: Additional arguments to subprocess.run

    Returns:
        Tuple of (return_code, stdout, stderr)

    Examples:
        # Simple command
        await command('git', ['clone', 'https://repo.git'])

        # Command with creates (idempotent)
        await command(
            'make',
            ['install'],
            creates='/usr/local/bin/myapp'
        )

        # Command with sudo
        await command(
            'dnf',
            ['install', '-y', 'package'],
            sudo=True
        )
    """
    # Check creates condition
    if creates:
        creates_path = Path(creates).expanduser()
        if creates_path.exists():
            return (0, '', '')  # Already done

    # Build command
    full_cmd = []
    if sudo:
        full_cmd.append('sudo')
    full_cmd.append(cmd)
    full_cmd.extend(args)

    # Set working directory
    if chdir:
        chdir = Path(chdir).expanduser()

    try:
        # Run in executor to avoid blocking
        loop = asyncio.get_event_loop()

        def _run():
            result = subprocess.run(
                full_cmd,
                capture_output=True,
                text=True,
                cwd=chdir,
                **kwargs
            )
            return result.returncode, result.stdout, result.stderr

        returncode, stdout, stderr = await loop.run_in_executor(None, _run)

        if returncode != 0 and not raw:
            print(f"  ❌ Command failed: {' '.join(full_cmd)}")
            if stderr:
                print(f"     {stderr.strip()}")

        return (returncode, stdout, stderr)

    except Exception as e:
        print(f"  ❌ Command error: {e}")
        return (-1, '', str(e))


# ============================================================================
# Backup Primitive
# ============================================================================

async def backup(
    src: Path | str,
    backup_dir: Optional[Path | str] = None,
    suffix: str = '.backup'
) -> bool:
    """Backup a file before modifying it.

    Args:
        src: Source file to backup
        backup_dir: Directory to store backups (default: ~/.backups)
        suffix: Suffix to add to backup filename

    Returns:
        True if successful or nothing to backup, False on error
    """
    src = Path(src).expanduser()

    if not src.exists():
        return True  # Nothing to backup

    if not backup_dir:
        backup_dir = Path.home() / '.backups'
    else:
        backup_dir = Path(backup_dir).expanduser()

    backup_dir.mkdir(parents=True, exist_ok=True)

    # Create backup filename
    backup_name = f"{src.name}{suffix}"
    backup_path = backup_dir / backup_name

    try:
        shutil.copy2(src, backup_path)
        print(f"  ✓ Backed up {src} -> {backup_path}")
        return True
    except Exception as e:
        print(f"  ❌ Backup failed: {e}")
        return False


# ============================================================================
# Task Decorator
# ============================================================================

def task(description: str = ""):
    """Decorator to mark a function as a task.

    Currently just marks the function for documentation/organization.
    In the future, could add logging, timing, error handling, etc.

    Args:
        description: Human-readable description of the task

    Example:
        @task('Install dependencies')
        async def install_deps():
            await command('dnf', ['install', '-y', 'pkg1', 'pkg2'])
    """
    def decorator(func):
        func._adc_task = True
        func._adc_description = description
        return func
    return decorator


# ============================================================================
# Fetch Primitive (for downloading files)
# ============================================================================

async def fetch(
    url: str,
    dest: Path | str,
    encoding: Optional[str] = 'utf-8',
    **kwargs
) -> bool:
    """Download a file from a URL.

    Args:
        url: URL to download from
        dest: Destination path
        encoding: Text encoding (None for binary)
        **kwargs: Additional arguments (e.g., headers)

    Returns:
        True if successful, False otherwise
    """
    dest = Path(dest).expanduser()
    dest.parent.mkdir(parents=True, exist_ok=True)

    try:
        # Use urllib to avoid external dependencies
        import urllib.request

        loop = asyncio.get_event_loop()

        def _download():
            urllib.request.urlretrieve(url, dest)

        await loop.run_in_executor(None, _download)

        print(f"  ✓ Downloaded {url} -> {dest}")
        return True

    except Exception as e:
        print(f"  ❌ Download failed: {e}")
        return False


# ============================================================================
# Convenience Functions
# ============================================================================

def variable() -> VariableContext:
    """Get the current variable context."""
    return _get_context()


def path(p: str) -> Path:
    """Create a Path from a string (convenience)."""
    return Path(p)


# ============================================================================
# Context Manager for Task Execution
# ============================================================================

from contextlib import contextmanager

@contextmanager
def task_context(profile_data: dict, part_name: str, part_dir: Path):
    """Context manager for executing tasks with proper variable context.

    Usage:
        with task_context(profile, 'nvim', nvim_dir):
            # Now variable(), file(), command() etc. are available
            await some_task()
    """
    global _current_context
    old_context = _current_context

    try:
        _current_context = VariableContext(
            profile_data=profile_data,
            part_name=part_name,
            part_dir=Path(part_dir)
        )
        yield _current_context
    finally:
        _current_context = old_context
