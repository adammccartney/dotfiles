"""ADC - Add Config

A uv-based command line program for configuring machines via profiles.

Parts use primitives from this package:
    from adc import file, command, variable, task

    @task('Do something')
    async def configure():
        await file(path='~/.config', state='directory')
        await command('git', ['clone', '...'])
"""

__version__ = "0.1.0"

# Export primitives for parts to use
from .primitives import (
    file,
    command,
    variable,
    task,
    backup,
    fetch,
    path,
    task_context,
    VariableContext,
)

__all__ = [
    'file',
    'command',
    'variable',
    'task',
    'backup',
    'fetch',
    'path',
    'task_context',
    'VariableContext',
]
