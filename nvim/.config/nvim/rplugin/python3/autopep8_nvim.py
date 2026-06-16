import re
from pathlib import Path
from subprocess import PIPE, Popen

import neovim


AUTOPEP8_COMMAND = "autopep8"
AUTOPEP8_OPTIONS = [
        "--global-config",
        "--ignore-local-config",
        "--pep8-passes",
        "--aggressive",
        "--ignore",
]


@neovim.plugin
class Autopep8Nvim:
    def __init__(self, nvim):
        self.nvim = nvim

    @neovim.command(
            "Autopep8", nargs="*", range="%",
            complete="customlist,Autopep8Completions"
    )
    def autopep8_command(self, args, range):
        buffer = self.nvim.current.buffer
        text = self._get_lines(buffer, range)
        output = self._autopep8(text, *args, cwd=Path(buffer.name).parent)
        if text != output:
            lines = re.split(r"\r\n?|\n", output)
            buffer[range[0] - 1 : range[1]] = lines


    def _get_lines(self, buffer, range):
        lines = buffer[range[0] - 1 : range[1]]
        return "\n".join(lines)


    def _autopep8(self, text, *args, cwd=None):
        autopep8_command = self.nvim.vars.get("autopep8_command",
                                              AUTOPEP8_COMMAND)
        autopep8_args = [autopep8_command] + list(args) + ["-"]
        with Popen(autopep8_args, stdin=PIPE, stdout=PIPE, stderr=PIPE, cwd=cwd) as proc:
            output, error = proc.communicate(input=text.encode())
            return output.decode()

    @neovim.function("Autopep8Completions", sync=True)
    def autopep8_completions(self, args):
        arglead, cmdline, cursorpos, *_ = args
        return [option for option in AUTOPEP8_OPTIONS if option.startswith(arglead)]
