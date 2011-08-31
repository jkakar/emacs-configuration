#!/usr/bin/env python

import re
import sys
from subprocess import Popen, PIPE


class Checker(object):

    def check(self):
        pipe = Popen(self._command, shell = True, bufsize = -1, stdin = PIPE,
                     stdout = PIPE, stderr = PIPE, close_fds = True)
        for line in pipe.stdout:
            line = line.strip()
            match = re.compile(self._regex).match(line)
            if match:
                print self.format(match)


class PyFlakes(Checker):

    def __init__(self):
        self._command = "pyflakes %s" % sys.argv[1]
        self._regex = "^([^:]+):(\d+):\s+(.*)$"

    def format(self, match):
        filename, line_number, description = match.groups()
        message = "Warning"
        return "%s %s at %s line %s." % (message, description, filename,
                                         line_number)


class PyLint(Checker):

    def __init__(self):
        checkers = ["basic", "typecheck", "variables", "classes", "design"]
        disabled_messages = [
            "C0103", # Invalid name
            "C0111", # Missing docstring
            "I0011", # Warning locally suppressed
            "I0012", # Warning locally suppressed
            "W0142", # *args or **kwargs magic.
            "E0611", # No name X in module Y
            "E1101", # Instance of 'X' has no 'Y' member
            "W0221", # Arguments differ from overridden method
            "W0511", # FIXME/TODO
            "W0622", # Redefining built-in
            "R0904", # Too many public methods
            "R0201", # Method could be a function
            "R0903", # Too few public methods
            "W0212", # Access to a protected memberof a client class
            "W0231", # __init__ method from base class 'X' is not called
            "W0232", # Class has no __init__ method
        ]
        self._command = "pylint --output-format parseable --include-ids y "\
                        "--enable=%s --disable=%s --reports n %s"\
                        % (",".join(checkers), ",".join(disabled_messages),
                           sys.argv[1])
        self._regex = (
            "^([^:]+):(\d+):\s*\[([WECR])([^,\]]+),?\s*([^\]]*)\]\s*(.*)$")

    def format(self, match):
        (filename, linenum, errtype, errnum, _, description) = match.groups()
        if errtype == "E":
            msg = "Error"
        else:
            msg = "Warning"
        # Here we are targetting the following Flymake regexp:
        #
        #  ("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)
        #
        # where the number at the end indicate the index into the regexp
        # groups of ( file, line, column, error text )
        #
        # You can see what regular expressions Flymake uses to parse its
        # output by running 'M-x describe-variable' on the variable
        # 'flymake-err-line-patterns'
        return "%s %s%s %s at %s line %s." % (msg, errtype, errnum,
                                              description, filename, linenum)


class PEP8(Checker):

    def __init__(self):
        self._command = "pep8.py %s" % sys.argv[1]
        self._regex = "^([^:]+):(\d+):(\d+):\s*([WECR])(\d+)\s+(.*)$"

    def format(self, match):
        (filename, linenum, _, errtype, errnum, description) = match.groups()
        if errtype == "E":
            msg = "Warning"
        else:
            msg = "Error"
        return "%s %s%s %s at %s line %s." % (msg, errtype, errnum,
                                              description, filename, linenum)


# pylint = PyLint()
# pylint.check()
pyflakes = PyFlakes()
pyflakes.check()
pep8 = PEP8()
pep8.check()
