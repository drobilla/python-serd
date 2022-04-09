# Copyright 2020-2022 David Robillard <d@drobilla.net>
# SPDX-License-Identifier: CC0-1.0 OR ISC

#!/usr/bin/env python3

"""The configuration script for serd-python."""

import os
import subprocess

from setuptools import setup, Command, Extension

try:
    from Cython.Build import cythonize
except ImportError:
    cythonize = None


def pkgconfig(package):
    """Return Extension arguments for a pkg-config package."""

    flag_map = {"-I": "include_dirs", "-L": "library_dirs", "-l": "libraries"}
    output = subprocess.run(
        ["pkg-config", "--cflags", "--libs", f"{package}"],
        capture_output=True,
        check=True,
        encoding="utf-8",
    )

    result = {}
    for token in output.stdout.strip().split():
        result.setdefault(flag_map.get(token[:2]), []).append(token[2:])

    return result


VERSION = "1.0.1"

serd_config = pkgconfig("serd-1")
extensions = [Extension("serd", ["serd.pyx"], **serd_config)]
serd_cython_ext = Extension("serd", ["serd.pyx"], **serd_config)
serd_ext = Extension("serd", ["serd.c"], **serd_config)

CYTHONIZE = bool(int(os.getenv("CYTHONIZE", "0"))) and cythonize is not None

if CYTHONIZE:
    extensions = cythonize(
        [serd_cython_ext],
        compiler_directives={
            "language_level": 3,
            "embedsignature": True,
        },
    )
else:
    extensions = [serd_ext]


class Doctest(Command):
    """A command to run the doctests with Sphinx."""

    description = "Run doctests with Sphinx"
    user_options = []

    def initialize_options(self):
        """Set default values for all the options that this command supports."""

    def finalize_options(self):
        """Set final values for all the options that this command supports."""

    def run(self):
        """Run the command, calling sphinx to run the doctests."""

        # pragma pylint: disable=import-outside-toplevel

        from sphinx.application import Sphinx

        sph = Sphinx(
            "doc",  # source
            "doc",  # directory with conf.py
            "build/sphinx",  # output
            "build/sphinx/doctrees",  # doctrees directory
            "doctest",  # target
        )

        self.announce("Running doctests with sphinx")
        sph.build()

        # pragma pylint: enable=import-outside-toplevel


with open("README.md", "r", encoding="utf-8") as readme_md:
    long_description = readme_md.read()

setup(
    extras_require={
        "build_sphinx": ["sphinx>=4.0.0", "sphinx-lv2-theme"],
        "doctest": ["sphinx>=4.0.0"],
    },
    cmdclass={"doctest": Doctest},
    name="python-serd",
    version=VERSION,
    description="A lightweight library for working with RDF data",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://gitlab.com/drobilla/serd",
    author="David Robillard",
    author_email="d@drobilla.net",
    zip_safe=False,
    license="ISC",
    ext_modules=extensions,
    command_options={
        "build_sphinx": {
            "builder": ("setup.py", ["epub", "html", "singlehtml"]),
            "nitpicky": ("setup.py", True),
            "release": ("setup.py", VERSION),
            "version": ("setup.py", VERSION),
            "warning_is_error": ("setup.py", True),
        }
    },
    install_requires=[],
    classifiers=[
        "Development Status :: 5 - Production/Stable",
        "Intended Audience :: Developers",
        "License :: OSI Approved :: ISC License (ISCL)",
        "Operating System :: POSIX",
        "Programming Language :: C",
        "Programming Language :: Cython",
        "Programming Language :: Python",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
        "Programming Language :: Python :: 3.9",
    ],
)
