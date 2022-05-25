python-serd
===========

This is the Python `serd` module, which provides bindings for Serd,
a lightweight C library for working with RDF data in [Turtle][],
[NTriples][], [NQuads][], and [TriG][] formats.

Requirements
------------

Building this package requires serd to be installed.
Specifically, [pkg-config][] must be able to find `serd-1`:

    pkg-config --modversion serd-1

Note that if serd is installed to a non-standard prefix,
you will need to configure `PKG_CONFIG_PATH` in your environment.
See the [guide to pkg-config](https://people.freedesktop.org/~dbn/pkg-config-guide.html) for more information.

The [pip][] packages `wheel`, `cython`, and `sphinx` are required for building:

    pip install wheel cython sphinx

A meson configuration is included that can fetch and build serd as a submodule,
and build these bindings against that library,
but this is intended for development and its use is unsupported.

Installation
------------

This package can be installed with `pip`:

    cd path_to_python_serd_source
    pip install .

By default, this will install locally for your user only.
For other possibilities, consult the `pip` documentation.

Note that the generated C sources are not stored in git.
To build from a git clone, `CYTHONIZE` must be enabled in the environment:

    CYTHONIZE=1 pip install .

Documentation
-------------

The latest API reference documentation is available online:

   * [Paginated HTML](https://drobilla.gitlab.io/python-serd/html)
   * [Single page HTML](https://drobilla.gitlab.io/python-serd/singlehtml)
   * [EPUB](https://drobilla.gitlab.io/python-serd/Using-Serd-in-Python.epub)

Development
-----------

Development targets can be built using `setup.py` directly.

    CYTHONIZE=1 python setup.py build

The documentation can be built with the `build_sphinx` command:

    python setup.py build_sphinx

The unit tests and doctests can be run with the `test` and `doctest` commands, respectively:

    python setup.py test doctest

The generated documentation can then be found in `build/sphinx`.

 -- David Robillard <d@drobilla.net>

[Turtle]: https://www.w3.org/TR/turtle/
[NTriples]: https://www.w3.org/TR/n-triples/
[NQuads]: https://www.w3.org/TR/n-quads/
[TriG]: https://www.w3.org/TR/trig/
[pkg-config]: https://www.freedesktop.org/wiki/Software/pkg-config/
[pip]: https://pypi.org/project/pip/
