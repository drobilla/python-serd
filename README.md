python-serd
===========

This is the Python `serd` module, which provides bindings for Serd,
a lightweight C library for working with RDF data in [Turtle][],
[NTriples][], [NQuads][], and [TriG][] formats.

Installation
------------

This package can be installed with `pip`:

    cd path_to_python_serd_source
    pip install .

By default, this will install locally for your user only.
For other possibilities, consule the `pip` documentation.

Development
-----------

Building the documentation and running tests can be done by using `setup.py` directly.

For building from only source (for example, from the git repository),
the `CYTHONIZE` environment variable must be set to explicitly request generating the C bindings:

    CYTHONIZE=1 python setup.py build

Documentation
-------------

The latest API reference documentation is available online:

   * [Paginated HTML](https://drobilla.gitlab.io/python-serd/html)
   * [Single page HTML](https://drobilla.gitlab.io/python-serd/singlehtml)
   * [EPUB](https://drobilla.gitlab.io/python-serd/Using-Serd-in-Python.epub)

The documentation can be built from the source repository via the `build_sphinx` target:

    python setup.py build_sphinx

Several formats of the documentation will be generated in `build/sphinx`.

Tests
-----

Several unit tests are included,
and the documentation includes inlined tests to ensure that the code examples work properly.

The unit tests can be run with the `test` target,
and the documentation tests with the `doctest` target:

    python setup.py test doctest

 -- David Robillard <d@drobilla.net>

[Turtle]: https://www.w3.org/TR/turtle/
[TriG]: https://www.w3.org/TR/trig/
[NTriples]: https://www.w3.org/TR/n-triples/
[NQuads]: https://www.w3.org/TR/n-quads/
