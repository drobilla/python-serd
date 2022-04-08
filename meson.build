# Copyright 2020-2022 David Robillard <d@drobilla.net>
# SPDX-License-Identifier: CC0-1.0 OR ISC

project('python-serd', ['c'],
        version: '1.0.1',
        license: 'ISC',
        meson_version: '>= 0.56.2',
        default_options: [
          'b_ndebug=if-release',
          'buildtype=release',
          'c_std=c99',
        ])

cc = meson.get_compiler('c')

srcdir = meson.current_source_dir()
blddir = meson.current_build_dir()

if cc.get_id() == 'clang' or cc.get_id() == 'gcc'
  cython_c_args = [
    '-Wno-deprecated-declarations',
    '-Wno-unused-variable',
  ]
endif

# Find the tools and dependencies we need at build time

## Python 3
pymod = import('python')
py = pymod.find_installation('python3')
py_dep = py.dependency(required: true)

## Cython
cython = find_program('cython3', required: false)
if not cython.found()
  cython = find_program('cython', required: true)
endif

## The serd C library
serd_dep = dependency('serd-1',
                      version: '>= 1.0.0',
                      fallback: ['serd', 'serd_dep'])

# Generate extension module C source code with cython

cython_args = (['-3', '--fast-fail', '-Wextra'] +
               (get_option('werror') ? ['-Werror'] : []) +
               (get_option('buildtype') == 'debug' ? ['--gdb'] : []))

cython_serd_c = custom_target(
  'serd.c',
  command: [cython] + cython_args + ['@INPUT0@', '-o', '@OUTPUT@'],
  input: files('serd.pyx'),
  output: 'serd.c')

# Compile extension module
py.extension_module('serd',
                    cython_serd_c,
                    c_args: cython_c_args,
                    dependencies: [py_dep, serd_dep],
                    install: true,
                    install_dir: py.get_install_dir())

# Set up an environment for loading the module from the build directory
python_env = environment()
python_env.set('PYTHONPATH', meson.current_build_dir())

# Run API unit tests
test('serd.pyx',
     py,
     args: ['-m', 'unittest', 'discover', '-b', '-v', srcdir],
     env: python_env,
     suite: ['bindings', 'python'])

# Run doctests
if get_option('tests')
  sphinx_build = find_program('sphinx-build')

  # Set up an environment for loading the module from the build directory
  python_env = environment()
  python_env.set('PYTHONPATH', meson.current_build_dir())

  # Test all code snippets in the documentation
  if sphinx_build.found()
    test('doctest',
         sphinx_build,
         args: ['-W', '-b', 'doctest',
                meson.current_source_dir() / 'doc',
                meson.current_build_dir() / 'doc'],
         env: python_env,
         suite: ['bindings', 'python'])
  endif
endif
