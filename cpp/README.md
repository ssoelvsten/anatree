# Anatree (C++)

[![LGPL-3.0 License](https://img.shields.io/badge/license-LGPL%203.0-blue.svg)](../COPYING.LESSER.md)
&nbsp;
[![tests](https://github.com/SSoelvsten/anatree/actions/workflows/cpp_test.yml/badge.svg)](https://github.com/SSoelvsten/anatree/actions/workflows/cpp_test.yml)
&nbsp;
[![codecov](https://codecov.io/gh/SSoelvsten/anatree/branch/main/graph/badge.svg?token=j4JA35K7Ec)](https://codecov.io/gh/SSoelvsten/anatree)

## Usage

The data structure is implemented in a single header-only file, *src/anatree.h*
that you can link up to directly.

For ease of use, we provide the following *make* targets.

| Make Target | Description                           |
|-------------|---------------------------------------|
| `help`      | Prints out a list of all targets      |
| `clean`     | Removes all build files               |
| `install`   | Installs Anatree                      |
| `uninstall` | Removes the installed files once more |
| `tests`     | Builds and runs the unit tests        |
| `docs`      | Builds the Doxygen Documentation      |

### CMake Dependency

The entire project is set up to be built with CMake. Indeed, we recommend that
you include this *Anatree* repository as a submodule of your own project as
follows

```bash
git submodule add -b cpp/header-only https://github.com/ssoelvsten/anatree external/anatree
git submodule update --init
```

Then within your own project you can just do

```cmake
add_subdirectory(external/anatree)

add_executable(<target> <source>)
target_link_libraries(<target> anatree)
```

### Installation

The Anatree library can also be installed to */usr/local/include/* (or your
machine's equivalent) by use of the `install` *make* target or with the CMake
target of the same name.

## Unit Tests

The anatree is tested with the [Bandit](https://github.com/banditcpp/bandit)
unit testing framework. If you did not already clone this repository recursively
to include submodules, please do

```bash
git submodule update --init --recursive
```

You can run the unit tests using the `tests` *make* target mentioned above or
with the following commands.

```bash
mkdir -p build/
cd build/
cmake -D CMAKE_BUILD_TYPE=Debug ..
make anatree_test
./tests/anatree_test
cd ..
```

## License

The software and documentation files in this repository are provided under the
[*GNU Lesser General Public License* v3.0](../COPYING.LESSER.md) .
