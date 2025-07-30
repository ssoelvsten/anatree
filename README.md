# Anatree

[![LGPL-3.0 License](https://img.shields.io/badge/license-LGPL%203.0-blue.svg)](COPYING.LESSER.md)

The (mixed) Anatree [[Reams2012](#references)] provides a data structure
optimised for solving some subset of *Constraint Satisfaction Problems* (CPSs),
e.g. finding within a dictionary all (sub)anagrams of a given word.

## Implementations

### C++

[![cpp/test](https://github.com/SSoelvsten/anatree/actions/workflows/cpp_test.yml/badge.svg)](https://github.com/SSoelvsten/anatree/actions/workflows/cpp__test.yml)
&nbsp;
[![codecov](https://codecov.io/gh/SSoelvsten/anatree/branch/main/graph/badge.svg?token=j4JA35K7Ec)](https://codecov.io/gh/SSoelvsten/anatree)

- [`cpp/main`](https://github.com/ssoelvsten/anatree/tree/cpp/main)

  Development branch for the C++ implementation, including tests and more.

- [`cpp/header-only`](https://github.com/ssoelvsten/anatree/tree/cpp/header-only)

  This only includes the *anatree.h* file and its CMake for inclusion in your
  project as a submodule.

### Haskell

[![hs/test](https://github.com/ssoelvsten/anatree/actions/workflows/hs_test.yml/badge.svg)](https://github.com/ssoelvsten/anatree/actions/workflows/hs_test.yml)

- [`hs/main`](https://github.com/ssoelvsten/anatree/tree/hs/main)

  Development branch for the Haskell implementation, including tests and more.

## License

The software and documentation files in this repository are provided under the
[*GNU Lesser General Public License* v3.0](/LICENSE.md) .

## References

- [[Reams2012](https://doi.org/10.1145/2133803.2133804)]
  Charles Reams. 2012. *Anatree: A Fast Data Structure for Anagrams*. ACM J.
  Exp. Algorithmics 17, Article 1.1 (2012), 16 pages.

