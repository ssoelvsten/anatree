# Anatree

[![LGPL-3.0 License](https://img.shields.io/badge/license-LGPL%203.0-blue.svg)](COPYING.LESSER.md)

The (mixed) Anatree [[Reams2012](#references)] provides a data structure
optimised for solving some subset of *Constraint Satisfaction Problems* (CPSs),
e.g. finding within a dictionary all (sub)anagrams of a given word.

## Implementations

### C++
[![cpp/test](https://github.com/SSoelvsten/anatree/actions/workflows/cpp_test.yml/badge.svg)](https://github.com/SSoelvsten/anatree/actions/workflows/cpp_test.yml)
&nbsp;
[![codecov](https://codecov.io/gh/SSoelvsten/anatree/branch/main/graph/badge.svg?token=j4JA35K7Ec)](https://codecov.io/gh/SSoelvsten/anatree)

- [`cpp/`](./cpp/)

  Subproject with the C++ implementation, including tests and more.

- [`cpp/header-only`](https://github.com/ssoelvsten/anatree/tree/cpp/header-only)

  Branch with the *anatree.h* file and its CMake only for easy inclusion in your project.

**Authors:** Steffan Sølvsten ([@ssoelvsten](github.com/ssoelvsten/)) and
Andreas H. H. Hansen ([@APersonH](https://github.com/APersonH))

- - -

### Haskell

[![hs/test](https://github.com/ssoelvsten/anatree/actions/workflows/hs_test.yml/badge.svg)](https://github.com/ssoelvsten/anatree/actions/workflows/hs_test.yml)

- [`hs/`](./hs/)

  Subproject with the Haskell implementation, including tests and more.

**Authors:** Steffan Sølvsten ([@ssoelvsten](github.com/ssoelvsten/))

## License

The software and documentation files in this repository are provided under the
[*GNU Lesser General Public License* v3.0](/LICENSE.md) .

## References

- [[Reams2012](https://doi.org/10.1145/2133803.2133804)]
  Charles Reams. 2012. *Anatree: A Fast Data Structure for Anagrams*. ACM J.
  Exp. Algorithmics 17, Article 1.1 (2012), 16 pages.

