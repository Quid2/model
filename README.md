
Derivation of data type models from Haskell data types.

### Installation

It is not yet on [hackage](https://hackage.haskell.org/) so to use in your [stack](https://docs.haskellstack.org/en/stable/README/) projects, add a reference to its github location under the 'packages' section:

````
packages:
- location: https://github.com/tittoassini/model/archive/master.zip
````

### Compatibility

Tested with [ghc](https://www.haskell.org/ghc/) 7.10.3 and 8.0.1.

### Known Bugs and Infelicities

* Works incorrectly with data types with more than 5 type variables.
