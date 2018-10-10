# Implementation of `polyparse`


## `CHANGELOG`

* Changes in `polyparse-1.20` include:

    * add `MonadFail` instances;
    * add non-`return` `pure` definitions;
    * remove `return` and `fail` methods from `Monad`.

These changes are **all** `CPP`-guarded (see below).


## Future Compatibility

`polyparse-1.20` updates the `Monad` instances for its (several) `Parser` types. Why? For compatibility with these proposals:

* <https://ghc.haskell.org/trac/ghc/wiki/Proposal/MonadOfNoReturn MonadOfNoReturn>
* <https://ghc.haskell.org/trac/ghc/wiki/Proposal/MonadFail MonadFail>


### Future Compatibility: `MonadOfNoReturn`

For `MonadOfNoReturn`, warnings (and forwards-compatibility, see below) exist from `ghc-7.10`. When these warnings become errors is **unknown**, because this transition is delayed by several incompatible packages on `Hackage`.

The `MonadOfNoReturn` proposal recommends the following (paraphrasing):

> Forward-compatible `Monad` instances (i.e. those which omit `return`) are valid since `ghc-7.10` (/ `base-4.8`).

(introduced in `ghc-7.10` via the `-Wnoncanonical-monad-instances` flag).


### Future Compatibility: `MonadFail`

For `MonadFail`, warnings (introduced in `ghc-8.0` via the `-Wnoncanonical-monadfail-instances` flag) become **errors** in `ghc-8.6`.


## Backwards-Compatibility

`polyparse-1.20` wraps the following changes (see above) with `CPP`:

* extra `import`s,
* new `class`es,
* omitted (`default`) methods.

Then, `polyparse-1.20` guards the `CPP` itself via:

* `#if`s (obviously), like `MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)` and `MIN_VERSION_base(4,9,0)`;
* and these `#if`s are beneath `#ifdefs` on `CPP` symbols, defined by newer version of `ghc` and/or `cabal`.

These guards, along with the already existing manually-written instances, provide backwards-compatibility for:

* older GHC versions,
* older Cabal versions,
* non-GHC compilers,
* and any haskell compiler supporting `-XCPP`.


