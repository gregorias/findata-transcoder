# 🛠️ Developer documentation

This is a documentation file for Findata Transcoder’s developers.

## Dev environment setup

This project requires the following tools:

- [Cabal]
- [Commitlint]
- [Lefthook]
- [Hpack]

Install lefthook:

```shell
lefthook install
```

[Cabal]: https://www.haskell.org/cabal/
[Commitlint]: https://github.com/conventional-changelog/commitlint
[Lefthook]: https://github.com/evilmartians/lefthook
[Hpack]: https://github.com/sol/hpack

## Operations

### Dependency update

To update used dependencies:

1. Update the Stackage version in `cabal.project`.
  You can check the current Stackage at stackage.org.
2. Update Cabal’s local package metadata with `cabal update`.

### Release

1. Update the version in `package.yaml`.
2. Run `hpack`.
3. Commit the changes `git commit -am 'chore: bump version'`.

## ARDs

### Invidual items in Coop receipts

I keep indivudal items in Coop transactions for auditability. I want to be
able to verify that the category and debt assignments are correct.
