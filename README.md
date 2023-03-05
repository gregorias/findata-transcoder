# findata-transcoder

This tool that parses and transforms CSV and other text financial statements
into a [ledger-like](https://www.ledger-cli.org/) file.

I use it in my automation system that downloads financial statements and puts
their data into one big ledger of my personal finances. There are other tools
I've written that handle fetching those statements, preprocessing them (e.g.,
`pdftotext` on PDF files), and orchestrating.

## Installation

Install the binary with

```shell
stack install
```

### \[Optional\] Shell completion

To provide [fish] shell completion run

```sh
findata-transcoder --fish-completion-script (which findata-transcoder) \
  > ~/.config/fish/completions/findata-transcoder.fish
```

For other shells, check out [optparse-applicative's
documentation](https://hackage.haskell.org/package/optparse-applicative#:~:text=revoir%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20%20Say%20goodbye-,Bash%2C%20Zsh%2C%20and%20Fish%20Completions,-optparse%2Dapplicative%20has)
and adapt accordingly. findata-transcoder uses
[optparse-applicative] as the CLI framework.

[fish]: https://fishshell.com
[optparse-applicative]: https://github.com/pcapriotti/optparse-applicative
