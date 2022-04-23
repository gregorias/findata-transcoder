# findata-transcoder

This tool that parses and transforms CSV and text financial statements into a
[ledger-like](https://www.ledger-cli.org/) file.

The background for this tool is that I use it in my automation system that
downloads financial statements and puts their data into one big ledger of my
personal finances. There are other tools I've written that handle fetching
those statements, preprocessing them (e.g., `pdftotext` on PDF files), and
orchestrating.

## Development

This section is intended for developrs.

### Dev environment setup

#### Install Git hook tools

1. Set up yarn packages:

   ```shell
   yarn install
   ```

2. Install lefthook:

   ```shell
   lefthook install
   ```
