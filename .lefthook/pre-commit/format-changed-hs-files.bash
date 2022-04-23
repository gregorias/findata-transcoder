#!/usr/bin/env bash
# Git pre-commit hook

function format-file {
  stack exec hlint "$1" -- --refactor --refactor-options="-i"
  fourmolu -o "-XTypeApplications" -i "$1"
}
export -f format-file

function format-files {
  ################################################################################
  # Check staged Haskell files for formatting issues with:
  #
  # * hlint
  # * fourmolu
  ################################################################################

  # Find all staged Haskell files, and exit early if there aren't any.
  HASKELL_FILES=()
  while IFS=$'\n' read -r line; do HASKELL_FILES+=("$line"); done \
    < <(git diff --name-only --cached --diff-filter=AM | grep --color=never '.hs$')
  if [ ${#HASKELL_FILES[@]} -eq 0 ]; then
    return 0
  fi

  if ! stack exec hlint -- --version 2>/dev/null 2>&1; then
    echo 'hlint not on path; can not format. Please install hlint:'
    echo '    stack install hlint'
    exit 2
  fi


  if ! fourmolu --version 2>/dev/null 2>&1; then
    echo 'fourmolu not on path; can not format. Please install fourmolu.'
    exit 2
  fi


  # Check for unstaged changes to files in the index.
  CHANGED_FILES=()
  while IFS=$'\n' read -r line; do CHANGED_FILES+=("$line"); done \
    < <(git diff --name-only "${HASKELL_FILES[@]}")
  if [ ${#CHANGED_FILES[@]} -gt 0 ]; then
    echo 'You have unstaged changes to some files in your commit; skipping '
    echo 'auto-format. Please stage, stash, or revert these changes. You may '
    echo 'find `git stash -k` helpful here.'
    echo 'Files with unstaged changes:' "${CHANGED_FILES[@]}"
    exit 1
  fi

  # Format all staged files, then exit with an error code if any have uncommitted
  # changes.
  echo 'Formatting staged Haskell files . . .'

  printf "%s\0" "${HASKELL_FILES[@]}" \
    | xargs -0 -n1 -P8 -I {} bash -c 'format-file "{}"' 2>/dev/null

  if [[ $? -ne 0 ]]; then
    echo 'File formatting has failed. Please debug.'
    exit 1
  fi

  CHANGED_FILES=()
  while IFS=$'\n' read -r line; do CHANGED_FILES+=("$line"); done \
    < <(git diff --name-only "${HASKELL_FILES[@]}")
  if [ ${#CHANGED_FILES[@]} -gt 0 ]; then
    echo 'Reformatted staged files. Please review and stage the changes.'
    echo 'Files updated: ' "${CHANGED_FILES[@]}"
    exit 1
  fi
}

format-files
