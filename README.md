[![Build Status](https://antonstefanov.visualstudio.com/orwell/_apis/build/status/antonstefanov.orwell?branchName=master)](https://antonstefanov.visualstudio.com/orwell/_build/latest?definitionId=3&branchName=master)

## Install

```
npm -g install orwell-cli
```

to add shell autocompletions (follow instructions):

```
orwell --rarg-suggestions-script
```

## Owners

`owners [CMD]` is printing a human-readable list of owners in the terminal.
You can use `--copy` (or `-c`) to copy the output in `Markdown` format that you can paste in PRs.

For more up-to-date information use `--help`

```
orwell owners --help
```

### Sample use

To view changed files owners of the current branch, compared to `origin/master`:

```
orwell owners changed --copy
```

To view changed files owners between 2 branches/commits (useful when checking remote PR chains):

```
orwell owners changed origin/base-branch origin/checked-branch
```

You can pipe a list of files to be checked with `orwell owners files`:

```
find . -path node_modules -prune -o -type f -name '*.jpg' | orwell owners files
```

## Git

`git [CMD]` is are helpful git aliases for diffs, that exclude `.xlf` files.

For example to copy a diff for pasting as a GitHub comment:

```
orwell git diff --filter M --copy
```

Or to see the impacted files

```
orwell git diff --display status
```

It supports the same format for base-branch checked-branch as `orwell owners changed`, for example:

```
orwell git diff origin/base-branch origin/checked-branch --display status
```
