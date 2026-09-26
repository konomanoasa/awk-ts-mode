# awk-ts-mode

[![CI](https://github.com/konomanoasa/awk-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/awk-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
POSIX.1-2024 AWK.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/awk-ts-mode")
```

## Automatic Activation

Enabled for `.awk` files and scripts with an `awk` shebang.

## Features

- Comment Commands
- Electric Pair
- Font Lock
- Imenu
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock                                                                                                      |
| ----- | -------------------------------------------------------------------------------------------------------------- |
| 1     | Comments                                                                                                       |
| 2     | Built-in functions, function definitions and calls, keywords, parameters, strings, and variable names and uses |
| 3     | Numbers and escapes outside static EREs                                                                        |
| 4     | Operators, punctuation, brackets, and static EREs                                                              |

## Grammar

[tree-sitter-posix-awk](https://github.com/konomanoasa/tree-sitter-posix-awk)

## License

[MIT](LICENSE)
