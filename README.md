# awk-ts-mode

[![CI](https://github.com/konomanoasa/awk-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/awk-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
GNU Awk 5.4.1 and POSIX.1-2024 AWK.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/awk-ts-mode")
```

## Automatic Activation

Enabled for `.awk` files and scripts with an `awk` or `gawk` shebang.
GNU Awk is the default. Select POSIX Awk through EditorConfig:

```ini
[*.awk]
posix = true
```

`posix = false` selects GNU Awk. The shebang selects the mode; the
EditorConfig property selects the language when the mode starts.

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
| 2     | Built-in functions, function definitions and calls, keywords, directives, namespaces, parameters, strings, and variable names and uses |
| 3     | Numbers and escapes outside static EREs                                                                        |
| 4     | Operators, punctuation, brackets, and static EREs                                                              |

## Grammar

- [tree-sitter-gawk](https://github.com/konomanoasa/tree-sitter-gawk)
- [tree-sitter-posix-awk](https://github.com/konomanoasa/tree-sitter-posix-awk)

## License

[MIT](LICENSE)
