# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.0] - 2019-07-23 🏖️

### Added

- The document structure is provided in DocumentSymbol. (Thanks to Brenton's `AST` package)
- Hovering for `MessageName` and numeral literals.
- Completion for both aliases and long names of the Unicode characters (with leader key <kbd>\\</kbd>).
- Auto check for upgrades and dependencies.

### Changed

- Using Brenton's `Lint` package to diagnose the document.
- Documentations now have a better format with code block (in hovers and completions).
- Diagnostics are published schedulely.

### Fixed

- High CPU usage when reading message via sockets under Linux.  
  (Reported by [@kc9jud](https://github.com/kc9jud) and solution suggested by
  [@megatron0000](https://github.com/megatron0000))

### Known Issues

- Some of the documentations are not transcribed correctly into Markdown format, see issues. 

## [0.1.2] - 2019-05-15

### Added

- Named pipe support for Windows

### Fixed

- Avoid early evaluation that slow down the initialization

- Markdown conversion for special characters

### Removed

- SVG image for document information

## [0.1.1] - 2019-02-05 🧧

### Added

- Markdown text-based document information

### Fixed

- Tail-recursive message handler with no limits

### Deprecated

- SVG image for document information

## [0.1.0] - 2019-01-12

### Added

- A state-less server to handle messages, via TCP socket protocol.

- Support for the following language features:

  - Hover: Provide definitions for variables with usage message.

  - Completion: for wolfram system names.

  - Completion Resolve: Further information would be provided for the items in
    the list.

  - Diagnostics: Syntax error would be underlined.

- Support for exporting temporary SVG images for the information of the variables.

- A type system with pattern test.

- A unit test framework.
