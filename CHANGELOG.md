# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.2] - 2020-08-01 🐱‍🏍

### Added

- SignatureHelp is available for functions when `[` and `,` is input.

### Changed

- Change the dependencies from `AST` and `Lint` to `CodeParser` and
`CodeInspector`.
- The alias completion has been improved by reducing the number of
`completionItem`s, and returning `\`-prefixed aliases when triggered twice.

### Fixed

- `triggerCharacters` should be a list (thanks to
[@dalanicolai](https://github.com/dalanicolai)).
- The `duplicate requests`
error will not popup in Output window in VSCode (reported by
[@GiovanniBordiga](https://github.com/GiovanniBordiga)).
- MessageName should be shown when hovered

## [0.2.1] - 2020-01-24 🏮🐀

### Added

- Definition / References are available to show in scopes / the whole file.
- DocumentHighlight is available to show the scoped variables.
- DocumentColor / ColorRepresentation are available to show the color names and models.

### Changed

- The Top-level function / list declaration is visible in documentSymbol.
- Attributes and Option are showed in hover messages.
- Operators will also trigger the hover messages.

### Fixed

- Mathematica before version 11.3 are correctly using the StringRepeat.

### Known issues

- The hovering could be triggered, when cursor is in side a parent syntax
  node even it is not on the operator/function head. Not sure about the UX.
- Color-related requests are slowed down intentionally, since it is in low
  use frequency comparing to messages like didChange and completion.
- Even if some requests are prioritized to improve the experience, sometimes
  responds could still be slower if the document is changing too frequent.

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
