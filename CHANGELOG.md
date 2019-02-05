# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
