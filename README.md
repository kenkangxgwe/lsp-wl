# Wolfram Language Server

[![Develop with: Wolfram Language](https://img.shields.io/badge/Develop%20with-Wolfram%20Language-%23d81013.svg)](http://www.wolfram.com/language/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CI](https://github.com/kenkangxgwe/lsp-wl/workflows/CI/badge.svg?branch=develop)](https://github.com/kenkangxgwe/lsp-wl/actions?query=workflow%3ACI+branch%3Adevelop)
[![GitHub release (latest by date)](https://img.shields.io/github/v/release/kenkangxgwe/lsp-wl)](https://github.com/kenkangxgwe/lsp-wl/releases)
[![Open in Visual Studio Code](https://open.vscode.dev/badges/open-in-vscode.svg)](https://open.vscode.dev/kenkangxgwe/lsp-wl)

![WolframLanguageServerLogo](images/wolfram-language-server-logo-clipped.png)

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Wolfram Language Server](#wolfram-language-server)
    - [Installation](#installation)
    - [Run the Server](#run-the-server)
    - [Language Server Features](#language-server-features)
        - [DocumentSymbol](#documentsymbol)
        - [Hover](#hover)
        - [Completion](#completion)
        - [Diagnostics](#diagnostics)
        - [Definition / References / Document Highlight](#definition--references--document-highlight)
        - [Code Action](#code-action)
        - [Document Color / Color Presentation](#document-color--color-presentation)
        - [Notes](#notes)
    - [Debug Adapter Features](#debug-adapter-features)
        - [Evaluate](#evaluate)
        - [Variables](#variables)
        - [Notes](#notes-1)
    - [Contribute](#contribute)
        - [Design Principles](#design-principles)
        - [Todo list](#todo-list)
    - [Donations :dollar:](#donations-dollar)
    - [Footnotes](#footnotes)

<!-- markdown-toc end -->

**Wolfram Language Server (WLServer)** is an implementation of the Microsoft's
[Language Server Protocol
(LSP)](https://microsoft.github.io/language-server-protocol) for [Wolfram
Language](http://www.wolfram.com/language). This server is implemented in
Wolfram Language itself.

Our current goal is to provide the experience as good as the Mathematica
FrontEnd with addition power from the editor.

We have provided the client-side code for VS Code
[here](https://github.com/kenkangxgwe/vscode-lsp-wl), which is based on some
slight modifications of [Microsoft's LSP
example](https://github.com/Microsoft/vscode-extension-samples/tree/master/lsp-sample).
If you are using other editors supporting LSP, some adaptation to the client
would certainly work too.

## Installation

0. [Wolfram Mathematica](http://www.wolfram.com/mathematica/) (11.2 or higher<a
    name="ref1"></a>[<sup>1</sup>](#footnote1)) or [Wolfram
    Engine](https://www.wolfram.com/engine/) (12.0 or higher).

1. Use git to clone this repository.  
    ``` sh
    git clone https://github.com/kenkangxgwe/lsp-wl.git
    ```

2. Install the dependent paclets with the latest versions from the Wolfram kernel / Mathematica.
(_This will cost some time for the first time_) :  
    ``` mathematica
    PacletInstall["CodeParser"]
    PacletInstall["CodeInspector"]
    PacletInstall["ZeroMQLink"] (* 1.2.6+ *)
    ```

3. Install the client. Currently, we provide the VS Code extension on [Visual
Studio Marketplace: Wolfram Language
Server](https://marketplace.visualstudio.com/items?itemName=lsp-wl.lsp-wl-client)
For other editors, please refer to the
[wiki](https://github.com/kenkangxgwe/lsp-wl/wiki).

## Run the Server

This section is usually handled by the client (editor). If you want to introduce
lsp-wl to a new client, please continue reading.

Clients can start the server by running the `init.wls` file from Wolfram
Mathematica executables

``` sh
wolfram -script /path/to/lsp-wl/init.wls [args]
# or
wolframscript -f /path/to/lsp-wl/init.wls [args]
```

The possible arguments for the server are

- `--help, -h` to print help information.
- `--socket=port` to assign the port to which the server will connect. (Default:
`6536`)
- `--tcp-server=port` to assign the port at which the server will start.
(Default: `6536`)
- `--pipe=pipename` to specify the pipe name for the server to connect to.
- `--log=level, -l level` to specify the logging level of the server. (Levels:
  `error`, `warn`, `info`, `debug`. Default: `info`)
- `--test, -t` to run the unit test for the server.

If you want to run the server from Mathematica you can use the following code.

``` mathematica
initfile = "/path/to/lsp-wl/init.wls";
args = {};
Block[{$ScriptCommandLine = Prepend[args, initfile], Quit = Function[{}, Throw[Null]]},
    Catch[<< (initfile)]
];
```

To use the debugger adapter (current in a very early stage), you need to include
`debuggerPort` in the `initializationOptions`. And attach the frontend to that
port. For VS Code, it is automatically done by the extension.

## Language Server Features

### DocumentSymbol

You may typeset your package in the same way that Mathematica FrontEnd handles
it: a cell begins with two lines of comments, where the first line specifies the
style of the cell and the second line names it. So you may get the outline
structure of the file.

``` mathematica
(* ::Title:: *)
(*Title of the file*)

(* ::Section:: *)
(*Section 1*)
```

![documentSymbol](images/documentSymbol.png)

### Hover

Provide documentations for functions and variables from the ``System` ``
context, such as `String` and `$Path`, the `MessageName` and the special
numerical literals with `^^` or `*^`.

![hover](images/hover.png)

### Completion

The completion is shown by the client automatically. Functions and system
variables from the ``System` `` context that matches the input would be
displayed. To enter an unicode character, you may use the leader key
<kbd>\\</kbd> followed by the alias just like <kbd>esc</kbd> in Wolfram
FrondEnd. E.g., `<esc>a` in the FrontEnd is input as `\a` in the editor and the
server will show you the available completions.

![completion-unicode](images/completion_alias.png)

**Completion Resolve:** Further information (such as documentation) would be
provided for the items in the list.

![completion](images/completion.png)

### Diagnostics

Syntax error would be underlined. This feature is powered by
[CodeParser](https://github.com/WolframResearch/codeparser) and
[CodeInspector](https://github.com/WolframResearch/codeinspector) paclets, thank
you [@bostick](https://github.com/bostick).

![diagnostics](images/diagnostics.png)

### Definition / References / Document Highlight

It is now able to look up the definition and references of a local variable in a
scope such as `Module` or pattern rules.

![references](images/references.png)

### Code Action

Code action is now able to,

- Open the documentation of system symbols in Mathematica (Not available for
  Wolfram Engine).  
  ![documentation](images/codeActionSymbolDocumentation.png)

- Evaluate the selected code if debugger is running. See [Evaluate](#evaluate).

### Document Color / Color Presentation

Both Named Colors and
Color Models with constant parameters are able to show and modify.  
(_Experimental, may have performance issues._)

![documentColor](images/documentColor.png)

### Notes

The project is under development, so more features are on the way. Notice that,
**syntax highlight** will not be provided as long as it is excluded from the
LSP, but I believe there are already plenty of good Mathematica highlighters
available for your editor.

Here is a full list of [LSP
features](https://microsoft.github.io/language-server-protocol/specification).

## Debug Adapter Features

### Evaluate

Code evaluation can be run from the code action of the selection or code lens
below each section title. The results are usually shown in the debug console on
the editor side.

![evaluate-code-action](images/evaluate_code_action.png)

Expressions can also be directly input from the debug console.

![evaluate-debug-console](images/evaluate_debug_console.png)

### Variables

After evaluation, the symbol values can be retrieved from the editor. This
includes the own values of variables and the down/up/sub values of functions
defined.

![variables](images/variables.png)

The behavior of the variables mimics the workspace in MATLAB, so all the symbols
defined in the debug console as well as evaluated from the file will be
recorded. This also includes contexts other than ``Global` ``. The editor can
also watch on a specific expression after each evaluation if applicable.

### Notes

Here is a full list of [DAP
features](https://microsoft.github.io/debug-adapter-protocol/specification).

## Contribute

### Design Principles

1. The files are located according to its context name. The `init.wls` is the
   entry script that parses the commandline arguments, loads packages and starts
   the server. This is intended to be different from a paclet, since it is not
   intended to be normally used inside Mathematica / Wolfram Kernel.

2. We implemented a *stateless* (by passing the state around :)) server in
   ``WolframLanguageServer`Server` `` that will parse and handle messages.

3. ``DataType` `` is a simple type system extracted as an independent package in
  the [Matypetica](https://github.com/kenkangxgwe/Matypetica) library that
  supports pattern test on every field of a data structure. The operations on
  the data objects are designed to be immutable.

4. ``WolframLanguageServer`Test`* `` stores the unit tests for some of the
   functions which are integrated into GitHub Action.

### Todo list

It will be nice if you want to make a contribution to the following topic.

* Our server-client communication only supports Socket with TCP protocol. We
  tried to use ZMQ_Stream protocol and `SocketListen[]` to enable concurrency,
  but it fails to send responses back to the client.

* It will be helpful to implement a stdio channel, ~so that the Mathematica
  earlier than 11.2 will also be supported,~ but it is really hard to expose the
  `stdin` channel. Hope this will be dicussed in future release of Wolfram
  kernel.

* More editor clients are needed. You can feel free to open a repository and
  create a pull request to add the clients in README.md once your client is
  released.

* Thanks to [CodeParser](https://github.com/WolframResearch/codeparser) and
  [CodeInspector](https://github.com/WolframResearch/codeinspector) paclets, we
  are able to parse the code and extract useful information. Please consider to
  contribute to them as well.
  
  If you have ideas about how to use these fantastic language tools to help the
  language server with more features, please send us issues or pull requests.

If you want to help us with this project, feel free to fork and create a pull
request. Do not forget to add unit tests if possible.

## Donations :dollar:

If you really like this project, please donate to us! **$5 (or equivalently
￥35)**. A cup of coffee :coffee: would certainly brighten our day! Your
donation would be the motivation for us to move forward, thanks in advance
:smile:.

- Paypal: qwe95123@126.com
- Alipay (With QRCode): 13916018006  
![Alipay QRCode](images/alipay.jpg)

## Footnotes

<a name="footnote1"> </a> **[1]** `SocketListen[]` is used for server-client
communication, which is introduced since 11.2. [^](#ref1)
