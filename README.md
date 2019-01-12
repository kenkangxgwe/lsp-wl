# Wolfram Language Server

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

![WolframLanguageServerLogo](https://raw.githubusercontent.com/kenkangxgwe/lsp-wl/develop/images/wolfram-language-server-logo-clipped.png)
> by [kenkangxgwe](mailto:kenkangxgwe@gmail.com) and [hxianglong](https://github.com/huxianglong) 

**Wolfram Language Server (WLServer)** is an implementation of the Microsoft's
[Language Server Protocol
(LSP)](https://microsoft.github.io/language-server-protocol) for [Wolfram
Language](http://www.wolfram.com/language). This server is
implemented in Wolfram Language itself.

Our current goal is to provide the experience as good as the Mathematica FrontEnd 
with addition power from the editor.

We have provided the client-side code for VS Code in this repo, which is based on some slight
modifications of [Microsoft's LSP
example](https://github.com/Microsoft/vscode-extension-samples/tree/master/lsp-sample).
If you are using other tools supporting LSP, some slight modifications to the
client would certainly work too.

## Installation

0. [Wolfram Mathematica](http://www.wolfram.com/mathematica/) (11.2 or
    higher<a name="ref1"></a>[<sup>1</sup>](#footnote1)).

1. Download the [server](https://github.com/kenkangxgwe/lsp-wl) from its
   repository.

  ```
  git clone https://github.com/kenkangxgwe/lsp-wl.git
  ```

2. Install the client. Currently, we provide the VS Code extension on [Visual
Studio Marketplace: Wolfram Language Server]()

## Run the Server

Clients can start the server by running the `init.wls` file from Wolfram
Mathematica executables

``` sh
wolfram -script /path/to/lsp-wl/init.wls [args]
# or
wolframscript -f /path/to/lsp-wl/init.wls [args]
```

The posible arguments for the server are

* `--help, -h` to print help information.
* `--socket=port` to assign the port that the server connect to. (Default:
`6536`)  
Socket is the only channel that we currently support.
* `--theme=dark/light` to specify the documentation text color since it is rendered
  to SVG image for better typesetting. (Default: `dark`)
* `--log=level, -l level` to specify the logging level of the server.
  (Levels: `error`, `warn`, `info`, `debug`. Default: `info`)
* `--test, -t` to run the unit test for the server.


## Features

- *Hover:* Provide definitions for Wolfram functions and system variables, such
  as `String` and `$Path`.

- *Completion:* The completion is triggered by the client automatically.
  Currently, Wolfram functions and system variables would be displayed.

- *Completion Resolve:* Further information would be provided for the items in
  the list.

- *Diagnostics:* Syntax error would be underlined. However, the specific syntax
  error is not supported at the moment.
  
This is an early release, so more features are on the way. Syntax highlight is
NOT supported according to the design of LSP, but there are already some good
enough extensions like [Wolfram
Language](https://marketplace.visualstudio.com/items?itemName=flipphillips.wolfram-language).

Here is a full list of [LSP features](https://microsoft.github.io/language-server-protocol/specification).

## Contributions

### Design Principle

1. The files are located according to its context name. The `init.wls` is the
   entry script that parses the commandline arguments, loads packages
   and starts the server.

2. We implemented an stateless server in ``WolframLanguageServer`Server` `` that
   will parse and handle the messages.

3. ``WolframLanguageServer`DataType` `` is a simple type system
   that supports pattern test on every field of a class. The operations on the
   objects are designed to be immutable.
   
4. ``WolframLanguageServer`Test`* `` stores the unit tests for some of
   the packages.
   
### Todo list

It will be nice if you want to make a contribution to the following topic. 

* Our server-client communication only supports Socket with TCP protocol. We
  tried to use ZMQ_Stream protocol and `SocketListen[]` to enable concurrency,
  but it fails to send responses back to the client.
  
* It will be helpful to implement a stdio channel, so that the Mathematica
  earlier than 11.2 will also be supported.
  
* More editor clients are needed. You can feel free to open a repository and
  create a pull request to add the clients in README.md once your client is released.
  
* A scanner/parser might be needed to extract tokens for future usage. We have
  investigated serveral implementations in other languages, but we are still
  prefer a wolfram language scanner/parser written in wolfram language and can
  be easily integrated into this project.

If you want to help us with this project, feel free to fork and create a pull
request. Do not forget to add unit tests if possible.

## Donations :dollar:

If you really like this project, please donate to us! **$5 (or equivalently
￥35)**. A cup of coffee :coffee: would certainly
brighten our day! Your donation would be the motivation for us to move forward,
thanks in advance :smile:.

- Paypal: qwe95123@126.com
- Alipay (With QRCode): 13916018006  
![Alipay
QRCode](https://raw.githubusercontent.com/kenkangxgwe/lsp-wl/develop/images/alipay.jpg)

## Footnotes

<a name="footnote1"> </a> **[1]** `SocketListen[]` is used for server-client
communication, which is introduced since 11.2. We plan to support stdio for
better compatibility [^](#ref1)

<a name="footnote2"> </a> **[2]** This reminds me of a joke making fun of
Project Managers. A programmer, who cannot put up with endless unreasonable
requests from his project manager, complained about one stupid task from him
online, which is to automatically change the theme of the app with the conion, 'Add an option for
users to choose when launching the app, what is the color of your cell phone
protector. Then change the them accordingly.' [^](#ref2)
