# Visual Studio Code Client for Wolfram Language Server

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> by [kenkangxgwe](mailto:kenkangxgwe@gmail.com) and [hxianglong](https://github.com/huxianglong)

**Wolfram Language Server (WLServer)** is an implementation of the Microsoft's
[Language Server Protocol
(LSP)](https://microsoft.github.io/language-server-protocol) for [Wolfram
Language](http://www.wolfram.com/language). This server is
implemented in Wolfram Language itself.

This is the client-side code for VS Code, which is based on some slight
modifications of [Microsoft's LSP
example](https://github.com/Microsoft/vscode-extension-samples/tree/master/lsp-sample).
However, you still need to manually install the
[server](https://github.com/kenkangxgwe/lsp-wl) .

## Installation

0. [Wolfram Mathematica](http://www.wolfram.com/mathematica/) (11.2 or higher<a
   name="ref1"></a>[<sup>1</sup>](#footnote1)).

1. Download the [server](https://github.com/kenkangxgwe/lsp-wl) from its
   repository.

  ```
  git clone https://github.com/kenkangxgwe/lsp-wl.git
  ```

2. Install the client extenstion from [Visual Studio Marketplace: Wolfram
Language Server](https://marketplace.visualstudio.com/items?itemName=lsp-wl.lsp-wl-client).

## Client Settings

Once you have installed the extension, a few settings have to be done manually
in the client side to make things work.

After the extension is launched, go to **Preference -> Settings -> User Settings
-> Extensions -> Wolfram Language Server**, and configure the following options:

- *Port:* The client communicates with the server through port. Feel free to use
  any port that is not occupied by other processes.

- *Theme:* For better typesetting of documentation, SVG images are used in the
popup for hovering. Since the background is transparent, this setting is to
ensure the text in the images are actually visible. If you use dark themes, then
choose `dark` <a name="ref2"> </a>[<sup>2</sup>](#footnote2).

- *WLServer Path:* The path to the server repository.

- *Wolfram Path:* The path of the `Wolfram` executable. **(NOT `Mathematica` or `WolframKernel`)**  
  For **Windows** users, the default path is `C:\Program Files\Wolfram
  Research\Mathematica\11.*\wolfram.exe`.  
  For **MaxOS** users, the default path is
  `/Applications/Mathematica.app/Contents/wolfram`.  
  For **Linux** users, the default path is
  `/usr/local/Wolfram/Mathematica/11.*/wolfram`.

Restart VS Code to take effect.

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

## Footnotes

<a name="footnote1"> </a> **[1]** `SocketListen[]` is used for server-client
communication, which is introduced since 11.2. We plan to support stdio for
better compatibility [^](#ref1)

<a name="footnote2"> </a> **[2]** This reminds me of a joke making fun of
Project Managers. A programmer, who cannot put up with endless unreasonable
requests from his project manager, complained about one stupid task from him
online, which is to automatically change the theme of the app with the color of
the cell phone protectors. "How could I know the color of protectors, my
goddness, my manager is so stupid". Someone left his opinion, 'Add an option for
users to choose when launching the app, what is the color of your cell phone
protector. Then change the them accordingly.' [^](#ref2)
