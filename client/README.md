# Visual Studio Code Client for Wolfram Language Server

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

- by [kenkangxgwe](mailto:kenkangxgwe@gmail.com) and [hxianglong](https://github.com/huxianglong) 


`Wolfram Language Server (WLServer)` is an implementation of the Microsoft's
[Language Server Protocol
(LSP)](https://microsoft.github.io/language-server-protocol) for Wolfram
Mathematica. This server is implemented in Wolfram Mathematica itself. From now
on, you can use this extension on every [editor that supports
LSP](https://microsoft.github.io/language-server-protocol/implementors/tools/).

This is the client-side code for VS Code, which is based on some slight
modifications of [Microsoft's LSP
example](https://github.com/Microsoft/vscode-extension-samples/tree/master/lsp-sample).
However, you still need to install the server on our github repo, [WLServer](https://github.com/kenkangxgwe/lsp-wl) .

## Installation

**Requirements** <a name="ref1"></a>
- Wolfram Mathematica (11.2 or higher[<sup>1</sup>](#footnote1))  
Which is the only requirement.

To install the server, simply download this git repository.

```
git clone https://github.com/kenkangxgwe/lsp-wl.git
```

To install the client, currently, it is released on [Visual Studio Marketplace:
Wolfram Language Server]().

## Client Settings

Once you have installed this extension, a few settings have to be done manually
in the client side (VS Code in this context) to make things work.

After the extension is launched, in the poped up window, go to **Preference ->
Settings -> User Settings -> Wolfram Language Server**.

- *Port:* The client communicates with the server through port. Feel free to use
  any port that is not occupied by other processes.

- *Theme:* Pictures are used in the poped up window for hovering, which is
transparent. So this setting is to ensure the pictures are actually visible.
This is the only way to display mathematical equations . If you use dark themes,
then choose dark[<sup>2</sup>](#footnote2). <a name="ref2"> </a>

- *WLServer Path:* Where the extension is installed.

- *Wolfram Path:* Set up the location of the Wolfram executable, which is
  *wolfram.exe* for Windows users. For example: C:\Program Files\Wolfram
  Research\Mathematica\11.3\wolfram.exe.




## Features

- *Hover:* Provide definitions for Wolfram functions and system variables, such
  as `String` and `$Path`.

- *Completion:* The completion is triggered by the client automatically.
  Currently, Wolfram functions and system variables would be displayed.

- *Completion Resolve:* Further information would be provided for the items in
  the list.

- *Diagnostics:* Syntax error would be underlined. However, the specific syntax
  error is not supported at the moment.
  
This is an early releasing, so more features are on its way. Syntax highlight is
not supported for there are already good enough highlighters like [Wolfram
Language](https://marketplace.visualstudio.com/items?itemName=flipphillips.wolfram-language).

Here is a full list of [LSP
features](https://microsoft.github.io/language-server-protocol/specification).


## Footnotes

<a name="footnote1"> </a> **[1]** `SocketListen[]` is used for server-client
communication, which is introduced since 11.2. [^](#ref1)


<a name="footnote2"> </a> **[2]** This reminds me of a joke making fun of
Project Managers. A programmer, who cannot put up with endless unreasonable
requests from his project manager, complained about one stupid task from him
online, which is to automatically change the theme of the app with the color of
the cell phone protectors. "How could I know the color of protectors, my
goddness, my manager is so stupid". Someone left his opnion, 'Add an option for
users to choose when launching the app, what is the color of your cell phone
protector. Then change the them accordingly.' [^](#ref2)
