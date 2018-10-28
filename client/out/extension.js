"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
const net = require("net");
const vscode_jsonrpc_1 = require("vscode-jsonrpc");
const vscode_1 = require("vscode");
const vscode_languageclient_1 = require("vscode-languageclient");
function activate(context) {
    // clangd command
    // const config = workspace.getConfiguration('WolframLanguageServer');
    // // The server is a started as a separate app and listens on port 5007
    // let wolfram: Executable = {
    //     command: config.get<string>('wolfram.kernelPath')
    //     // args: config.get<string>('arguments'),
    // };
    // let serverOptions: ServerOptions = wolfram;
    let connectionInfo = {
        port: 6009
    };
    let serverOptions = () => {
        // connect to language server via socket
        let socket = net.connect(connectionInfo);
        let result = {
            writer: socket,
            reader: socket
        };
        return Promise.resolve(result);
    };
    let clientOptions = {
        documentSelector: ["wolfram"],
        synchronize: {
            fileEvents: vscode_1.workspace.createFileSystemWatcher("**/*.*")
        }
    };
    // create the language client and start the client.
    let client = new vscode_languageclient_1.LanguageClient("WolframLanguageServer", "Wolfram Language Server", serverOptions, clientOptions);
    // var disposable2:Disposable =commands.registerCommand("mydsl.a.proxy", async () => {
    //     let activeEditor:TextEditor = window.activeTextEditor;
    //     if (!activeEditor || !activeEditor.document || activeEditor.document.languageId !== "mydsl") {
    //         return;
    //     }
    //     if (activeEditor.document.uri instanceof Uri) {
    //         commands.executeCommand("mydsl.a", activeEditor.document.uri.toString());
    //     }
    // });
    // context.subscriptions.push(disposable2);
    // enable tracing (.Off, .Messages, Verbose)
    client.trace = vscode_jsonrpc_1.Trace.Verbose;
    let disposable = client.start();
    // push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map