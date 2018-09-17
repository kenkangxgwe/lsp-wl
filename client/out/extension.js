"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
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
        documentSelector: ["mydsl"],
        synchronize: {
            fileEvents: vscode_1.workspace.createFileSystemWatcher("**/*.*")
        }
    };
    // create the language client and start the client.
    let client = new vscode_languageclient_1.LanguageClient("WolframLanguageServer", "Wolfram Language Server", serverOptions, clientOptions);
    var disposable2 = vscode_1.commands.registerCommand("mydsl.a.proxy", () => __awaiter(this, void 0, void 0, function* () {
        let activeEditor = vscode_1.window.activeTextEditor;
        if (!activeEditor || !activeEditor.document || activeEditor.document.languageId !== "mydsl") {
            return;
        }
        if (activeEditor.document.uri instanceof vscode_1.Uri) {
            vscode_1.commands.executeCommand("mydsl.a", activeEditor.document.uri.toString());
        }
    }));
    context.subscriptions.push(disposable2);
    // enable tracing (.Off, .Messages, Verbose)
    client.trace = vscode_jsonrpc_1.Trace.Verbose;
    let disposable = client.start();
    // push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
}
exports.activate = activate;
//# sourceMappingURL=extension.js.map