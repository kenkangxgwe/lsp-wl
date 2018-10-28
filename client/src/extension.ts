"use strict";

import * as net from "net";

import {Trace} from "vscode-jsonrpc";
import { window, workspace, commands, ExtensionContext, Uri, Disposable, TextEditor } from "vscode";
import {
    LanguageClient, LanguageClientOptions,
    StreamInfo,
    Position as LSPosition, Location as LSLocation,
    ServerOptions
} from "vscode-languageclient";

export function activate(context: ExtensionContext): void {

    // clangd command
    // const config = workspace.getConfiguration('WolframLanguageServer');
    // // The server is a started as a separate app and listens on port 5007
    // let wolfram: Executable = {
    //     command: config.get<string>('wolfram.kernelPath')
    //     // args: config.get<string>('arguments'),
    // };
    // let serverOptions: ServerOptions = wolfram;
    let connectionInfo: net.NetConnectOpts = {
        port: 6009
    };
    let serverOptions: ServerOptions = () => {
        // connect to language server via socket
        let socket: net.Socket = net.connect(connectionInfo);
        let result: StreamInfo = {
            writer: socket,
            reader: socket
        };
        return Promise.resolve(result);
    };

    let clientOptions: LanguageClientOptions = {
        documentSelector: ["wolfram"],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher("**/*.*")
        }
    };

    // create the language client and start the client.
    let client:LanguageClient = new LanguageClient("WolframLanguageServer", "Wolfram Language Server", serverOptions, clientOptions);

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
    client.trace = Trace.Verbose;
    let disposable:Disposable = client.start();

    // push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
}
