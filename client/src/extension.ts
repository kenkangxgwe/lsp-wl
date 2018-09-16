'use strict';

import * as net from 'net';

import {Trace} from 'vscode-jsonrpc';
import { window, workspace, commands, ExtensionContext, Uri } from 'vscode';
import { LanguageClient, LanguageClientOptions, StreamInfo, Position as LSPosition, Location as LSLocation, Executable, ServerOptions, ExecutableOptions } from 'vscode-languageclient';

export function activate(context: ExtensionContext) {

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
        // Connect to language server via socket
        let socket = net.connect(connectionInfo);
        let result: StreamInfo = {
            writer: socket,
            reader: socket
        };
        return Promise.resolve(result);
    };
    
    let clientOptions: LanguageClientOptions = {
        documentSelector: ['mydsl'],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher('**/*.*')
        }
    };
    
    // Create the language client and start the client.
    let lc = new LanguageClient('WolframLanguageServer', 'Wolfram Language Server', serverOptions, clientOptions);

    var disposable2 =commands.registerCommand("mydsl.a.proxy", async () => {
        let activeEditor = window.activeTextEditor;
        if (!activeEditor || !activeEditor.document || activeEditor.document.languageId !== 'mydsl') {
            return;
        }

        if (activeEditor.document.uri instanceof Uri) {
            commands.executeCommand("mydsl.a", activeEditor.document.uri.toString());
        }
    })

    context.subscriptions.push(disposable2);

    // enable tracing (.Off, .Messages, Verbose)
    lc.trace = Trace.Verbose;
    let disposable = lc.start();
    
    // Push the disposable to the context's subscriptions so that the 
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
}
