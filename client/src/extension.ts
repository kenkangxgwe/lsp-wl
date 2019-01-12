"use strict";

import { workspace, ExtensionContext, Disposable, WorkspaceConfiguration } from "vscode";
import {
    LanguageClient, LanguageClientOptions,
    NodeModule,
    TransportKind
} from "vscode-languageclient";

export function activate(context: ExtensionContext): void {

    const config: WorkspaceConfiguration = workspace.getConfiguration("WolframLanguageServer");
    let wolframkernel: string = config.get<string>("WolframPath");
    let wlServerDir: string = config.get<string>("WLServerPath");
    if (wlServerDir[-1] !== "\\" && wlServerDir[-1] !== "/") {
        wlServerDir += "/";
    }
    let socketport: number = Number(config.get<number>("Port"));
    let theme: string = config.get<string>("Theme");
    let serverOptions: NodeModule = {
        module: wlServerDir + "init.wls",
        runtime: wolframkernel,
        // args: ["--theme=" + theme],
        transport: {
            kind: TransportKind.socket,
            port: socketport
        },
        options: {
            execArgv: ["-script"]
        }
    };

    let clientOptions: LanguageClientOptions = {
        documentSelector: ["wolfram"],
        synchronize: {
            fileEvents: workspace.createFileSystemWatcher("**/*.*")
        },
        initializationOptions: {
            theme: theme
        }
    };

    // create the language client and start the client.
    let client: LanguageClient = new LanguageClient("WolframLanguageServer", "Wolfram Language Server", serverOptions, clientOptions);

    let disposable: Disposable = client.start();

    // push the disposable to the context's subscriptions so that the
    // client can be deactivated on extension deactivation
    context.subscriptions.push(disposable);
}
