/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : new P(function (resolve) { resolve(result.value); }).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const vscode = require("vscode");
const path = require("path");
/**
 * Activates the vscode.lsp-sample extension
 */
function activate(docUri) {
    return __awaiter(this, void 0, void 0, function* () {
        // The extensionId is `publisher.name` from package.json
        const ext = vscode.extensions.getExtension('vscode.lsp-sample');
        yield ext.activate();
        try {
            exports.doc = yield vscode.workspace.openTextDocument(docUri);
            exports.editor = yield vscode.window.showTextDocument(exports.doc);
            yield sleep(2000); // Wait for server activation
        }
        catch (e) {
            console.error(e);
        }
    });
}
exports.activate = activate;
function sleep(ms) {
    return __awaiter(this, void 0, void 0, function* () {
        return new Promise(resolve => setTimeout(resolve, ms));
    });
}
exports.getDocPath = (p) => {
    return path.resolve(__dirname, '../../testFixture', p);
};
exports.getDocUri = (p) => {
    return vscode.Uri.file(exports.getDocPath(p));
};
function setTestContent(content) {
    return __awaiter(this, void 0, void 0, function* () {
        const all = new vscode.Range(exports.doc.positionAt(0), exports.doc.positionAt(exports.doc.getText().length));
        return exports.editor.edit(eb => eb.replace(all, content));
    });
}
exports.setTestContent = setTestContent;
//# sourceMappingURL=helper.js.map