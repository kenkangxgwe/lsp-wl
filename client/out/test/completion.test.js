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
const assert = require("assert");
const helper_1 = require("./helper");
describe('Should do completion', () => {
    const docUri = helper_1.getDocUri('completion.txt');
    it('Completes JS/TS in txt file', () => __awaiter(this, void 0, void 0, function* () {
        yield testCompletion(docUri, new vscode.Position(0, 0), {
            items: [
                { label: 'JavaScript', kind: vscode.CompletionItemKind.Text },
                { label: 'TypeScript', kind: vscode.CompletionItemKind.Text }
            ]
        });
    }));
});
function testCompletion(docUri, position, expectedCompletionList) {
    return __awaiter(this, void 0, void 0, function* () {
        yield helper_1.activate(docUri);
        // Executing the command `vscode.executeCompletionItemProvider` to simulate triggering completion
        const actualCompletionList = (yield vscode.commands.executeCommand('vscode.executeCompletionItemProvider', docUri, position));
        assert.equal(actualCompletionList.items.length, expectedCompletionList.items.length);
        expectedCompletionList.items.forEach((expectedItem, i) => {
            const actualItem = actualCompletionList.items[i];
            assert.equal(actualItem.label, expectedItem.label);
            assert.equal(actualItem.kind, expectedItem.kind);
        });
    });
}
//# sourceMappingURL=completion.test.js.map