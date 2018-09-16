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
describe('Should get diagnostics', () => {
    const docUri = helper_1.getDocUri('diagnostics.txt');
    it('Diagnoses uppercase texts', () => __awaiter(this, void 0, void 0, function* () {
        yield testDiagnostics(docUri, [
            { message: 'ANY is all uppercase.', range: toRange(0, 0, 0, 3), severity: vscode.DiagnosticSeverity.Warning, source: 'ex' },
            { message: 'ANY is all uppercase.', range: toRange(0, 14, 0, 17), severity: vscode.DiagnosticSeverity.Warning, source: 'ex' },
            { message: 'OS is all uppercase.', range: toRange(0, 18, 0, 20), severity: vscode.DiagnosticSeverity.Warning, source: 'ex' }
        ]);
    }));
});
function toRange(sLine, sChar, eLine, eChar) {
    const start = new vscode.Position(sLine, sChar);
    const end = new vscode.Position(eLine, eChar);
    return new vscode.Range(start, end);
}
function testDiagnostics(docUri, expectedDiagnostics) {
    return __awaiter(this, void 0, void 0, function* () {
        yield helper_1.activate(docUri);
        const actualDiagnostics = vscode.languages.getDiagnostics(docUri);
        assert.equal(actualDiagnostics.length, expectedDiagnostics.length);
        expectedDiagnostics.forEach((expectedDiagnostic, i) => {
            const actualDiagnostic = actualDiagnostics[i];
            assert.equal(actualDiagnostic.message, expectedDiagnostic.message);
            assert.deepEqual(actualDiagnostic.range, expectedDiagnostic.range);
            assert.equal(actualDiagnostic.severity, expectedDiagnostic.severity);
        });
    });
}
//# sourceMappingURL=diagnostics.test.js.map