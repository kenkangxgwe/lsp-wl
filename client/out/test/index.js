/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */
'use strict';
Object.defineProperty(exports, "__esModule", { value: true });
const testRunner = require("vscode/lib/testrunner");
testRunner.configure({
    ui: 'bdd',
    useColors: true,
    timeout: 100000
});
module.exports = testRunner;
//# sourceMappingURL=index.js.map