/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */


import { spawnSync } from "child_process";
import * as os from "os";
import * as path from 'path';
import { workspace, ExtensionContext, Uri, window } from 'vscode';
import { SemanticTokensFeature } from 'vscode-languageclient/lib/common/semanticTokens';

import {
	ClientCapabilities,
	DocumentSelector,
	FeatureState,
	InitializeParams,
	LanguageClient,
	LanguageClientOptions,
	ServerCapabilities,
	ServerOptions,
	StaticFeature,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient;

class F implements StaticFeature {
	fillInitializeParams?: (params: InitializeParams) => void;
	fillClientCapabilities(capabilities: ClientCapabilities): void {
		capabilities.general = { positionEncodings: ['utf-8', 'utf-16'] };
	}
	preInitialize?: (capabilities: ServerCapabilities<any>, documentSelector: DocumentSelector) => void;
	initialize(capabilities: ServerCapabilities<any>, documentSelector: DocumentSelector): void {
	}
	getState(): FeatureState {
		return { kind: 'static' };
	}
	dispose(): void {
	}
}

export function activate(context: ExtensionContext) {
	// The server is implemented in rust
	//const command = "/home/janek/Git/owl-ms-language-server/target/debug/owl-ms-language-server";
	getServerCommand(context).then(command => {
		// If the extension is launched in debug mode then the debug server options are used
		// Otherwise the run options are used
		const serverOptions: ServerOptions = {
			run: { command, transport: TransportKind.stdio },
			debug: { command, transport: TransportKind.stdio }
		};

		// Options to control the language client
		const clientOptions: LanguageClientOptions = {
			// Register the server for plain text documents
			documentSelector: [{ scheme: 'file', language: 'owl-ms', }], // dont use pattern. this would break everything.
			synchronize: {
				// Notify the server about file changes to '.clientrc files contained in the workspace
				// TODO replace with useful
				fileEvents: workspace.createFileSystemWatcher('**/.clientrc')

			},
			markdown: {
				isTrusted: true
			}
		};

		// Create the language client and start the client.
		client = new LanguageClient(
			'owl-ms-language-server',
			'Owl Manchester Syntax Language Server',
			serverOptions,
			clientOptions
		);

		//client.registerFeature(new F());
		client.registerFeature(new SemanticTokensFeature(client));

		// Start the client. This will also launch the server
		client.start();
	})
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}

async function getServerCommand(context: ExtensionContext): Promise<string | undefined> {
	const explicitPath = process.env["__OWL_MS_LSP_SERVER_DEBUG"];
	if (explicitPath) {
		if (explicitPath.startsWith("~/")) {
			return os.homedir() + explicitPath.slice("~".length);
		}
		console.log("using debug language server");
		return explicitPath;
	}


	let installedServerVersion = await getVersion("owl-ms-language-server")
	if (installedServerVersion) {
		console.log("using installed language server");
		console.log(installedServerVersion)
		return "owl-ms-language-server";
	}

	const ext = process.platform === "win32" ? ".exe" : "";
	const bundled = Uri.joinPath(context.extensionUri, "server", `owl-ms-language-server${ext}`);
	const bundledExists = await fileExists(bundled);
	if (bundledExists) {
		console.log("using bundled language server");
		let server = bundled;
		return server.fsPath;
	}


	window.showErrorMessage(
		"Unfortunately we don't ship binaries for your platform yet. " +
		"You need to manually clone the owl-ms-language-server repository and " +
		"run `cargo install --path .` to build the language server from sources. " +
		"If you feel that your platform should be supported, please create an issue " +
		"about that [here](https://github.com/janekx21/owl-ms-language-server/issues) and we " +
		"will consider it.",
	);

	return undefined;
}


async function fileExists(uri: Uri) {
	return await workspace.fs.stat(uri).then(
		() => true,
		() => false,
	);
}

async function getVersion(command: string): Promise<string | undefined> {
	const res = spawnSync(command, ["--version"], {
		encoding: "utf8",
		env: { ...process.env },
	});
	let installedServerVersion = undefined;
	if (!res.error && res.status === 0) {
		const out = res.stdout.trim()
		if (out.startsWith("owl-ms-languge-server")) {
			return out
		}
	}
	return undefined
}
