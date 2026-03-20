import { spawnSync } from "child_process";
import * as os from "os";
import { workspace, ExtensionContext, Uri, window } from 'vscode';
import { SemanticTokensFeature } from 'vscode-languageclient/lib/common/semanticTokens';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

let client: LanguageClient | undefined;

export function activate(context: ExtensionContext) {
	// Start the language server
	startClient(context);

	// Restart the language server when configuration changes
	context.subscriptions.push(
		workspace.onDidChangeConfiguration(async (e) => {
			if (e.affectsConfiguration('omn')) {
				await restartClient(context);
			}
		})
	);
}

async function startClient(context: ExtensionContext): Promise<void> {
	const command = await getServerCommand(context);
	if (!command) {
		return;
	}

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used
	const serverOptions: ServerOptions = {
		run: { command, transport: TransportKind.stdio },
		debug: { command, transport: TransportKind.stdio }
	};

	// Get configuration
	const config = workspace.getConfiguration('omn');
	const orderFrames = config.get<boolean>('orderFrames', false);

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
		},
		initializationOptions: {
			omn: {
				orderFrames: orderFrames
			}
		}
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'owl-ms-language-server',
		'OWL Manchester Syntax Language Server',
		serverOptions,
		clientOptions
	);

	client.registerFeature(new SemanticTokensFeature(client));

	// Start the client. This will also launch the server
	await client.start();
}

async function restartClient(context: ExtensionContext): Promise<void> {
	if (client) {
		await client.stop();
		client = undefined;
	}
	await startClient(context);
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
