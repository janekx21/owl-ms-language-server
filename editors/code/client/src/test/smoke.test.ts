import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate, doc, setTestContent } from './helper';

// 0-indexed positions in testFixture/o.omn
const PERSON_DEF_LINE = 16;   // "Class: Person"
const PERSON_DEF_COL = 7;     // "Person" identifier starts here
const PERSON_REF_LINE = 22;   // "    SubClassOf: Person" (inside Student)
const PERSON_REF_COL = 16;
const ENROLLED_IN_LINE = 43;  // "ObjectProperty: enrolledIn"
const ENROLLED_IN_COL = 16;

suite('Should do smoke', () => {
  const docUri = getDocUri('o.omn');
  let originalContent: string;

  suiteSetup(async () => {
    await activate(docUri);
    originalContent = doc.getText();
  });

  teardown(async () => {
    await setTestContent(originalContent);
    await sleep(500);
  });

  test('Hover over prefix keyword', async () => {
    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      'vscode.executeHoverProvider', docUri, new vscode.Position(0, 0)
    );
    assert.ok(hovers.length === 1);
    assert.ok((hovers[0].contents[0] as vscode.MarkdownString).value.length > 0);
  });

  test('Hover over class definition', async () => {
    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      'vscode.executeHoverProvider', docUri, new vscode.Position(PERSON_DEF_LINE, PERSON_DEF_COL)
    );
    assert.ok(hovers.length > 0);
    assert.ok((hovers[0].contents[0] as vscode.MarkdownString).value.length > 0);
  });

  test('Hover over property', async () => {
    const hovers = await vscode.commands.executeCommand<vscode.Hover[]>(
      'vscode.executeHoverProvider', docUri, new vscode.Position(ENROLLED_IN_LINE, ENROLLED_IN_COL)
    );
    assert.ok(hovers.length > 0);
    assert.ok((hovers[0].contents[0] as vscode.MarkdownString).value.length > 0);
  });

  test('Go to definition of class reference', async () => {
    const locations = await vscode.commands.executeCommand<vscode.Location[]>(
      'vscode.executeDefinitionProvider', docUri, new vscode.Position(PERSON_REF_LINE, PERSON_REF_COL)
    );
    assert.ok(locations.length > 0);
    assert.strictEqual(locations[0].uri.fsPath, docUri.fsPath);
    assert.strictEqual(locations[0].range.start.line, PERSON_DEF_LINE);
  });

  test('Find references of class', async () => {
    const locations = await vscode.commands.executeCommand<vscode.Location[]>(
      'vscode.executeReferenceProvider', docUri, new vscode.Position(PERSON_DEF_LINE, PERSON_DEF_COL)
    );
    assert.ok(locations.length >= 2);
  });

  test('Completion at class reference position', async () => {
    const completions = await vscode.commands.executeCommand<vscode.CompletionList>(
      'vscode.executeCompletionItemProvider', docUri, new vscode.Position(PERSON_REF_LINE, PERSON_REF_COL)
    );
    assert.ok(completions.items.length > 0);
    const labels = completions.items.map(i =>
      typeof i.label === 'string' ? i.label : i.label.label
    );
    assert.ok(labels.some(l => l.includes('Student')));
  });

  test('Edited class appears in completions', async () => {
    await setTestContent(originalContent + '\n\nClass: SmokeTestClass\n');
    await sleep(1500);

    const completions = await vscode.commands.executeCommand<vscode.CompletionList>(
      'vscode.executeCompletionItemProvider', docUri, new vscode.Position(PERSON_REF_LINE, PERSON_REF_COL)
    );
    const labels = completions.items.map(i =>
      typeof i.label === 'string' ? i.label : i.label.label
    );
    assert.ok(labels.some(l => l.includes('SmokeTestClass')));
  });

  test('Rename class produces workspace edits', async () => {
    const edit = await vscode.commands.executeCommand<vscode.WorkspaceEdit>(
      'vscode.executeDocumentRenameProvider', docUri, new vscode.Position(PERSON_DEF_LINE, PERSON_DEF_COL), 'Human'
    );
    assert.ok(edit !== undefined);
    const changes = edit.get(docUri);
    assert.ok(changes.length >= 2);
  });
});

async function sleep(ms: number) {
  return new Promise(resolve => setTimeout(resolve, ms));
}
