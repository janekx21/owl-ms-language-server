import * as vscode from 'vscode';
import * as assert from 'assert';
import { getDocUri, activate } from './helper';

suite('Should do smoke', () => {
  const docUri = getDocUri('o.omn');

  test('Hover keyword', async () => {
    await activate(docUri);


    let hovers = (await vscode.commands.executeCommand('vscode.executeHoverProvider', docUri, new vscode.Position(0, 0))) as vscode.Hover[];

    assert.ok(hovers.length == 1);

    assert.ok((hovers[0].contents[0] as vscode.MarkdownString).value.length > 0);


    // await testCompletion(docUri, new vscode.Position(0, 0), {
    //   items: [
    //     { label: 'JavaScript', kind: vscode.CompletionItemKind.Text },
    //     { label: 'TypeScript', kind: vscode.CompletionItemKind.Text }
    //   ]
    // });
  });
});

async function testCompletion(
  docUri: vscode.Uri,
  position: vscode.Position,
  expectedCompletionList: vscode.CompletionList
) {
  await activate(docUri);

  // Executing the command `vscode.executeCompletionItemProvider` to simulate triggering completion
  const actualCompletionList = (await vscode.commands.executeCommand(
    'vscode.executeCompletionItemProvider',
    docUri,
    position
  )) as vscode.CompletionList;

  assert.ok(actualCompletionList.items.length >= 2);
  expectedCompletionList.items.forEach((expectedItem, i) => {
    const actualItem = actualCompletionList.items[i];
    assert.equal(actualItem.label, expectedItem.label);
    assert.equal(actualItem.kind, expectedItem.kind);
  });
}
