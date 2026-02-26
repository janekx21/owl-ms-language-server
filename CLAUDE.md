# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is an **OWL 2 Manchester Syntax Language Server** written in Rust with a VS Code extension. It provides IDE features (diagnostics, hover, completion, go-to-definition, rename, etc.) for `.omn` files containing OWL ontologies.

The project uses:
- **tower-lsp** for LSP protocol implementation
- **tree-sitter-owl-ms** for parsing (custom tree-sitter grammar)
- **horned-owl** for OWL/RDF ontology manipulation
- **ropey** for efficient text buffer management
- **tokio** for async runtime

## Build and Test Commands

### Language Server (Rust)

```bash
# Build the language server
cargo build

# Build release version
cargo build --release

# Run tests
cargo test

# Run specific test
cargo test test_name

# Run tests with logging output
cargo test -- --nocapture

# Lint
cargo clippy

# Format code
cargo fmt

# Run benchmarks
cargo bench
```

### VS Code Extension (TypeScript)

```bash
cd editors/code

# Install dependencies
npm install

# Compile TypeScript
npm run compile

# Watch mode for development
npm run watch

# Package extension
npm run package
```

### Development Setup

For VS Code development:
1. Build the language server with `cargo build`
2. Open `editors/code` in VS Code
3. Press F5 to launch Extension Development Host

To use local language server binary, either:
- Install it: `cargo install --path crates/owl-ms-language-server`
- Set `_OWL_MS_LSP_SERVER_DEBUG` environment variable to the binary path (e.g., `target/debug/owl-ms-language-server`)

The project requires **nightly Rust toolchain**: `rustup default nightly`

## Architecture

### Core Components

**Backend** (`src/lib.rs`):
- Main LSP implementation implementing `tower_lsp::LanguageServer`
- Owns a `SyncBackend` (via `Arc<RwLock<SyncBackend>>`) that manages workspaces
- Handles all LSP requests (hover, completion, diagnostics, etc.)
- Uses async/await extensively with tokio
- Clones itself for spawning async diagnostic tasks

**SyncBackend** (`src/sync_backend.rs`):
- Container for multiple `Workspace` instances
- Protected by RwLock for concurrent access
- Maps URLs to appropriate workspaces
- Creates single-file workspaces if no workspace folder contains the file

**Workspace** (`src/workspace.rs`):
- Contains `internal_documents` (HashMap<PathBuf, InternalDocument>) - files opened/indexed by the server
- Contains `external_documents` (HashMap<Url, ExternalDocument>) - external ontologies fetched from web
- Has a `WorkspaceFolder` and loaded `Catalog` files
- Provides frame lookup and IRI resolution across all documents

**InternalDocument** (`src/workspace.rs`):
- Represents an opened `.omn` file
- Contains: `Rope` (text buffer), `Tree` (parsed tree-sitter AST), version, path
- Incrementally updates tree on edits using tree-sitter's `InputEdit`
- Extracts "frames" (Class, ObjectProperty, etc.) from the AST
- Maps IRIs to their definitions and references

**ExternalDocument** (`src/workspace.rs`):
- Represents external ontologies loaded via HTTP or file system
- Parsed using horned-owl (OWL/XML, RDF/XML, etc.)
- Converted to in-memory graph using sophia

### Multi-File Architecture

The language server supports multi-ontology projects using **catalog-v001.xml** files:
- Catalog files map ontology IRIs (used in `Import:` statements) to local file paths
- Example: `<uri name="http://a.b/multi-file/other" uri="other.omn"/>`
- Catalogs are loaded recursively from workspace folders on initialization
- When a document imports an IRI, the server uses the catalog to resolve it to a local file
- If not in catalog, attempts to fetch from the IRI URL directly (can be disabled with `--offline` flag)

### Dependency Loading

When a document is opened (`did_open`):
1. Parse the document and extract all `Import:` IRIs
2. Spawn background task via `load_dependencies()` to resolve imports
3. For each import IRI:
   - Check catalog for local file mapping
   - If found locally, load as `InternalDocument` and recursively load its dependencies (depth limit: 2)
   - If not found, attempt HTTP fetch and parse as `ExternalDocument`
4. Background tasks use `.index_handles` to track completion (in tests, awaited synchronously)

### Diagnostic Flow

Diagnostics are computed asynchronously after document changes:
1. `did_change` triggers `update_diagnostics_for_url_and_dependent()`
2. Spawns async task that:
   - Computes diagnostics for the changed document
   - Publishes diagnostics via LSP `client.publish_diagnostics()`
   - Finds dependent documents (documents that import this one)
   - Recursively triggers diagnostics for each dependent
3. Race conditions are acceptable - diagnostics will converge to correct state

### Position Encoding

The server supports both UTF-8 and UTF-16 position encoding:
- Negotiated during LSP initialization
- Stored in `Backend.position_encoding: OnceCell<PositionEncodingKind>`
- All position conversions go through `Position::from_lsp()` / `Position::into_lsp()`
- `Rope` is used for efficient column/offset conversions

### Frame System

A "frame" is an OWL entity definition (Class, ObjectProperty, DataProperty, AnnotationProperty, Individual, Datatype):
- Extracted using tree-sitter queries (`src/queries.rs`)
- Each frame has: IRI, type, label (from rdfs:label), definitions (locations), references
- `FrameInfo` aggregates all information about an IRI across documents
- Used for hover, completion, go-to-definition, references, and rename

### IRI Handling

Three IRI forms are supported:
- **Full IRI**: `<http://example.org/Thing>`
- **Abbreviated IRI**: `ex:Thing` (prefix must be declared)
- **Simple IRI**: `Thing` (uses default prefix)

Functions like `abbreviated_iri_to_full_iri()` convert between forms using the document's prefix declarations.

### Tree-sitter Integration

- Grammar is in separate crate: `crates/tree-sitter-owl-ms/`
- Queries defined in `src/queries.rs` (highlighting, frame extraction, etc.)
- Global parser in `GLOBAL_PARSER` protected by Mutex
- Incremental reparsing on document edits using `tree.edit()` and `InputEdit`

## Key Files

- `src/lib.rs` - Main Backend and LanguageServer implementation
- `src/sync_backend.rs` - Workspace container with locking
- `src/workspace.rs` - Document management and frame extraction (large file ~3000 lines)
- `src/catalog.rs` - Catalog XML parsing and IRI resolution
- `src/web.rs` - HTTP client abstraction for fetching external ontologies
- `src/queries.rs` - Tree-sitter queries for syntax highlighting and frame detection
- `src/rope_provider.rs` - Adapter for tree-sitter to use ropey Rope
- `src/pos.rs`, `src/range.rs` - Position/Range types with LSP conversion

## Testing

- Unit tests in `src/tests.rs`
- Test files in `demo-ontologies/` including multi-file examples
- Tests use `#[test_log::test]` attribute for logging
- Some tests create temporary directories with `tempdir` crate
- Tests await indexing tasks synchronously (controlled by `#[cfg(test)]`)

## Common Patterns

**Reading a document**:
```rust
let sync = backend.read_sync().await;
let (doc, workspace) = sync.get_internal_document(&url)?;
// doc: &InternalDocument, workspace: &Workspace
```

**Modifying a document**:
```rust
let mut sync = backend.write_sync().await;
let (document, workspace) = sync.take_internal_document(&url)?;
let new_document = document.edit(&params, encoding)?;
workspace.insert_internal_document(new_document);
```

**Converting positions**:
```rust
let pos: Position = Position::from_lsp(lsp_position, doc.rope(), encoding)?;
let lsp_pos = pos.into_lsp(doc.rope(), encoding)?;
```

**Tree traversal**:
```rust
let node = doc.tree().root_node()
    .named_descendant_for_point_range(pos.into(), pos.into())?;
let text = node_text(&node, doc.rope());
```

## Development Notes

- Position encoding must be checked for all LSP position/range conversions
- Errors should use the custom `Error` type in `src/error.rs`
- Use `ResultExt` trait for `.log_if_error()` and `.inspect_log()`
- Diagnostics are published asynchronously - spawned tasks should clone `Backend`
- Lock ordering: always acquire `sync` lock before any document operations
- Tree-sitter parser is behind a global mutex - keep parse calls short
- Catalog files must be named exactly `catalog-v001.xml`
