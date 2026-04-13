#include "tree_sitter/parser.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

// The external tokens we can emit, in the same order as
// the `externals` array in grammar.js
typedef enum {
    TOKEN_PNAME_LN,   // abbreviated_iri: prefix:local
    TOKEN_PN_LOCAL,   // simple_iri: local (no colon)
} TokenType;

// ── character classification ─────────────────────────────────────────────────

static bool is_pn_chars_base(int32_t c) {
    return (c >= 'A' && c <= 'Z') ||
           (c >= 'a' && c <= 'z') ||
           (c >= 0x00C0 && c <= 0x00D6) ||
           (c >= 0x00D8 && c <= 0x00F6) ||
           (c >= 0x00F8 && c <= 0x02FF) ||
           (c >= 0x0370 && c <= 0x037D) ||
           (c >= 0x037F && c <= 0x1FFF) ||
           (c >= 0x200C && c <= 0x200D) ||
           (c >= 0x2070 && c <= 0x218F) ||
           (c >= 0x2C00 && c <= 0x2FEF) ||
           (c >= 0x3001 && c <= 0xD7FF) ||
           (c >= 0xF900 && c <= 0xFDCF) ||
           (c >= 0xFDF0 && c <= 0xFFFD);
}

static bool is_pn_chars_u(int32_t c) {
    return is_pn_chars_base(c) || c == '_';
}

static bool is_pn_chars(int32_t c) {
    return is_pn_chars_u(c) ||
           c == '-' ||
           (c >= '0' && c <= '9') ||
           c == 0x00B7 ||
           (c >= 0x0300 && c <= 0x036F) ||
           (c >= 0x203F && c <= 0x2040);
}

// pn_local first char: pn_chars_u | [0-9]
static bool is_pn_local_start(int32_t c) {
    return is_pn_chars_u(c) || (c >= '0' && c <= '9');
}

// ── scanner ──────────────────────────────────────────────────────────────────

bool tree_sitter_owl_ms_external_scanner_scan(
    void *payload,
    TSLexer *lexer,
    const bool *valid_symbols
) {
    fprintf(stderr, "SCANNER called: lookahead=0x%X valid_pname=%d valid_pnlocal=%d\n",
        lexer->lookahead, valid_symbols[TOKEN_PNAME_LN], valid_symbols[TOKEN_PN_LOCAL]);

    // Only activate when at least one of our tokens is valid
    if (!valid_symbols[TOKEN_PNAME_LN] && !valid_symbols[TOKEN_PN_LOCAL]) {
        return false;
    }

    // Must start with a valid pn_local first character
    if (!is_pn_local_start(lexer->lookahead)) {
        return false;
    }

    // Consume characters that are valid in pn_chars or '.'
    // Track the last non-'.' position for mark_end (pn_local cannot end with '.')
    // Also watch for ':' which would make this a pname_ln
    bool found_colon = false;

    // Consume the first character
    lexer->advance(lexer, false);

    while (true) {
        if (lexer->lookahead == ':') {
            // This is a pname_ln: prefix: consumed so far is the prefix name
            // Now consume ':' and the local part
            found_colon = true;
            lexer->advance(lexer, false);

            // Consume pn_local after the colon
            if (!is_pn_local_start(lexer->lookahead)) {
                // "prefix:" with no local part is a bare prefix_name,
                // not a pname_ln — let the normal lexer handle it
                return false;
            }
            lexer->advance(lexer, false);
            lexer->mark_end(lexer);

            while (is_pn_chars(lexer->lookahead) || lexer->lookahead == '.') {
                if (lexer->lookahead == '.') {
                    // Don't mark_end on '.'; peek if next is pn_chars
                    lexer->advance(lexer, false);
                    if (!is_pn_chars(lexer->lookahead)) {
                        // trailing dot — stop before it
                        break;
                    }
                } else {
                    lexer->advance(lexer, false);
                }
                lexer->mark_end(lexer);
            }

            if (valid_symbols[TOKEN_PNAME_LN]) {
                lexer->result_symbol = TOKEN_PNAME_LN;
                return true;
            }
            return false;
        } else if (is_pn_chars(lexer->lookahead) || lexer->lookahead == '.') {
            if (lexer->lookahead != '.') {
                lexer->mark_end(lexer);
            }
            lexer->advance(lexer, false);
        } else {
            break;
        }
    }

    // No colon found — this is a simple_iri (pn_local)
    if (!found_colon && valid_symbols[TOKEN_PN_LOCAL]) {
        lexer->mark_end(lexer);
        lexer->result_symbol = TOKEN_PN_LOCAL;
        return true;
    }

    return false;
}

// ── boilerplate (stateless scanner) ──────────────────────────────────────────

void *tree_sitter_owl_ms_external_scanner_create() { return NULL; }
void  tree_sitter_owl_ms_external_scanner_destroy(void *p) { (void)p; }

unsigned tree_sitter_owl_ms_external_scanner_serialize(void *p, char *buf) {
    (void)p; (void)buf;
    return 0; // no state
}

void tree_sitter_owl_ms_external_scanner_deserialize(
    void *p, const char *buf, unsigned n
) {
    (void)p; (void)buf; (void)n;
}
