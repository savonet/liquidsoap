#include <regex>
#include <stdlib.h>
#include <tree_sitter/parser.h>

enum TokenType {
  VAR,
  LPAR,
  LBRA,
  FLOAT_NO_LBRA,
  NO_EXTERNAL,
  COMMENT,
  UMINUS,
  NO_GETS,
};

enum State {
  START,
  POST_VAR,
  IN_FLOAT,
  IN_FLOAT_NO_LBRA,
  IN_COMMENT_START,
  IN_INLINE_COMMENT,
  IN_INLINE_COMMENT_END,
  IN_MULTILINE_COMMENT,
  IN_MULTILINE_COMMENT_END,
  IS_UMINUS,
  IS_COMMENT,
};

class Config {
public:
  bool no_uminus;
  int comment_level;
  Config() { reset(); }
  void reset() {
    no_uminus = false;
    comment_level = 0;
  }
};

#define NUMBER_REX L"[\\d]"
#define SKIP_REX L"[[:space:]]"
#define SPACE_REX L"[ ]"

extern "C" {
void *tree_sitter_liquidsoap_external_scanner_create() {
  Config *config = new Config;
  return config;
}

void tree_sitter_liquidsoap_external_scanner_destroy(void *config) {
  delete (Config *)config;
}

void tree_sitter_liquidsoap_external_scanner_reset(void *config) {
  ((Config *)config)->reset();
}

unsigned tree_sitter_liquidsoap_external_scanner_serialize(void *payload,
                                                           char *buffer) {
  return 0;
}

void tree_sitter_liquidsoap_external_scanner_deserialize(void *payload,
                                                         const char *buffer,
                                                         unsigned n) {}

bool tree_sitter_liquidsoap_external_scanner_scan(void *payload, TSLexer *lexer,
                                                  const bool *valid_symbols) {
  std::wregex const is_number(NUMBER_REX);
  std::wregex const skip_rex(SKIP_REX);
  std::wregex const space(SPACE_REX);
  State state = START;
  Config *config = (Config *)payload;
  std::wstring lookahead_string;

  if (!valid_symbols[VAR] && !valid_symbols[LBRA] && !valid_symbols[LPAR] &&
      !valid_symbols[NO_EXTERNAL] && !valid_symbols[FLOAT_NO_LBRA] &&
      !valid_symbols[COMMENT] && !valid_symbols[UMINUS]) {
    config->reset();
    return false;
  }

  if (valid_symbols[NO_EXTERNAL])
    return false;

  if (valid_symbols[VAR] || valid_symbols[LBRA] || valid_symbols[LPAR])
    state = POST_VAR;

  START_LEXER();
  eof = lexer->eof(lexer);
  lookahead_string = L"";
  lookahead_string += lookahead;

  if (eof) {
    config->reset();
    END_STATE();
  }

  switch (state) {
  case START:
    if (std::regex_match(lookahead_string, skip_rex))
      SKIP(START);

    if (std::regex_match(lookahead_string, is_number)) {
      config->no_uminus = true;
      ADVANCE(IN_FLOAT);
    }

    if (lookahead == ')') {
      config->no_uminus = true;
      END_STATE();
    }

    if (lookahead == '#')
      ADVANCE(IN_COMMENT_START);

    if (lookahead == '-' && !config->no_uminus)
      ADVANCE(IS_UMINUS);

    config->reset();
    END_STATE();

  case IN_FLOAT:
    if (std::regex_match(lookahead_string, is_number))
      ADVANCE(IN_FLOAT);

    if (lookahead == '_')
      ADVANCE(IN_FLOAT);

    if (lookahead == '.')
      ADVANCE(IN_FLOAT_NO_LBRA);

    config->no_uminus = true;
    END_STATE();

  case IN_FLOAT_NO_LBRA:
    if (std::regex_match(lookahead_string, space))
      SKIP(IN_FLOAT_NO_LBRA);

    if (lookahead == '{') {
      config->no_uminus = true;
      END_STATE();
    }

    if (std::regex_match(lookahead_string, is_number)) {
      config->no_uminus = true;
      END_STATE();
    }

    ACCEPT_TOKEN(FLOAT_NO_LBRA);
    config->no_uminus = true;
    END_STATE();

  case IN_COMMENT_START:
    if (lookahead == '<') {
      config->comment_level++;
      ADVANCE(IN_MULTILINE_COMMENT);
    }

    if (config->comment_level) {
      ADVANCE(IN_MULTILINE_COMMENT);
    }

    if (lookahead == '\n')
      ADVANCE(IS_COMMENT);

    ADVANCE(IN_INLINE_COMMENT);

  case IN_INLINE_COMMENT:
    if (lookahead == '\n') {
      lexer->mark_end(lexer);
      ADVANCE(IN_INLINE_COMMENT_END);
    }

    ADVANCE(IN_INLINE_COMMENT);

  case IN_INLINE_COMMENT_END:
    if (std::regex_match(lookahead_string, skip_rex))
      ADVANCE(IN_INLINE_COMMENT_END);

    if (lookahead == '#')
      ADVANCE(IN_INLINE_COMMENT);

    result = true;
    lexer->result_symbol = COMMENT;
    config->reset();
    END_STATE();

  case IN_MULTILINE_COMMENT:
    if (lookahead == '#')
      ADVANCE(IN_COMMENT_START);

    if (lookahead == '>')
      ADVANCE(IN_MULTILINE_COMMENT_END);

    ADVANCE(IN_MULTILINE_COMMENT);

  case IN_MULTILINE_COMMENT_END:
    if (lookahead == '#') {
      config->comment_level--;

      if (!config->comment_level)
        ADVANCE(IS_COMMENT);
    }

    ADVANCE(IN_MULTILINE_COMMENT);

  case POST_VAR:
    if (std::regex_match(lookahead_string, space))
      SKIP(POST_VAR);

    result = true;

    if (lookahead == '(') {
      lexer->result_symbol = LPAR;
    } else if (lookahead == '[') {
      lexer->result_symbol = LBRA;
    } else
      lexer->result_symbol = VAR;

    config->no_uminus = true;
    END_STATE();
  case IS_UMINUS:
    ACCEPT_TOKEN(UMINUS);
    config->reset();
    END_STATE();
  case IS_COMMENT:
    ACCEPT_TOKEN(COMMENT);
    config->reset();
    END_STATE();
  default:
    config->reset();
    END_STATE();
  }
}
}
