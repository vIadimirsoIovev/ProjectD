#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

/* Operators in the same order as the enum in tokenizer.c */
enum operator {
  O_SEMICOL, O_COMMA, O_ASSIGN, O_ARROW, O_RANGE, O_SBOPEN, O_SBCLOSE,
  O_LT, O_GT, O_LE, O_GE, O_EQ, O_NEQ, O_ADD, O_SUB, O_MUL, O_DIV,
  O_BOPEN, O_BCLOSE, O_CBOPEN, O_CBCLOSE, O_DOT, O_REM, O_LEN,
  O_EXISTS, O_ACCESS
};

/* Same order as the keyword list in tokenizer.c */
enum keyword {
  K_VAR, K_IF, K_THEN, K_ELSIF, K_ELSE, K_END, K_WHILE, K_LOOP,
  K_FOR, K_IN, K_EXIT, K_PRINT, K_RETURN, K_OR, K_AND, K_XOR,
  K_NOT, K_IS, K_INT, K_REAL, K_BOOL, K_STRING, K_NONE, K_FUNC,
  K_TRUE, K_FALSE
};

enum an_type {
  AN_BODY, /* program|body; 1+ children */
  AN_DECL, /* var ...; 1+ children */
  AN_ASSIGN, /* ref := expr; 2 children */
  AN_IF, /* if expr then ...; 2+ children */
  AN_IFSHORT, /* if expr => ...; 2 children */
  AN_WHILE, /* while expr loop ...; 2 children */
  AN_FORNVND, /* for expr loop; 2 children */
  AN_FORVD, /* for ident in expr .. expr loop; 4 children */
  AN_FORNVD, /* for expr .. expr loop; 3 */
  AN_FORVND, /* for ident in expr loop; 3 */
  AN_LOOP, /* loop ...; 1 child */
  AN_EXIT, /* exit; 0 */
  AN_RETURN, /* return [expr]; 0-1 */
  AN_PRINT, /* print expr, expr; 1+ */
  AN_VARDEF, /* ident := expr; 1-2 */

  /* rvalue */
  AN_IDENT, /* ident; 0 */
  AN_ARRELEM, /* ref[expr]; 2 */
  AN_TUPLENAME, /* ref.ident; 2 */
  AN_TUPLEINT, /* ref.intlit; 2 */
  AN_ARRLEN, /* ref[$]; 1 */
  AN_TESTELEM, /* ref?[expr]; 2 */
  AN_TUPLEEXPR, /* ref.[expr]; 2 */
  AN_FUNCCALL, /* rvalue(expr, ...); 1+ */

  AN_EXPR, /* rel (and|or|xor) rel...; 2+ */
  AN_COMP_LE, AN_COMP_LT, AN_COMP_EQ, AN_COMP_NE, AN_COMP_GE, AN_COMP_GT,
  AN_FACTOR, /* term (+|-) term...; 2+ */
  AN_TERM, /* unary (*|/) unary...; 2+ */
  AN_ISEXPR, /* rvalue is typeident; 2 */
  AN_UNARYPLUS, AN_UNARYMINUS, AN_NOT,
  AN_FUNCLIT, AN_FUNCLITSHORT,
  AN_PARENTHESES, /* (expr); 1 */

  AN_INT, AN_REAL, AN_BOOL, AN_STRING, AN_NONE, AN_FUNC,
  AN_ARRTYPE, AN_TPLTYPE,

  AN_INTLIT, AN_REALLIT, AN_TRUE, AN_FALSE, AN_STRINGLIT,
  AN_ARRLIT, AN_TPLLIT, AN_TPLELEM
};

static const char* const op_str[] = {
  ";", ",", ":=", "=>", "..", "[", "]", "<", ">", "<=", ">=", "=", "/=",
  "+", "-", "*", "/", "(", ")", "{", "}", ".", "%", "$", "?[", ".["
};

static const char* const kw_str[] = {
  "var", "if", "then", "elsif", "else", "end", "while", "loop",
  "for", "in", "exit", "print", "return", "or", "and", "xor",
  "not", "is", "int", "real", "bool", "string", "none", "func",
  "true", "false"
};

union token_value {
  unsigned int num;
  enum operator op;
  enum keyword key;
  char *other;
};

struct token {
  char type;
  union token_value tv;
};

struct ast_node {
  enum an_type type;
  char *node_type; /* "aox" for and/or/xor, "+-" for +/- , ident/literal text */
  int child_cnt;
  struct ast_node **children;
};

static struct token *tokens;
static int tok_cnt;
static int cur_tok;

static void error(const char* message) {
  perror(message);
  exit(EXIT_FAILURE);
}

static int cur_line(void) {
  int line = 1;
  int i;
  for (i = 0; i < cur_tok && i < tok_cnt; i++)
    if (tokens[i].type == 'N') line++;
  return line;
}

static const char* token_desc(struct token* t) {
  static char buf[64];
  switch (t->type) {
    case 0:   return "end of file";
    case 'O': return op_str[t->tv.op];
    case 'K': return kw_str[t->tv.key];
    case 'E': snprintf(buf, sizeof buf, "identifier \"%s\"", t->tv.other); return buf;
    case 'I': snprintf(buf, sizeof buf, "integer %s", t->tv.other); return buf;
    case 'R': snprintf(buf, sizeof buf, "real %s", t->tv.other); return buf;
    case 'N': return "end of line";
    default:  return "string literal";
  }
}

static struct token* peek(void);

static void parse_error(const char* expected) {
  fprintf(stderr, "parse error at line %d: expected %s, got %s\n",
          cur_line(), expected, token_desc(peek()));
  exit(EXIT_FAILURE);
}

static struct token* peek(void) {
  static struct token eof_tok; /* zeroed, type 0 means the end of input */
  if (cur_tok >= tok_cnt) return &eof_tok;
  return &tokens[cur_tok];
}

static struct token* read_token(void) {
  struct token* t = peek();
  if (cur_tok < tok_cnt) cur_tok++;
  return t;
}

static int at_op(enum operator op) {
  struct token* t = peek();
  return t->type == 'O' && t->tv.op == op;
}

static int at_kw(enum keyword k) {
  struct token* t = peek();
  return t->type == 'K' && t->tv.key == k;
}

static int at_ident(void) {
  return peek()->type == 'E';
}

static void expect_op(enum operator op) {
  if (!at_op(op)) parse_error(op_str[op]);
  read_token();
}

static void expect_kw(enum keyword k) {
  if (!at_kw(k)) parse_error(kw_str[k]);
  read_token();
}

static char* expect_ident(void) {
  struct token* t = peek();
  if (t->type != 'E') parse_error("identifier");
  read_token();
  return t->tv.other;
}

static struct ast_node* ast_node(enum an_type type, int child_cnt, ...) {
  struct ast_node* n = malloc(sizeof(struct ast_node));
  int i;
  va_list ap;

  if (n == NULL) error("Out of memory");
  n->type = type;
  n->node_type = NULL;
  n->child_cnt = child_cnt;
  n->children = NULL;

  if (child_cnt > 0) {
    n->children = malloc(sizeof(struct ast_node*) * child_cnt);
    if (n->children == NULL) error("Out of memory");
    va_start(ap, child_cnt);
    for (i = 0; i < child_cnt; i++)
      n->children[i] = va_arg(ap, struct ast_node*);
    va_end(ap);
  }

  return n;
}

static void ast_add(struct ast_node* p, struct ast_node* c) {
  p->child_cnt++;
  p->children = realloc(p->children, sizeof(struct ast_node*) * p->child_cnt);
  if (p->children == NULL) error("Out of memory");
  p->children[p->child_cnt - 1] = c;
}

/* ident_node stores the name into node_type since an identifier has no children */
static struct ast_node* ident_node(char* name) {
  struct ast_node* n = ast_node(AN_IDENT, 0);
  n->node_type = name;
  return n;
}

/* expr and body are handled in the other two parts of the plan */
extern struct ast_node* parse_expr(void);
extern struct ast_node* parse_body(void);

/* ---- my part: lvalue, rvalue, assignments, arrays, tuples, functions ---- */

static struct ast_node* parse_lvalue(void);
static struct ast_node* parse_arrlit(void);
static struct ast_node* parse_tpllit(void);
static struct ast_node* parse_tplelem(void);

static struct ast_node* parse_assign(void) {
  struct ast_node* lhs = parse_lvalue();
  struct ast_node* rhs;

  expect_op(O_ASSIGN);
  rhs = parse_expr();

  return ast_node(AN_ASSIGN, 2, lhs, rhs);
}

/* lvalue := IDENT | lvalue "[" expr "]" */
static struct ast_node* parse_lvalue(void) {
  struct ast_node* ref = ident_node(expect_ident());

  while (at_op(O_SBOPEN)) {
    struct ast_node* idx;
    read_token();
    if (at_op(O_LEN)) { /* "[$]" - array length, reads as "[", "$", "]" */
      read_token();
      expect_op(O_SBCLOSE);
      ref = ast_node(AN_ARRLEN, 1, ref);
      continue;
    }
    idx = parse_expr();
    expect_op(O_SBCLOSE);
    ref = ast_node(AN_ARRELEM, 2, ref, idx);
  }

  return ref;
}

static struct ast_node* parse_rvalue(void) {
  struct ast_node* ref = parse_lvalue();

  for (;;) {
    if (at_op(O_BOPEN)) { /* rvalue(expr, ...) */
      struct ast_node* call = ast_node(AN_FUNCCALL, 0);
      read_token();
      ast_add(call, ref);
      if (!at_op(O_BCLOSE)) {
        for (;;) {
          ast_add(call, parse_expr());
          if (!at_op(O_COMMA)) break;
          read_token();
        }
      }
      expect_op(O_BCLOSE);
      ref = call;
    } else if (at_op(O_DOT)) { /* rvalue "." IDENT | rvalue "." INTLIT */
      read_token();
      if (at_ident()) {
        struct ast_node* id = ident_node(expect_ident());
        ref = ast_node(AN_TUPLENAME, 2, ref, id);
      } else if (peek()->type == 'I') {
        struct ast_node* iv = ast_node(AN_INTLIT, 0);
        iv->node_type = read_token()->tv.other;
        ref = ast_node(AN_TUPLEINT, 2, ref, iv);
      } else {
        parse_error("identifier or integer after \".\"");
      }
    } else if (at_op(O_LEN)) { /* rvalue "[$]" */
      read_token();
      if (at_op(O_SBCLOSE)) read_token();
      ref = ast_node(AN_ARRLEN, 1, ref);
    } else if (at_op(O_EXISTS)) { /* rvalue "?[" expr "]" */
      struct ast_node* idx;
      read_token();
      idx = parse_expr();
      expect_op(O_SBCLOSE);
      ref = ast_node(AN_TESTELEM, 2, ref, idx);
    } else if (at_op(O_ACCESS)) { /* rvalue ".[" expr "]" */
      struct ast_node* idx;
      read_token();
      idx = parse_expr();
      expect_op(O_SBCLOSE);
      ref = ast_node(AN_TUPLEEXPR, 2, ref, idx);
    } else {
      break;
    }
  }

  return ref;
}

/* funcliteral := "func" ["(" IDENT {"," IDENT} ")"] funbody */
/* funbody := "is" body "end" | "=>" expr */
static struct ast_node* parse_funcliteral(void) {
  struct ast_node* n = ast_node(AN_FUNCLITSHORT, 0);

  expect_kw(K_FUNC);

  if (at_op(O_BOPEN)) {
    read_token();
    while (!at_op(O_BCLOSE)) {
      ast_add(n, ident_node(expect_ident()));
      if (at_op(O_COMMA)) read_token();
    }
    expect_op(O_BCLOSE);
  }

  if (at_kw(K_IS)) {
    read_token();
    n->type = AN_FUNCLIT;
    ast_add(n, parse_body());
    expect_kw(K_END);
  } else if (at_op(O_ARROW)) {
    read_token();
    n->type = AN_FUNCLITSHORT;
    ast_add(n, parse_expr());
  } else {
    parse_error("\"is\" or \"=>\"");
    return NULL;
  }

  return n;
}

/* literal := INTLIT | REALLIT | "true" | "false" | STRINGLIT | arrlit | tpllit | "none" */
static struct ast_node* parse_literal(void) {
  struct token* t = peek();
  struct ast_node* n;

  if (t->type == 'I') {
    n = ast_node(AN_INTLIT, 0);
    n->node_type = read_token()->tv.other;
    return n;
  }
  if (t->type == 'R') {
    n = ast_node(AN_REALLIT, 0);
    n->node_type = read_token()->tv.other;
    return n;
  }
  if (t->type == 'S' || t->type == 'D') {
    n = ast_node(AN_STRINGLIT, 0);
    n->node_type = read_token()->tv.other;
    return n;
  }
  if (t->type == 'K') {
    switch (t->tv.key) {
      case K_TRUE:  read_token(); return ast_node(AN_TRUE, 0);
      case K_FALSE: read_token(); return ast_node(AN_FALSE, 0);
      case K_NONE:  read_token(); return ast_node(AN_NONE, 0);
      default: break;
    }
  }
  if (t->type == 'O') {
    if (t->tv.op == O_SBOPEN) return parse_arrlit();
    if (t->tv.op == O_CBOPEN) return parse_tpllit();
  }

  parse_error("literal");
  return NULL;
}

/* arrlit := "[" [expr {"," expr}] "]" */
static struct ast_node* parse_arrlit(void) {
  struct ast_node* n = ast_node(AN_ARRLIT, 0);

  expect_op(O_SBOPEN);
  if (!at_op(O_SBCLOSE)) {
    for (;;) {
      ast_add(n, parse_expr());
      if (!at_op(O_COMMA)) break;
      read_token();
    }
  }
  expect_op(O_SBCLOSE);

  return n;
}

/* tpllit := "{" tplelem {"," tplelem} "}" */
static struct ast_node* parse_tpllit(void) {
  struct ast_node* n = ast_node(AN_TPLLIT, 0);

  expect_op(O_CBOPEN);
  if (!at_op(O_CBCLOSE)) {
    for (;;) {
      ast_add(n, parse_tplelem());
      if (!at_op(O_COMMA)) break;
      read_token();
    }
  }
  expect_op(O_CBCLOSE);

  return n;
}

/* tplelem := [IDENT ":="] expr */
static struct ast_node* parse_tplelem(void) {
  if (at_ident() && cur_tok + 1 < tok_cnt &&
      tokens[cur_tok + 1].type == 'O' && tokens[cur_tok + 1].tv.op == O_ASSIGN) {
    struct ast_node* id = ident_node(expect_ident());
    read_token(); /* := */
    return ast_node(AN_TPLELEM, 2, id, parse_expr());
  }

  return ast_node(AN_TPLELEM, 1, parse_expr());
}