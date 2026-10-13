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
static struct token* peek_n(int n);

static void parse_error(const char* expected) {
  fprintf(stderr, "parse error at line %d: expected %s, got %s\n",
          cur_line(), expected, token_desc(peek()));
  exit(EXIT_FAILURE);
}

static struct token* peek(void) {
  static struct token eof_tok; /* zeroed, type 0 means the end of input */
  /* newlines are only kept for line-number reporting: skip them here */
  while (cur_tok < tok_cnt && tokens[cur_tok].type == 'N') cur_tok++;
  if (cur_tok >= tok_cnt) return &eof_tok;
  return &tokens[cur_tok];
}

/* the n-th next significant token (0 = same as peek), newlines skipped */
static struct token* peek_n(int n) {
  static struct token eof_tok;
  int i = cur_tok;
  while (i < tok_cnt && tokens[i].type == 'N') i++;
  while (n-- > 0 && i < tok_cnt) {
    i++;
    while (i < tok_cnt && tokens[i].type == 'N') i++;
  }
  if (i >= tok_cnt) return &eof_tok;
  return &tokens[i];
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
    } else if (at_op(O_SBOPEN)) { /* rvalue "[expr]" or rvalue "[$]" */
      read_token();
      if (at_op(O_LEN)) {
        read_token();
        expect_op(O_SBCLOSE);
        ref = ast_node(AN_ARRLEN, 1, ref);
      } else {
        struct ast_node* idx = parse_expr();
        expect_op(O_SBCLOSE);
        ref = ast_node(AN_ARRELEM, 2, ref, idx);
      }
    } else if (at_op(O_LEN)) { /* rvalue "[$]" (kept for compatibility) */
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
  if (at_ident() && peek_n(1)->type == 'O' && peek_n(1)->tv.op == O_ASSIGN) {
    struct ast_node* id = ident_node(expect_ident());
    read_token(); /* := */
    return ast_node(AN_TPLELEM, 2, id, parse_expr());
  }

  return ast_node(AN_TPLELEM, 1, parse_expr());
}

/* ======================================================================== */
/* ---- remaining part: types, expressions, statements, body, driver ---- */
/* ======================================================================== */

static struct ast_node* parse_typeident(void);
static struct ast_node* parse_primary(void);
static struct ast_node* parse_unary(void);
static struct ast_node* parse_term(void);
static struct ast_node* parse_factor(void);
static struct ast_node* parse_rel(void);
static struct ast_node* parse_decl(void);
static struct ast_node* parse_if(void);
static struct ast_node* parse_while(void);
static struct ast_node* parse_for(void);
static struct ast_node* parse_loop(void);
static struct ast_node* parse_return(void);
static struct ast_node* parse_print(void);
static struct ast_node* parse_statement(void);

static void ast_dump(struct ast_node* n, int depth);
static void load_tokens(const char* path);
static char* read_until(FILE* f, int term);

static char* dup_str(const char* s) {
  size_t len = strlen(s);
  char* p = malloc(len + 1);
  if (p == NULL) error("Out of memory");
  memcpy(p, s, len + 1);
  return p;
}

/* ---- types -------------------------------------------------------------- */

/* typeident := "int" | "real" | "bool" | "string" | "none" | "func"
              | "[" typeident "]" | "{" [typeident {"," typeident}] "}" */
static struct ast_node* parse_typeident(void) {
  if (at_kw(K_INT))    { read_token(); return ast_node(AN_INT, 0); }
  if (at_kw(K_REAL))   { read_token(); return ast_node(AN_REAL, 0); }
  if (at_kw(K_BOOL))   { read_token(); return ast_node(AN_BOOL, 0); }
  if (at_kw(K_STRING)) { read_token(); return ast_node(AN_STRING, 0); }
  if (at_kw(K_NONE))   { read_token(); return ast_node(AN_NONE, 0); }
  if (at_kw(K_FUNC))   { read_token(); return ast_node(AN_FUNC, 0); }

  if (at_op(O_SBOPEN)) { /* array type */
    struct ast_node* elem;
    read_token();
    elem = parse_typeident();
    expect_op(O_SBCLOSE);
    return ast_node(AN_ARRTYPE, 1, elem);
  }

  if (at_op(O_CBOPEN)) { /* tuple type */
    struct ast_node* n = ast_node(AN_TPLTYPE, 0);
    read_token();
    if (!at_op(O_CBCLOSE)) {
      for (;;) {
        ast_add(n, parse_typeident());
        if (!at_op(O_COMMA)) break;
        read_token();
      }
    }
    expect_op(O_CBCLOSE);
    return n;
  }

  parse_error("type identifier");
  return NULL;
}

/* ---- expressions -------------------------------------------------------- */

static int starts_literal(void) {
  struct token* t = peek();
  if (t->type == 'I' || t->type == 'R' || t->type == 'S' || t->type == 'D')
    return 1;
  if (t->type == 'K' &&
      (t->tv.key == K_TRUE || t->tv.key == K_FALSE || t->tv.key == K_NONE))
    return 1;
  if (t->type == 'O' && (t->tv.op == O_SBOPEN || t->tv.op == O_CBOPEN))
    return 1;
  return 0;
}

/* primary := literal | funcliteral | "(" expr ")"
              | rvalue ["is" typeident] */
static struct ast_node* parse_primary(void) {
  struct token* t = peek();

  if (t->type == 'K' && t->tv.key == K_FUNC)
    return parse_funcliteral();

  if (at_op(O_BOPEN)) {
    struct ast_node* e;
    read_token();
    e = parse_expr();
    expect_op(O_BCLOSE);
    return ast_node(AN_PARENTHESES, 1, e);
  }

  if (starts_literal())
    return parse_literal();

  {
    struct ast_node* r = parse_rvalue();
    if (at_kw(K_IS)) {
      read_token();
      return ast_node(AN_ISEXPR, 2, r, parse_typeident());
    }
    return r;
  }
}

/* unary := ("+"|"-"|"not") unary | primary */
static struct ast_node* parse_unary(void) {
  if (at_op(O_ADD)) { read_token(); return ast_node(AN_UNARYPLUS,  1, parse_unary()); }
  if (at_op(O_SUB)) { read_token(); return ast_node(AN_UNARYMINUS, 1, parse_unary()); }
  if (at_kw(K_NOT)) { read_token(); return ast_node(AN_NOT,        1, parse_unary()); }
  return parse_primary();
}

/* term := unary {("*"|"/"|"%") unary} */
static struct ast_node* parse_term(void) {
  struct ast_node* first = parse_unary();
  struct ast_node* n;
  char ops[64];
  int cnt = 0;

  if (!at_op(O_MUL) && !at_op(O_DIV) && !at_op(O_REM)) return first;

  n = ast_node(AN_TERM, 0);
  ast_add(n, first);
  while (at_op(O_MUL) || at_op(O_DIV) || at_op(O_REM)) {
    if (at_op(O_MUL))      ops[cnt++] = '*';
    else if (at_op(O_DIV)) ops[cnt++] = '/';
    else                   ops[cnt++] = '%';
    read_token();
    ast_add(n, parse_unary());
  }
  ops[cnt] = '\0';
  n->node_type = dup_str(ops);
  return n;
}

/* factor := term {("+"|"-") term} */
static struct ast_node* parse_factor(void) {
  struct ast_node* first = parse_term();
  struct ast_node* n;
  char ops[64];
  int cnt = 0;

  if (!at_op(O_ADD) && !at_op(O_SUB)) return first;

  n = ast_node(AN_FACTOR, 0);
  ast_add(n, first);
  while (at_op(O_ADD) || at_op(O_SUB)) {
    ops[cnt++] = at_op(O_ADD) ? '+' : '-';
    read_token();
    ast_add(n, parse_term());
  }
  ops[cnt] = '\0';
  n->node_type = dup_str(ops);
  return n;
}

/* rel := factor [ ("<"|">"|"<="|">="|"="|"/=") factor ] */
static struct ast_node* parse_rel(void) {
  struct ast_node* l = parse_factor();
  struct token* t = peek();
  enum an_type ty;

  if (t->type != 'O') return l;
  switch (t->tv.op) {
    case O_LE:  ty = AN_COMP_LE; break;
    case O_LT:  ty = AN_COMP_LT; break;
    case O_EQ:  ty = AN_COMP_EQ; break;
    case O_NEQ: ty = AN_COMP_NE; break;
    case O_GE:  ty = AN_COMP_GE; break;
    case O_GT:  ty = AN_COMP_GT; break;
    default: return l;
  }
  read_token();
  return ast_node(ty, 2, l, parse_factor());
}

/* expr := rel {("and"|"or"|"xor") rel} */
struct ast_node* parse_expr(void) {
  struct ast_node* first = parse_rel();
  struct ast_node* n;
  char ops[64];
  int cnt = 0;

  if (!at_kw(K_AND) && !at_kw(K_OR) && !at_kw(K_XOR)) return first;

  n = ast_node(AN_EXPR, 0);
  ast_add(n, first);
  while (at_kw(K_AND) || at_kw(K_OR) || at_kw(K_XOR)) {
    if (at_kw(K_AND))      ops[cnt++] = 'a';
    else if (at_kw(K_OR))  ops[cnt++] = 'o';
    else                   ops[cnt++] = 'x';
    read_token();
    ast_add(n, parse_rel());
  }
  ops[cnt] = '\0';
  n->node_type = dup_str(ops);
  return n;
}

/* ---- statements --------------------------------------------------------- */

/* returns 1 when the next token cannot start a statement */
static int at_stmt_end(void) {
  struct token* t = peek();
  if (t->type == 0) return 1;
  if (t->type == 'O' && t->tv.op == O_SEMICOL) return 1;
  if (t->type == 'K' &&
      (t->tv.key == K_END || t->tv.key == K_ELSE || t->tv.key == K_ELSIF))
    return 1;
  return 0;
}

/* decl := "var" vardef {"," vardef} */
static struct ast_node* parse_decl(void) {
  struct ast_node* n = ast_node(AN_DECL, 0);

  expect_kw(K_VAR);
  for (;;) {
    struct ast_node* vd;
    char* name = expect_ident();
    if (at_op(O_ASSIGN)) {
      read_token();
      vd = ast_node(AN_VARDEF, 2, ident_node(name), parse_expr());
    } else {
      vd = ast_node(AN_VARDEF, 1, ident_node(name));
    }
    ast_add(n, vd);
    if (!at_op(O_COMMA)) break;
    read_token();
  }
  return n;
}

/* if := "if" expr "then" body {"elsif" expr "then" body}
        ["else" body] "end" | "if" expr "=>" expr */
static struct ast_node* parse_if(void) {
  struct ast_node *cond, *body, *n;

  expect_kw(K_IF);
  cond = parse_expr();

  if (at_op(O_ARROW)) { /* short form: if expr => statement */
    read_token();
    return ast_node(AN_IFSHORT, 2, cond, parse_statement());
  }

  expect_kw(K_THEN);
  body = parse_body();
  n = ast_node(AN_IF, 2, cond, body);

  while (at_kw(K_ELSIF)) {
    read_token();
    cond = parse_expr();
    expect_kw(K_THEN);
    ast_add(n, cond);
    ast_add(n, parse_body());
  }
  if (at_kw(K_ELSE)) {
    read_token();
    ast_add(n, parse_body());
  }
  expect_kw(K_END);
  return n;
}

/* while := "while" expr "loop" body "end" */
static struct ast_node* parse_while(void) {
  struct ast_node *cond, *body;

  expect_kw(K_WHILE);
  cond = parse_expr();
  expect_kw(K_LOOP);
  body = parse_body();
  expect_kw(K_END);
  return ast_node(AN_WHILE, 2, cond, body);
}

/* for := "for" ["ident" "in"] expr [".." expr] "loop" body "end" */
static struct ast_node* parse_for(void) {
  int has_var;
  char* vname = NULL;
  struct ast_node *from, *to = NULL, *body;

  expect_kw(K_FOR);

  has_var = at_ident() &&
            peek_n(1)->type == 'K' && peek_n(1)->tv.key == K_IN;
  if (has_var) {
    vname = expect_ident();
    expect_kw(K_IN);
  }

  from = parse_expr();

  if (at_op(O_RANGE)) {
    read_token();
    to = parse_expr();
  }

  expect_kw(K_LOOP);
  body = parse_body();
  expect_kw(K_END);

  if (has_var && to != NULL)
    return ast_node(AN_FORVD, 4, ident_node(vname), from, to, body);
  if (has_var)
    return ast_node(AN_FORVND, 3, ident_node(vname), from, body);
  if (to != NULL)
    return ast_node(AN_FORNVD, 3, from, to, body);
  return ast_node(AN_FORNVND, 2, from, body);
}

/* loop := "loop" body "end" */
static struct ast_node* parse_loop(void) {
  struct ast_node* body;
  expect_kw(K_LOOP);
  body = parse_body();
  expect_kw(K_END);
  return ast_node(AN_LOOP, 1, body);
}

/* return := "return" [expr] */
static struct ast_node* parse_return(void) {
  expect_kw(K_RETURN);
  if (at_stmt_end())
    return ast_node(AN_RETURN, 0);
  return ast_node(AN_RETURN, 1, parse_expr());
}

/* print := "print" expr {"," expr} */
static struct ast_node* parse_print(void) {
  struct ast_node* n = ast_node(AN_PRINT, 0);
  expect_kw(K_PRINT);
  for (;;) {
    ast_add(n, parse_expr());
    if (!at_op(O_COMMA)) break;
    read_token();
  }
  return n;
}

static struct ast_node* parse_statement(void) {
  if (at_kw(K_VAR))    return parse_decl();
  if (at_kw(K_IF))     return parse_if();
  if (at_kw(K_WHILE))  return parse_while();
  if (at_kw(K_FOR))    return parse_for();
  if (at_kw(K_LOOP))   return parse_loop();
  if (at_kw(K_EXIT))   { read_token(); return ast_node(AN_EXIT, 0); }
  if (at_kw(K_RETURN)) return parse_return();
  if (at_kw(K_PRINT))  return parse_print();
  if (at_ident())      return parse_assign();

  parse_error("statement");
  return NULL;
}

/* body := { statement [";"] } */
struct ast_node* parse_body(void) {
  struct ast_node* n = ast_node(AN_BODY, 0);

  for (;;) {
    while (at_op(O_SEMICOL)) read_token();
    if (at_kw(K_END) || at_kw(K_ELSE) || at_kw(K_ELSIF)) break;
    if (peek()->type == 0) break;
    ast_add(n, parse_statement());
  }

  return n;
}

/* ---- AST dump ----------------------------------------------------------- */

static const char* const an_name[] = {
  "BODY", "DECL", "ASSIGN", "IF", "IFSHORT", "WHILE", "FOR-NV-ND",
  "FOR-V-D", "FOR-NV-D", "FOR-V-ND", "LOOP", "EXIT", "RETURN", "PRINT",
  "VARDEF",
  "IDENT", "ARRELEM", "TUPLENAME", "TUPLEINT", "ARRLEN", "TESTELEM",
  "TUPLEEXPR", "FUNCCALL",
  "EXPR", "COMP-LE", "COMP-LT", "COMP-EQ", "COMP-NE", "COMP-GE", "COMP-GT",
  "FACTOR", "TERM", "ISEXPR", "UNARYPLUS", "UNARYMINUS", "NOT",
  "FUNCLIT", "FUNCLITSHORT", "PARENTHESES",
  "INT", "REAL", "BOOL", "STRING", "NONE", "FUNC", "ARRTYPE", "TPLTYPE",
  "INTLIT", "REALLIT", "TRUE", "FALSE", "STRINGLIT", "ARRLIT", "TPLLIT",
  "TPLELEM"
};

static void ast_dump(struct ast_node* n, int depth) {
  int i;
  if (n == NULL) return;
  for (i = 0; i < depth; i++) fputs("  ", stdout);
  fputs(an_name[n->type], stdout);
  if (n->node_type != NULL) printf(" [%s]", n->node_type);
  putchar('\n');
  for (i = 0; i < n->child_cnt; i++)
    ast_dump(n->children[i], depth + 1);
}

/* ---- token loading ------------------------------------------------------ */

/* reads characters up to (not including) `term`, returns a heap string */
static char* read_until(FILE* f, int term) {
  int cap = 64, len = 0, c;
  char* buf = malloc(cap);
  if (buf == NULL) error("Out of memory");

  while ((c = fgetc(f)) != EOF && c != term) {
    if (len + 1 >= cap) {
      cap *= 2;
      buf = realloc(buf, cap);
      if (buf == NULL) error("Out of memory");
    }
    buf[len++] = (char)c;
  }
  buf[len] = '\0';
  return buf;
}

/* The tokenizer writes one token per line:
     O<c> / K<c>   operator / keyword (c = enum value + 32)
     E<text>       identifier
     I<text>       integer      R<text>  real
     S<text>'      string       D<text>" double-quoted string
     N             line break */
static void load_tokens(const char* path) {
  FILE* f = fopen(path, "rb");
  int cap = 256, c;

  if (f == NULL) error("Cannot open file tokens");

  tokens = malloc(sizeof(struct token) * cap);
  if (tokens == NULL) error("Out of memory");
  tok_cnt = 0;

  while ((c = fgetc(f)) != EOF) {
    struct token t;
    memset(&t, 0, sizeof t);
    t.type = (char)c;

    if (c == 'O' || c == 'K') {
      int v = fgetc(f);
      if (v == EOF) break;
      if (c == 'O') t.tv.op  = (enum operator)(v - 32);
      else          t.tv.key = (enum keyword)(v - 32);
      fgetc(f); /* newline */
    } else if (c == 'N') {
      fgetc(f); /* newline */
    } else if (c == 'E' || c == 'I' || c == 'R') {
      t.tv.other = read_until(f, '\n');
    } else if (c == 'S' || c == 'D') {
      t.tv.other = read_until(f, (c == 'S') ? '\'' : '"');
      while ((c = fgetc(f)) != EOF && c != '\n') ; /* skip to end of line */
    } else {
      while ((c = fgetc(f)) != EOF && c != '\n') ; /* unknown: skip line */
      continue;
    }

    if (tok_cnt == cap) {
      cap *= 2;
      tokens = realloc(tokens, sizeof(struct token) * cap);
      if (tokens == NULL) error("Out of memory");
    }
    tokens[tok_cnt++] = t;
  }

  fclose(f);
}

int main(void) {
  struct ast_node* root;

  load_tokens("tokens");
  cur_tok = 0;

  root = parse_body();
  if (peek()->type != 0) parse_error("end of file");

  ast_dump(root, 0);
  return 0;
}