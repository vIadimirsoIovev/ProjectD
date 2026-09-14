#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>

#define print_opr(c) fprintf(outf, "O%c\n", (c + 32))

/* ";" | "," | ":=" | "=>" | ".." | "[" | "]" | "<" | ">" | "<=" | ">=" | "=" | "/=" |
  "+" | "-" | "*" | "/" | "(" | ")" | "{" | "}" | "." | "%" | "$" | "?[" | ".[" */

enum operators {
  O_SEMICOL,
  O_COMMA,
  O_ASSIGN,
  O_ARROW,
  O_RANGE,
  O_SBOPEN,
  O_SBCLOSE,
  O_LT,
  O_GT,
  O_LE,
  O_GE,
  O_EQ,
  O_NEQ,
  O_ADD,
  O_SUB,
  O_MUL,
  O_DIV,
  O_BOPEN,
  O_BCLOSE,
  O_CBOPEN,
  O_CBCLOSE,
  O_DOT,
  O_REM,
  O_LEN,
  O_EXISTS,
  O_ACCESS
};

static void error(const char* message) {
  perror(message);
  exit(EXIT_FAILURE);
}

// Returns a shifted enum value of the keyword or -1.

static int compKeywords(char* str, size_t len) {
  static const char* const keywords[] = {
    "var", "if", "then", "elsif", "else", "end", "while", "loop",
    "for", "in", "exit", "print", "return", "or", "and", "xor",
    "not", "is", "int", "real", "bool", "string", "none", "func",
    "true", "false"
  };

  size_t klen = sizeof(keywords) / sizeof(keywords[0]);
  size_t i;

  for (i = 0; i < klen; i++) {
    size_t maxlen = strlen(keywords[i]);
    if (len > maxlen) maxlen = len;

    if (memcmp(str, keywords[i], maxlen) == 0) return i + 32;
  }

  return -1;
}

static void parseLetter(FILE* inf, FILE* outf, int c){
  char buf[32];
  int len = 1;
  int kw = -1;

  buf[0] = (char)c;

  while((c = getc(inf)) != EOF){
    if (!isalpha(c) && !isdigit(c) && c != '_') { // Next token
      ungetc(c, inf);
      break;
    }

    if (len == 32) error("Too long identifier name");
    buf[len++] = (char)c;
  }

  kw = compKeywords(buf, len);

  if (kw >= 0) {
    fprintf(outf, "K%c\n", kw); // Keyword
  } else {
    fputc('E', outf); // Identifier

    {
      int i;
      for (i = 0; i < len; i++) {
        fputc(buf[i], outf);
      }
      fputc('\n', outf);
    }
  }
}

static void parseDigit(FILE* inf, FILE* outf, int c){
  int is_real = 0;
  int is_exp = 0;
  int len = 1;
  char buf[32];

  buf[0] = (char)c;

  // Reading the number.

  while ((c = getc(inf)) != EOF) {
    if (isdigit(c)) { // Numerical part
      if (len == 32) {
        error("Too long integer number");
      } else {
        buf[len++] = (char)c;
      }

    } else if (c == '.') { // Real dot
      if (is_real || is_exp) {
        error("Incorrect real syntax");
      } else {
        // Check for range op (..)
        int tempc = getc(inf);

        if (tempc == '.') {
          print_opr(O_RANGE);
          break;
        }

        ungetc(tempc, inf);

        is_real = 1;
      }
      if (len == 32) {
        error("Too long real number");
      }

      buf[len++] = (char)c;
      
    } else if (c == 'e') { // Exp real e
      if(is_exp){
        error("Incorrect real syntax");
      } else {
        is_real = 1;
        is_exp = 1;
        buf[len++] = (char)c;

        c = getc(inf);
        if(c == '+' || c == '-') { // 1e+1 or 1e-1
          buf[len++] = (char)c;
        } else if (isdigit(c)) { // 1e1
          ungetc(c, inf);
        } else {
          error("Incorrect real syntax");
        }
      }

    } else { // Next token
      ungetc(c, inf);
      break;
    }
  }

  if (is_real) {
    fputc('R', outf); // Real
  } else {
    fputc('I', outf); // Integer
  }

  {
    int i;
    for (i = 0; i < len; i++) {
      fputc(buf[i], outf);
    }
    fputc('\n', outf);
  }
}

static void parseOther(FILE* inf, FILE* outf, int c){
  switch (c) {
    // Operators
    case ';':
      print_opr(O_SEMICOL); // ;
    break;

    case ',':
      print_opr(O_COMMA); // ,
    break;

    case ':':
      c = getc(inf);
      if (c != '=') {
        error("Invalid operator");
      }
      print_opr(O_ASSIGN); // :=
    break;

    case '=':
      c = getc(inf);
      if (c == '>') {
        print_opr(O_ARROW); // =>
      } else {
        print_opr(O_EQ); // =
        ungetc(c, inf);
      }
    break;

    case '.':
      c = getc(inf);
      if (c == '.'){
        print_opr(O_RANGE); // ..
      } else if (c == '['){
        print_opr(O_ACCESS); // .[
      } else {
        print_opr(O_DOT); // .
        ungetc(c, inf);
      }
    break;

    case '[':
      print_opr(O_SBOPEN); // [
    break;

    case ']':
      print_opr(O_SBCLOSE); // ]
    break;

    case '<':
      c = getc(inf);
      if(c == '='){
         print_opr(O_LE); // <=
      } else {
        print_opr(O_LT); // <
         ungetc(c, inf);
      }
    break;

    case '>':
      c = getc(inf);
      if(c == '='){
         print_opr(O_GE); // >=
      } else {
         print_opr(O_GT); // >
        ungetc(c, inf);
      }
    break;

    case '/':
      c = getc(inf);
      if (c == '=') {
        print_opr(O_NEQ); // /=
      } else if (c == '/') { // Comments (single-line)
        while (1) {
          c = getc(inf);
          if (c == '\r' || c == '\n' || c == EOF) break;
        }
        ungetc(c, inf);
      } else {
        print_opr(O_DIV); // /
        ungetc(c, inf);
      }
    break;

    case '+':
      print_opr(O_ADD); // +
    break;

    case '-':
      print_opr(O_SUB); // -
    break;

    case '*':
      print_opr(O_MUL); // *
    break;

    case '(':
      print_opr(O_BOPEN); // (
    break;

    case ')':
      print_opr(O_BCLOSE); // )
    break;

    case '{':
      print_opr(O_CBOPEN); // {
    break;

    case '}':
      print_opr(O_CBCLOSE); // }
    break;

    case '%':
      print_opr(O_REM); // %
    break;

    case '$':
      print_opr(O_LEN); // $
    break;

    case '?':
      c = getc(inf);
      if (c != '[') {
        error("Invalid operator");
      }
      print_opr(O_EXISTS); // ?[
    break;

    // Strings
    case '\'': /* SingleQuoteString */
      fputc('S', outf);
      while ((c = getc(inf)) != '\'') {
        if(c == EOF) error("Incorrect string syntax");
        fputc(c, outf);
      }

      fputs("'\n", outf);
    break;

    case '\"': /* DoubleQuoteString */
      fputc('D', outf);
      while ((c = getc(inf)) != '"') {
        if(c == EOF) error("Incorrect string syntax");
        fputc(c, outf);
      }

      fputs("\"\n", outf);
    break;

    // Line break
    case '\n':
    case '\r':
      fputs("N\n", outf);
      while ((c = getc(inf)) != EOF) {
        if(c != '\n' && c != '\r') break;
      }
      ungetc(c, inf);
    break;

    // Whitespace
    case ' ':
    break;

    // If encountered an unexpected symbol that is not a part of a string, throw an error.
    default:
      error("Unexpected symbol");
  }
}

int main(void) {
  int c;
  
  FILE *inf = fopen("prog.d", "r");
  FILE *outf = fopen("tokens", "wb");

  if (inf == NULL) {
    error("Cannot open file prog.d");
    return EXIT_FAILURE;
  }

  if (outf == NULL) {
    error("Cannot open file tokens");
    return EXIT_FAILURE;
  }

  while ((c = getc(inf)) != EOF) {
    if (isalpha(c) || c == '_') {
      parseLetter(inf, outf, c); // Identifier | Keyword
    } else if (isdigit(c)) {
      parseDigit(inf, outf, c); // Integer | Real
    } else {
      parseOther(inf, outf, c); // Operator | String | NewLine
    }
  }

  fclose(inf);
  fclose(outf);
}
