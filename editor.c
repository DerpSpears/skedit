/*                          NOTE : This text editor has been built following the Guide bellow
                                https://viewsourcecode.org/snaptoken/kilo/index.html
                                    BUT with a few logical changes, Rope Data 
                                        structure(as god intended) and with
                                            regex searching and undo redo.
                                                Refrences used:
                                     https://vt100.net/docs/vt100-ug/chapter3.html
                                            Youtube channel mjölnir
                                                  Geeks For Geeks
                      https://www.geeksforgeeks.org/dsa/ropes-data-structure-fast-string-concatenation/
                                              Wyatt Saltzman @ Medium
*/


#define _DEFAULT_SOURCE
#define _BSD_SOURCE
#define _GNU_SOURCE

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#include "editor.h"

/*** filetypes ***/

char *C_HL_extensions[] = {".c", ".h", ".cpp", NULL};
char *C_HL_keywords[] = {
    "switch", "if", "while", "for", "break", "continue",
    "return", "else", "case", "default",
    "struct|", "union|", "typedef|", "static|", "enum|", "class|",
    "int|", "long|", "double|", "float|", "char|",
    "unsigned|", "signed|", "void|", "const|", "short|", "size_t|",
    NULL};

struct editorSyntax HLDB[] = {
    {"c", C_HL_extensions, C_HL_keywords, "//", "/*", "*/",
     HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS},
};

#define HLDB_ENTRIES (int)(sizeof(HLDB) / sizeof(HLDB[0]))

static struct editorConfig E;

static CommandStack undo_stack = {NULL, 0};
static CommandStack redo_stack = {NULL, 0};
static int recording_commands = 1;

/*** rope operations ***/

static RopeNode *ropeCreateLeaf(const char *str, int len)
{
  RopeNode *node = malloc(sizeof(RopeNode));
  node->left = node->right = NULL;
  node->is_leaf = 1;
  node->len = len;
  node->weight = len;
  node->data = malloc(len + 1);
  if (str)
  {
    memcpy(node->data, str, len);
  }
  node->data[len] = '\0';
  node->hl = NULL;
  return node;
}

static RopeNode *ropeCreateInternal(RopeNode *left, RopeNode *right)
{
  RopeNode *node = malloc(sizeof(RopeNode));
  node->left = left;
  node->right = right;
  node->is_leaf = 0;
  node->data = NULL;
  node->hl = NULL;
  node->len = 0;

  node->weight = 0;
  if (left)
  {
    RopeNode *curr = left;
    while (curr)
    {
      if (curr->is_leaf)
      {
        node->weight += curr->len;
        break;
      }
      node->weight += curr->weight;
      curr = curr->right;
    }
  }

  return node;
}

static int ropeLength(RopeNode *node)
{
  if (!node)
    return 0;
  if (node->is_leaf)
    return node->len;
  return node->weight + ropeLength(node->right);
}

static void ropeFree(RopeNode *node)
{
  if (!node)
    return;
  if (node->is_leaf)
  {
    free(node->data);
    free(node->hl);
  }
  else
  {
    ropeFree(node->left);
    ropeFree(node->right);
  }
  free(node);
}

static char ropeCharAt(RopeNode *node, int pos)
{
  if (!node)
    return '\0';
  if (node->is_leaf)
  {
    if (pos < node->len)
      return node->data[pos];
    return '\0';
  }
  if (pos < node->weight)
  {
    return ropeCharAt(node->left, pos);
  }
  return ropeCharAt(node->right, pos - node->weight);
}

static void ropeToString(RopeNode *node, char *buffer, int *offset)
{
  if (!node)
    return;
  if (node->is_leaf)
  {
    memcpy(buffer + *offset, node->data, node->len);
    *offset += node->len;
    return;
  }
  ropeToString(node->left, buffer, offset);
  ropeToString(node->right, buffer, offset);
}

static RopeNode *ropeConcat(RopeNode *left, RopeNode *right)
{
  if (!left)
    return right;
  if (!right)
    return left;
  return ropeCreateInternal(left, right);
}

static void ropeSplitAt(RopeNode *node, int pos, RopeNode **left_out, RopeNode **right_out)
{
  if (!node)
  {
    *left_out = *right_out = NULL;
    return;
  }

  if (node->is_leaf)
  {
    if (pos <= 0)
    {
      *left_out = NULL;
      *right_out = node;
    }
    else if (pos >= node->len)
    {
      *left_out = node;
      *right_out = NULL;
    }
    else
    {
      *left_out = ropeCreateLeaf(node->data, pos);
      *right_out = ropeCreateLeaf(node->data + pos, node->len - pos);
      ropeFree(node);
    }
    return;
  }

  if (pos < node->weight)
  {
    RopeNode *ll, *lr;
    ropeSplitAt(node->left, pos, &ll, &lr);
    *left_out = ll;
    *right_out = ropeConcat(lr, node->right);
    free(node);
  }
  else if (pos > node->weight)
  {
    RopeNode *rl, *rr;
    ropeSplitAt(node->right, pos - node->weight, &rl, &rr);
    *left_out = ropeConcat(node->left, rl);
    *right_out = rr;
    free(node);
  }
  else
  {
    *left_out = node->left;
    *right_out = node->right;
    free(node);
  }
}

static RopeNode *ropeInsert(RopeNode *root, int pos, int c)
{
  RopeNode *left, *right;
  ropeSplitAt(root, pos, &left, &right);

  char ch = (char)c;
  RopeNode *middle = ropeCreateLeaf(&ch, 1);

  RopeNode *temp = ropeConcat(left, middle);
  return ropeConcat(temp, right);
}

static RopeNode *ropeInsertString(RopeNode *root, int pos, const char *str, int len)
{
  RopeNode *left, *right;
  ropeSplitAt(root, pos, &left, &right);

  RopeNode *middle = ropeCreateLeaf(str, len);
  RopeNode *temp = ropeConcat(left, middle);
  return ropeConcat(temp, right);
}

static RopeNode *ropeDelete(RopeNode *root, int pos, int len)
{
  if (len <= 0)
    return root;

  RopeNode *left, *middle, *right;
  ropeSplitAt(root, pos, &left, &middle);
  ropeSplitAt(middle, len, &middle, &right);

  ropeFree(middle);
  return ropeConcat(left, right);
}

/*** undo/redo implementation ***/

void pushCommand(CommandStack *stack, Command *cmd)
{
  if (stack->size >= MAX_UNDO_LEVELS)
  {
    Command *curr = stack->top;
    Command *prev = NULL;
    while (curr && curr->next)
    {
      prev = curr;
      curr = curr->next;
    }
    if (prev)
    {
      prev->next = NULL;
      if (curr->deleted_line)
        free(curr->deleted_line);
      free(curr);
      stack->size--;
    }
  }

  cmd->next = stack->top;
  stack->top = cmd;
  stack->size++;
}

Command *popCommand(CommandStack *stack)
{
  if (stack->top == NULL)
    return NULL;

  Command *cmd = stack->top;
  stack->top = cmd->next;
  stack->size--;
  return cmd;
}

void clearStack(CommandStack *stack)
{
  while (stack->top != NULL)
  {
    Command *cmd = popCommand(stack);
    if (cmd->deleted_line)
      free(cmd->deleted_line);
    free(cmd);
  }
}

void recordCommand(enum CommandType type, int cx, int cy, int c,
                          char *deleted_line, int deleted_len)
{
  if (!recording_commands)
    return;

  clearStack(&redo_stack);

  Command *cmd = malloc(sizeof(Command));
  cmd->type = type;
  cmd->cx = cx;
  cmd->cy = cy;
  cmd->c = c;
  cmd->deleted_line = deleted_line;
  cmd->deleted_len = deleted_len;
  cmd->next = NULL;

  pushCommand(&undo_stack, cmd);
}

/*** terminal ***/

void die(const char *s)
{
  write(STDOUT_FILENO, "\x1b[2J", 4);
  write(STDOUT_FILENO, "\x1b[H", 3);
  perror(s);
  exit(1);
}

static void rawModeOff(void)
{
  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &E.orig_termios) == -1)
    die("tcsetattr");
}

void rawModeOn(void)
{
  if (tcgetattr(STDIN_FILENO, &E.orig_termios) == -1)
    die("tcgetattr");
  atexit(rawModeOff);

  struct termios raw = E.orig_termios;
  raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;

  if (tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw) == -1)
    die("tcsetattr");
}

static int readKey(void)
{
  int nread;
  char c;
  while ((nread = (int)read(STDIN_FILENO, &c, 1)) != 1)
  {
    if (nread == -1 && errno != EAGAIN)
      die("read");
  }

  if (c == '\x1b')
  {
    char seq[3];
    if (read(STDIN_FILENO, &seq[0], 1) != 1)
      return '\x1b';
    if (read(STDIN_FILENO, &seq[1], 1) != 1)
      return '\x1b';

    if (seq[0] == '[')
    {
      if (seq[1] >= '0' && seq[1] <= '9')
      {
        char seq2;
        if (read(STDIN_FILENO, &seq2, 1) != 1)
          return '\x1b';
        if (seq2 == '~')
        {
          switch (seq[1])
          {
          case '1':
          case '7':
            return HKEY;
          case '4':
          case '8':
            return EKEY;
          case '3':
            return DKEY;
          case '5':
            return PAGEUP;
          case '6':
            return PAGEDOWN;
          }
        }
      }
      else
      {
        switch (seq[1])
        {
        case 'A':
          return AUP;
        case 'B':
          return ADOWN;
        case 'C':
          return ARIGHT;
        case 'D':
          return ALEFT;
        case 'H':
          return HKEY;
        case 'F':
          return EKEY;
        }
      }
    }
    return '\x1b';
  }

  return c;
}

static int getCursorPosition(int *rows, int *cols)
{
  char buf[32];
  unsigned int i = 0;

  if (write(STDOUT_FILENO, "\x1b[6n", 4) != 4)
    return -1;

  while (i < sizeof(buf) - 1)
  {
    if (read(STDIN_FILENO, &buf[i], 1) != 1)
      break;
    if (buf[i] == 'R')
      break;
    i++;
  }
  buf[i] = '\0';

  if (buf[0] != '\x1b' || buf[1] != '[')
    return -1;
  if (sscanf(&buf[2], "%d;%d", rows, cols) != 2)
    return -1;

  return 0;
}

static int getWindowSize(int *rows, int *cols)
{
  struct winsize ws;

  if (ioctl(STDOUT_FILENO, TIOCGWINSZ, &ws) == -1 || ws.ws_col == 0)
  {
    if (write(STDOUT_FILENO, "\x1b[999C\x1b[999B", 12) != 12)
      return -1;
    return getCursorPosition(rows, cols);
  }
  else
  {
    *cols = ws.ws_col;
    *rows = ws.ws_row;
    return 0;
  }
}

/*** syntax highlighting ***/

static int is_separator(int c)
{
  return isspace(c) || c == '\0' || strchr(",.()+-/*=~%<>[];{}", c) != NULL;
}

void editorUpdateSyntax(RopeLine *line)
{
  line->hl = realloc(line->hl, line->rsize);
  memset(line->hl, HL_NORMAL, line->rsize);

  if (E.syntax == NULL)
    return;

  char **keywords = E.syntax->keywords;
  char *scs = E.syntax->singleline_comment_start;
  char *mcs = E.syntax->multiline_comment_start;
  char *mce = E.syntax->multiline_comment_end;

  int scs_len = scs ? (int)strlen(scs) : 0;
  int mcs_len = mcs ? (int)strlen(mcs) : 0;
  int mce_len = mce ? (int)strlen(mce) : 0;

  int prev_sep = 1;
  int in_string = 0;
  int in_comment = line->hl_open_comment;

  int i = 0;
  while (i < line->rsize)
  {
    char c = line->render[i];
    unsigned char prev_hl = (i > 0) ? line->hl[i - 1] : HL_NORMAL;

    if (scs_len && !in_string && !in_comment)
    {
      if (!strncmp(&line->render[i], scs, scs_len))
      {
        memset(&line->hl[i], HL_COMMENT, line->rsize - i);
        break;
      }
    }

    if (mcs_len && mce_len && !in_string)
    {
      if (in_comment)
      {
        line->hl[i] = HL_MLCOMMENT;
        if (!strncmp(&line->render[i], mce, mce_len))
        {
          memset(&line->hl[i], HL_MLCOMMENT, mce_len);
          i += mce_len;
          in_comment = 0;
          prev_sep = 1;
          continue;
        }
        else
        {
          i++;
          continue;
        }
      }
      else if (!strncmp(&line->render[i], mcs, mcs_len))
      {
        memset(&line->hl[i], HL_MLCOMMENT, mcs_len);
        i += mcs_len;
        in_comment = 1;
        continue;
      }
    }

    if (E.syntax->flags & HL_HIGHLIGHT_STRINGS)
    {
      if (in_string)
      {
        line->hl[i] = HL_STRING;
        if (c == '\\' && i + 1 < line->rsize)
        {
          line->hl[i + 1] = HL_STRING;
          i += 2;
          prev_sep = 0;
          continue;
        }
        if (c == in_string)
          in_string = 0;
        i++;
        prev_sep = 0;
        continue;
      }
      else
      {
        if (c == '"' || c == '\'')
        {
          in_string = c;
          line->hl[i] = HL_STRING;
          i++;
          prev_sep = 0;
          continue;
        }
      }
    }

    if ((E.syntax->flags & HL_HIGHLIGHT_NUMBERS) &&
        (isdigit(c) || (c == '.' && i > 0 && isdigit(line->render[i - 1]))))
    {
      if (prev_sep || prev_hl == HL_NUMBER)
      {
        line->hl[i] = HL_NUMBER;
        i++;
        prev_sep = 0;
        continue;
      }
    }

    if (prev_sep)
    {
      int j;
      for (j = 0; keywords[j]; j++)
      {
        int klen = (int)strlen(keywords[j]);
        int kw2 = keywords[j][klen - 1] == '|';
        if (kw2)
          klen--;

        if (!strncmp(&line->render[i], keywords[j], klen) &&
            is_separator(line->render[i + klen]))
        {
          memset(&line->hl[i], kw2 ? HL_KEYWORD2 : HL_KEYWORD1, klen);
          i += klen;
          break;
        }
      }
      if (keywords[j] != NULL)
      {
        prev_sep = 0;
        continue;
      }
    }

    prev_sep = is_separator(c);
    line->hl[i] = HL_NORMAL;
    i++;
  }

  int changed = (line->hl_open_comment != in_comment);
  line->hl_open_comment = in_comment;
  if (changed && E.cy + 1 < E.numofrows)
    editorUpdateSyntax(&E.lines[E.cy + 1]);
}

static int editorSyntaxToColor(int hl)
{
  switch (hl)
  {
  case HL_COMMENT:
  case HL_MLCOMMENT:
    return 36;
  case HL_KEYWORD1:
    return 33;
  case HL_KEYWORD2:
    return 32;
  case HL_STRING:
    return 35;
  case HL_NUMBER:
    return 31;
  case HL_MATCH:
    return 34;
  default:
    return 37;
  }
}

void editorSelectSyntaxHighlight(void)
{
  E.syntax = NULL;
  if (E.filename == NULL)
    return;

  char *ext = strrchr(E.filename, '.');

  for (int j = 0; j < HLDB_ENTRIES; j++)
  {
    struct editorSyntax *s = &HLDB[j];
    char **p = s->filematch;
    for (int i = 0; p[i]; i++)
    {
      int is_ext = (p[i][0] == '.');
      if ((is_ext && ext && !strcmp(ext, p[i])) ||
          (!is_ext && strstr(E.filename, p[i])))
      {
        E.syntax = s;

        for (int filerow = 0; filerow < E.numofrows; filerow++)
          editorUpdateSyntax(&E.lines[filerow]);

        return;
      }
    }
  }
}

/*** line operations ***/

static int editorLineCxToRx(RopeLine *line, int cx)
{
  int rx = 0;
  for (int j = 0; j < cx && j < line->total_len; j++)
  {
    char c = ropeCharAt(line->root, j);
    if (c == '\t')
      rx += (TAB_STOP - 1) - (rx % TAB_STOP);
    rx++;
  }
  return rx;
}

static int editorLineRxToCx(RopeLine *line, int rx)
{
  int cur_rx = 0;
  int cx;
  for (cx = 0; cx < line->total_len; cx++)
  {
    char c = ropeCharAt(line->root, cx);
    if (c == '\t')
      cur_rx += (TAB_STOP - 1) - (cur_rx % TAB_STOP);
    cur_rx++;
    if (cur_rx > rx)
      return cx;
  }
  return cx;
}

void editorUpdateRow(RopeLine *line)
{
  if (!line->dirty_render)
    return;

  int len = ropeLength(line->root);
  char *chars = malloc(len + 1);
  int offset = 0;
  ropeToString(line->root, chars, &offset);
  chars[len] = '\0';

  int tabs = 0;
  for (int j = 0; j < len; j++)
    if (chars[j] == '\t')
      tabs++;

  free(line->render);
  line->render = malloc(len + tabs * (TAB_STOP - 1) + 1);

  int idx = 0;
  for (int j = 0; j < len; j++)
  {
    if (chars[j] == '\t')
    {
      line->render[idx++] = ' ';
      while (idx % TAB_STOP != 0)
        line->render[idx++] = ' ';
    }
    else
    {
      line->render[idx++] = chars[j];
    }
  }
  line->render[idx] = '\0';
  line->rsize = idx;
  line->total_len = len;

  free(chars);

  editorUpdateSyntax(line);
  line->dirty_render = 0;
}

static void editorInsertLine(int at, const char *s, int len)
{
  if (at < 0 || at > E.numofrows)
    return;

  E.lines = realloc(E.lines, sizeof(RopeLine) * (E.numofrows + 1));
  memmove(&E.lines[at + 1], &E.lines[at],
          sizeof(RopeLine) * (E.numofrows - at));

  E.lines[at].root = ropeCreateLeaf(s, len);
  E.lines[at].total_len = len;
  E.lines[at].render = NULL;
  E.lines[at].rsize = 0;
  E.lines[at].hl = NULL;
  E.lines[at].hl_open_comment = 0;
  E.lines[at].dirty_render = 1;

  editorUpdateRow(&E.lines[at]);

  E.numofrows++;
  E.dirty++;
}

static void editorFreeLine(RopeLine *line)
{
  ropeFree(line->root);
  free(line->render);
  free(line->hl);
}

static void editorDelLine(int at)
{
  if (at < 0 || at >= E.numofrows)
    return;

  editorFreeLine(&E.lines[at]);
  memmove(&E.lines[at], &E.lines[at + 1],
          sizeof(RopeLine) * (E.numofrows - at - 1));

  E.numofrows--;
  E.dirty++;
}

static void editorLineInsertChar(RopeLine *line, int at, int c)
{
  if (at < 0)
    at = 0;
  if (at > line->total_len)
    at = line->total_len;

  line->root = ropeInsert(line->root, at, c);
  line->total_len++;
  line->dirty_render = 1;
  editorUpdateRow(line);
  E.dirty++;
}

static void editorLineAppendString(RopeLine *line, const char *s, int len)
{
  line->root = ropeInsertString(line->root, line->total_len, s, len);
  line->total_len += len;
  line->dirty_render = 1;
  editorUpdateRow(line);
  E.dirty++;
}

static void editorLineDelChar(RopeLine *line, int at)
{
  if (at < 0 || at >= line->total_len)
    return;

  line->root = ropeDelete(line->root, at, 1);
  line->total_len--;
  line->dirty_render = 1;
  editorUpdateRow(line);
  E.dirty++;
}

/*** editor operations ***/

static void editorInsertChar(int c);
static void editorInsertNewline(void);
static void editorDelChar(void);

static void editorInsertChar(int c)
{
  if (E.cy == E.numofrows)
  {
    editorInsertLine(E.numofrows, "", 0);
  }

  recordCommand(CMD_INSERT_CHAR, E.cx, E.cy, c, NULL, 0);

  editorLineInsertChar(&E.lines[E.cy], E.cx, c);
  E.cx++;
}

static void editorInsertNewline(void)
{
  recordCommand(CMD_INSERT_NEWLINE, E.cx, E.cy, 0, NULL, 0);

  if (E.cx == 0)
  {
    editorInsertLine(E.cy, "", 0);
  }
  else
  {
    RopeLine *line = &E.lines[E.cy];

    int right_len = line->total_len - E.cx;
    char *right_str = malloc(right_len + 1);
    for (int i = 0; i < right_len; i++)
    {
      right_str[i] = ropeCharAt(line->root, E.cx + i);
    }
    right_str[right_len] = '\0';

    editorInsertLine(E.cy + 1, right_str, right_len);
    free(right_str);

    line = &E.lines[E.cy];
    line->root = ropeDelete(line->root, E.cx, right_len);
    line->total_len = E.cx;
    line->dirty_render = 1;
    editorUpdateRow(line);
  }

  E.cy++;
  E.cx = 0;
}

static void editorDelChar(void)
{
  if (E.cy == E.numofrows)
    return;
  if (E.cx == 0 && E.cy == 0)
    return;

  RopeLine *line = &E.lines[E.cy];
  if (E.cx > 0)
  {
    char deleted_char = ropeCharAt(line->root, E.cx - 1);
    recordCommand(CMD_DELETE_CHAR, E.cx - 1, E.cy, deleted_char, NULL, 0);

    editorLineDelChar(line, E.cx - 1);
    E.cx--;
  }
  else
  {
    int len = line->total_len;
    char *str = malloc(len + 1);
    for (int i = 0; i < len; i++)
    {
      str[i] = ropeCharAt(line->root, i);
    }
    str[len] = '\0';

    E.cx = E.lines[E.cy - 1].total_len;

    recordCommand(CMD_DELETE_NEWLINE, E.cx, E.cy - 1, 0, str, len);

    char *str_copy = malloc(len + 1);
    memcpy(str_copy, str, len + 1);

    editorLineAppendString(&E.lines[E.cy - 1], str_copy, len);
    free(str_copy);

    editorDelLine(E.cy);
    E.cy--;
  }
}

/*** file i/o ***/

static char *editorLinesToString(int *buflen)
{
  int totlen = 0;
  for (int j = 0; j < E.numofrows; j++)
    totlen += E.lines[j].total_len + 1;

  *buflen = totlen;
  char *buf = malloc(totlen);
  char *p = buf;

  for (int j = 0; j < E.numofrows; j++)
  {
    for (int i = 0; i < E.lines[j].total_len; i++)
    {
      *p++ = ropeCharAt(E.lines[j].root, i);
    }
    *p++ = '\n';
  }

  return buf;
}

void fileOpen(const char *filename)
{
  free(E.filename);
  E.filename = strdup(filename);

  editorSelectSyntaxHighlight();

  FILE *fp = fopen(filename, "r");
  if (!fp)
    die("fopen");

  char *line = NULL;
  size_t linecap = 0;
  ssize_t linelen;

  while ((linelen = getline(&line, &linecap, fp)) != -1)
  {
    while (linelen > 0 && (line[linelen - 1] == '\n' ||
                           line[linelen - 1] == '\r'))
      linelen--;
    editorInsertLine(E.numofrows, line, linelen);
  }

  free(line);
  fclose(fp);
  E.dirty = 0;
}

void editorSave(void)
{
  if (E.filename == NULL)
  {
    E.filename = editorPrompt("Save as: %s (ESC to cancel)", NULL);
    editorSelectSyntaxHighlight();
    if (E.filename == NULL)
    {
      return;
    }
  }

  int len;
  char *buf = editorLinesToString(&len);

  int fd = open(E.filename, O_RDWR | O_CREAT, 0644);
  if (fd != -1)
  {
    if (ftruncate(fd, len) != -1)
    {
      if (write(fd, buf, len) == len)
      {
        close(fd);
        free(buf);
        E.dirty = 0;
        snprintf(E.statusmsg, sizeof(E.statusmsg), "%d bytes written to disk",
                 len);
        E.statusmsg_time = time(NULL);
        return;
      }
    }
    close(fd);
  }

  free(buf);
  snprintf(E.statusmsg, sizeof(E.statusmsg), "Can't save! I/O error: %s",
           strerror(errno));
  E.statusmsg_time = time(NULL);
}

/*** find ***/

static void editorFindCallback(char *query, int key)
{
  static int last_match = -1;
  static int direction = 1;

  static int saved_hl_line = -1;
  static unsigned char *saved_hl = NULL;

  if (saved_hl)
  {
    memcpy(E.lines[saved_hl_line].hl, saved_hl,
           E.lines[saved_hl_line].rsize);
    free(saved_hl);
    saved_hl = NULL;
    saved_hl_line = -1;
  }

  if (key == '\r' || key == '\x1b')
  {
    last_match = -1;
    direction = 1;
    return;
  }
  else if (key == ARIGHT || key == ADOWN)
  {
    direction = 1;
  }
  else if (key == ALEFT || key == AUP)
  {
    direction = -1;
  }
  else
  {
    last_match = -1;
    direction = 1;
  }

  if (last_match == -1)
    direction = 1;
  int current = last_match;

  for (int i = 0; i < E.numofrows; i++)
  {
    current += direction;
    if (current == -1)
      current = E.numofrows - 1;
    else if (current == E.numofrows)
      current = 0;

    RopeLine *line = &E.lines[current];
    char *match = strstr(line->render, query);
    if (match)
    {
      last_match = current;
      E.cy = current;
      E.cx = editorLineRxToCx(line, (int)(match - line->render));
      E.rowoffset = E.numofrows;

      saved_hl_line = current;
      saved_hl = malloc(line->rsize);
      memcpy(saved_hl, line->hl, line->rsize);
      memset(&line->hl[(int)(match - line->render)], HL_MATCH, strlen(query));
      break;
    }
  }
}

void editorFind(void)
{
  int saved_cx = E.cx;
  int saved_cy = E.cy;
  int saved_coloff = E.colloffset;
  int saved_rowoff = E.rowoffset;

  char *query =
      editorPrompt("Search: %s (Use ESC/Arrows/Enter)", editorFindCallback);

  if (query)
  {
    free(query);
  }
  else
  {
    E.cx = saved_cx;
    E.cy = saved_cy;
    E.colloffset = saved_coloff;
    E.rowoffset = saved_rowoff;
  }
}

/*** append buffer ***/

struct abuf
{
  char *b;
  int len;
};

#define ABUF_INIT {NULL, 0}

static void abAppend(struct abuf *ab, const char *s, int len)
{
  char *newb = realloc(ab->b, ab->len + len);

  if (newb == NULL)
    return;
  memcpy(&newb[ab->len], s, len);
  ab->b = newb;
  ab->len += len;
}

static void abFree(struct abuf *ab) { free(ab->b); }

/*** output ***/

static void editorScroll(void)
{
  E.rx = 0;
  if (E.cy < E.numofrows)
  {
    E.rx = editorLineCxToRx(&E.lines[E.cy], E.cx);
  }

  if (E.cy < E.rowoffset)
  {
    E.rowoffset = E.cy;
  }
  if (E.cy >= E.rowoffset + E.screenrows)
  {
    E.rowoffset = E.cy - E.screenrows + 1;
  }
  if (E.rx < E.colloffset)
  {
    E.colloffset = E.rx;
  }
  if (E.rx >= E.colloffset + E.screencols)
  {
    E.colloffset = E.rx - E.screencols + 1;
  }
}

static void drawRows(struct abuf *ab)
{
  for (int y = 0; y < E.screenrows; y++)
  {
    int filerow = y + E.rowoffset;
    if (filerow >= E.numofrows)
    {
      if (E.numofrows == 0 && y == E.screenrows / 3)
      {
        char welcome[80];
        int welcomelen = snprintf(welcome, sizeof(welcome),
                                  "Rope Editor with Undo/Redo -- version 1.0");
        if (welcomelen > E.screencols)
          welcomelen = E.screencols;
        int padding = (E.screencols - welcomelen) / 2;
        if (padding)
        {
          abAppend(ab, "~", 1);
          padding--;
        }
        while (padding--)
          abAppend(ab, " ", 1);
        abAppend(ab, welcome, welcomelen);
      }
      else
      {
        abAppend(ab, "~", 1);
      }
    }
    else
    {
      editorUpdateRow(&E.lines[filerow]);

      int len = E.lines[filerow].rsize - E.colloffset;
      if (len < 0)
        len = 0;
      if (len > E.screencols)
        len = E.screencols;

      char *c = &E.lines[filerow].render[E.colloffset];
      unsigned char *hl = &E.lines[filerow].hl[E.colloffset];
      int current_color = -1;

      for (int j = 0; j < len; j++)
      {
        if (hl[j] == HL_NORMAL)
        {
          if (current_color != -1)
          {
            abAppend(ab, "\x1b[39m", 5);
            current_color = -1;
          }
          abAppend(ab, &c[j], 1);
        }
        else
        {
          int color = editorSyntaxToColor(hl[j]);
          if (color != current_color)
          {
            char buf[16];
            int clen = snprintf(buf, sizeof(buf), "\x1b[%dm", color);
            abAppend(ab, buf, clen);
            current_color = color;
          }
          abAppend(ab, &c[j], 1);
        }
      }
      if (current_color != -1)
        abAppend(ab, "\x1b[39m", 5);
    }

    abAppend(ab, "\x1b[K", 3);
    abAppend(ab, "\r\n", 2);
  }
}

static void editorDrawStatusBar(struct abuf *ab)
{
  abAppend(ab, "\x1b[7m", 4);

  char status[80], rstatus[80];
  int len = snprintf(status, sizeof(status), "%.20s%s - %d lines [ROPE+UNDO]",
                     E.filename ? E.filename : "[No Name]",
                     E.dirty ? " (modified)" : "", E.numofrows);
  int rlen = snprintf(rstatus, sizeof(rstatus), "U:%d/R:%d | %d/%d",
                      undo_stack.size, redo_stack.size, E.cy + 1, E.numofrows);

  if (len > E.screencols)
    len = E.screencols;
  abAppend(ab, status, len);

  while (len < E.screencols)
  {
    if (E.screencols - len == rlen)
    {
      abAppend(ab, rstatus, rlen);
      break;
    }
    else
    {
      abAppend(ab, " ", 1);
      len++;
    }
  }

  abAppend(ab, "\x1b[m", 3);
  abAppend(ab, "\r\n", 2);
}

static void editorDrawMessageBar(struct abuf *ab)
{
  abAppend(ab, "\x1b[K", 3);
  int msglen = (int)strlen(E.statusmsg);
  if (msglen > E.screencols)
    msglen = E.screencols;
  if (msglen && time(NULL) - E.statusmsg_time < 5)
  {
    abAppend(ab, E.statusmsg, msglen);
  }
}

void refreshScreen(void)
{
  editorScroll();

  struct abuf ab = ABUF_INIT;

  abAppend(&ab, "\x1b[?25l", 6);
  abAppend(&ab, "\x1b[H", 3);

  drawRows(&ab);
  editorDrawStatusBar(&ab);
  editorDrawMessageBar(&ab);

  char buf[32];
  snprintf(buf, sizeof(buf), "\x1b[%d;%dH", (E.cy - E.rowoffset) + 1,
           (E.rx - E.colloffset) + 1);
  abAppend(&ab, buf, (int)strlen(buf));

  abAppend(&ab, "\x1b[?25h", 6);

  write(STDOUT_FILENO, ab.b, ab.len);
  abFree(&ab);
}

void editorSetStatusMessage(const char *fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(E.statusmsg, sizeof(E.statusmsg), fmt, ap);
  va_end(ap);
  E.statusmsg_time = time(NULL);
}

/*** input ***/

char *editorPrompt(char *prompt, void (*callback)(char *, int))
{
  size_t bufsize = 128;
  char *buf = malloc(bufsize);

  size_t buflen = 0;
  buf[0] = '\0';

  while (1)
  {
    editorSetStatusMessage(prompt, buf);
    refreshScreen();

    int c = readKey();
    if (c == DKEY || c == CTRL_KEY('h') || c == BACKSPACE)
    {
      if (buflen != 0)
        buf[--buflen] = '\0';
    }
    else if (c == '\x1b')
    {
      editorSetStatusMessage("");
      if (callback)
        callback(buf, c);
      free(buf);
      return NULL;
    }
    else if (c == '\r')
    {
      if (buflen != 0)
      {
        editorSetStatusMessage("");
        if (callback)
          callback(buf, c);
        return buf;
      }
    }
    else if (!iscntrl(c) && c < 128)
    {
      if (buflen == bufsize - 1)
      {
        bufsize *= 2;
        buf = realloc(buf, bufsize);
      }
      buf[buflen++] = (char)c;
      buf[buflen] = '\0';
    }

    if (callback)
      callback(buf, c);
  }
}

static void editorMoveCursor(int key)
{
  RopeLine *line = (E.cy >= E.numofrows) ? NULL : &E.lines[E.cy];

  switch (key)
  {
  case ALEFT:
    if (E.cx != 0)
    {
      E.cx--;
    }
    else if (E.cy > 0)
    {
      E.cy--;
      E.cx = E.lines[E.cy].total_len;
    }
    break;
  case ARIGHT:
    if (line && E.cx < line->total_len)
    {
      E.cx++;
    }
    else if (line && E.cx == line->total_len)
    {
      E.cy++;
      E.cx = 0;
    }
    break;
  case AUP:
    if (E.cy != 0)
      E.cy--;
    break;
  case ADOWN:
    if (E.cy < E.numofrows)
      E.cy++;
    break;
  }

  line = (E.cy >= E.numofrows) ? NULL : &E.lines[E.cy];
  int linelen = line ? line->total_len : 0;
  if (E.cx > linelen)
    E.cx = linelen;
}

void processKeypress(void)
{
  static int quit_times = QUIT_TIMES;

  int c = readKey();

  switch (c)
  {
  case '\r':
    editorInsertNewline();
    break;

  case CTRL_KEY('q'):
    if (E.dirty && quit_times > 0)
    {
      editorSetStatusMessage(
          "WARNING! Unsaved changes. Press Ctrl-Q %d more times to quit.",
          quit_times);
      quit_times--;
      return;
    }
    write(STDOUT_FILENO, "\x1b[2J", 4);
    write(STDOUT_FILENO, "\x1b[H", 3);
    exit(0);
    break;

  case CTRL_KEY('s'):
    editorSave();
    break;

  case CTRL_KEY('z'):
    editorUndo();
    break;

  case CTRL_KEY('y'):
    editorRedo();
    break;

  case CTRL_KEY('f'):
    editorFind();
    break;

  case HKEY:
    E.cx = 0;
    break;

  case EKEY:
    if (E.cy < E.numofrows)
      E.cx = E.lines[E.cy].total_len;
    break;

  case BACKSPACE:
  case CTRL_KEY('h'):
  case DKEY:
    if (c == DKEY)
      editorMoveCursor(ARIGHT);
    editorDelChar();
    break;

  case PAGEUP:
  case PAGEDOWN:
  {
    if (c == PAGEUP)
    {
      E.cy = E.rowoffset;
    }
    else if (c == PAGEDOWN)
    {
      E.cy = E.rowoffset + E.screenrows - 1;
      if (E.cy > E.numofrows)
        E.cy = E.numofrows;
    }

    int times = E.screenrows;
    while (times--)
      editorMoveCursor(c == PAGEUP ? AUP : ADOWN);
  }
  break;

  case AUP:
  case ADOWN:
  case ALEFT:
  case ARIGHT:
    editorMoveCursor(c);
    break;

  case CTRL_KEY('l'):
  case '\x1b':
    break;

  default:
    editorInsertChar(c);
    break;
  }

  quit_times = QUIT_TIMES;
}

void editorUndo(void)
{
  Command *cmd = popCommand(&undo_stack);
  if (cmd == NULL)
  {
    editorSetStatusMessage("Nothing to undo");
    return;
  }

  recording_commands = 0;

  switch (cmd->type)
  {
  case CMD_INSERT_CHAR:
  {
    E.cx = cmd->cx + 1;
    E.cy = cmd->cy;
    editorDelChar();
    E.cx = cmd->cx;
    break;
  }

  case CMD_DELETE_CHAR:
  {
    E.cx = cmd->cx;
    E.cy = cmd->cy;
    editorInsertChar(cmd->c);
    E.cx = cmd->cx;
    break;
  }

  case CMD_INSERT_NEWLINE:
  {
    E.cx = cmd->cx;
    E.cy = cmd->cy + 1;
    if (E.cy < E.numofrows)
    {
      E.cx = 0;
      editorDelChar();
    }
    E.cx = cmd->cx;
    E.cy = cmd->cy;
    break;
  }

  case CMD_DELETE_NEWLINE:
  {
    E.cx = cmd->cx;
    E.cy = cmd->cy;

    if (cmd->deleted_line)
    {
      RopeLine *line = &E.lines[E.cy];
      line->root = ropeDelete(line->root, cmd->cx, cmd->deleted_len);
      line->total_len -= cmd->deleted_len;
      line->dirty_render = 1;
      editorUpdateRow(line);

      editorInsertLine(E.cy + 1, cmd->deleted_line, cmd->deleted_len);
    }
    break;
  }
  }

  recording_commands = 1;
  pushCommand(&redo_stack, cmd);
  editorSetStatusMessage("Undo");
}

void editorRedo(void)
{
  Command *cmd = popCommand(&redo_stack);
  if (cmd == NULL)
  {
    editorSetStatusMessage("Nothing to redo");
    return;
  }

  recording_commands = 0;

  switch (cmd->type)
  {
  case CMD_INSERT_CHAR:
  {
    E.cx = cmd->cx;
    E.cy = cmd->cy;
    editorInsertChar(cmd->c);
    break;
  }

  case CMD_DELETE_CHAR:
  {
    E.cx = cmd->cx + 1;
    E.cy = cmd->cy;
    editorDelChar();
    E.cx = cmd->cx;
    break;
  }

  case CMD_INSERT_NEWLINE:
  {
    E.cx = cmd->cx;
    E.cy = cmd->cy;
    editorInsertNewline();
    break;
  }

  case CMD_DELETE_NEWLINE:
  {
    E.cx = cmd->cx;
    E.cy = cmd->cy - 1;

    if (cmd->deleted_line)
    {
      RopeLine *next_line = &E.lines[E.cy + 1];
      int len = next_line->total_len;
      char *str = malloc(len + 1);
      for (int i = 0; i < len; i++)
      {
        str[i] = ropeCharAt(next_line->root, i);
      }
      str[len] = '\0';

      editorLineAppendString(&E.lines[E.cy], str, len);
      free(str);
      editorDelLine(E.cy + 1);
    }
    break;
  }
  }

  recording_commands = 1;
  pushCommand(&undo_stack, cmd);
  editorSetStatusMessage("Redo");
}

/*** init ***/

void initEditor(void)
{
  E.cx = 0;
  E.cy = 0;
  E.rx = 0;
  E.rowoffset = 0;
  E.colloffset = 0;
  E.numofrows = 0;
  E.lines = NULL;
  E.dirty = 0;
  E.filename = NULL;
  E.statusmsg[0] = '\0';
  E.statusmsg_time = 0;
  E.syntax = NULL;

  if (getWindowSize(&E.screenrows, &E.screencols) == -1)
    die("getWindowSize");
  E.screenrows -= 2;
}
