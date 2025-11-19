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
*/

#ifndef EDITOR_H
#define EDITOR_H

#include <time.h>
#include <termios.h>

/*** defines ***/

#define TAB_STOP 8
#define QUIT_TIMES 3
#define CTRL_KEY(k) ((k) & 0x1f)
#define ROPE_LEAF_SIZE 512
#define MAX_UNDO_LEVELS 1000

enum editorKey
{
  ALEFT = 1000,
  ARIGHT,
  AUP,
  ADOWN,
  PAGEUP,
  PAGEDOWN,
  HKEY,
  EKEY,
  DKEY
};

#define BACKSPACE 127

// syntax highlight stuff

enum editorHighlight
{
  HL_NORMAL = 0,
  HL_COMMENT,
  HL_MLCOMMENT,
  HL_KEYWORD1,
  HL_KEYWORD2,
  HL_STRING,
  HL_NUMBER,
  HL_MATCH
};

#define HL_HIGHLIGHT_NUMBERS (1 << 0)
#define HL_HIGHLIGHT_STRINGS (1 << 1)

/*** data ***/

typedef struct RopeNode
{
  struct RopeNode *left;
  struct RopeNode *right;
  int weight;
  char *data;
  int len;
  unsigned char *hl;
  int is_leaf;
} RopeNode;

typedef struct
{
  RopeNode *root;
  int total_len;
  char *render;
  int rsize;
  unsigned char *hl;
  int hl_open_comment;
  int dirty_render;
} RopeLine;

/*** undo/redo ***/

enum CommandType
{
  CMD_INSERT_CHAR,
  CMD_DELETE_CHAR,
  CMD_INSERT_NEWLINE,
  CMD_DELETE_NEWLINE
};

typedef struct Command
{
  enum CommandType type;
  int cx, cy;
  int c;
  char *deleted_line;
  int deleted_len;
  struct Command *next;
} Command;

typedef struct CommandStack
{
  Command *top;
  int size;
} CommandStack;

struct editorSyntax
{
  char *filetype;
  char **filematch;
  char **keywords;
  char *singleline_comment_start;
  char *multiline_comment_start;
  char *multiline_comment_end;
  int flags;
};

struct editorConfig
{
  int cx, cy;
  int rx;
  int rowoffset;
  int colloffset;
  int screenrows;
  int screencols;
  int numofrows;
  RopeLine *lines;
  int dirty;
  char *filename;
  char statusmsg[80];
  time_t statusmsg_time;
  struct editorSyntax *syntax;
  struct termios orig_termios;
};

/*** function prototypes ***/

void die(const char *s);
void refreshScreen(void);
void editorSetStatusMessage(const char *fmt, ...);
char *editorPrompt(char *prompt, void (*callback)(char *, int));
void editorSave(void);
void editorFind(void);
void editorSelectSyntaxHighlight(void);
void editorUpdateSyntax(RopeLine *line);
void editorUpdateRow(RopeLine *line);
void pushCommand(CommandStack *stack, Command *cmd);
Command *popCommand(CommandStack *stack);
void clearStack(CommandStack *stack);
void recordCommand(enum CommandType type, int cx, int cy, int c,
                          char *deleted_line, int deleted_len);
void editorUndo(void);
void editorRedo(void);
void initEditor(void);
void rawModeOn(void);
void fileOpen(const char *filename);
void processKeypress(void);

#endif
