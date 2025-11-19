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

#include <unistd.h>
#include <stdlib.h>
#include "editor.h"

/*** main ***/

int main(int argc, char *argv[])
{
  rawModeOn();
  initEditor();

  if (argc >= 2)
  {
    fileOpen(argv[1]);
  }

  editorSetStatusMessage(
      "HELP: Ctrl-S=save | Ctrl-Q=quit | Ctrl-F=find | Ctrl-Z=undo | Ctrl-Y=redo");

  while (1)
  {
    refreshScreen();
    processKeypress();
  }

  return 0;
}
