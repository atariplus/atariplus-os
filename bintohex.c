/*
** Convert a binary file to a hex dump that is
** compileable by the gcc. This is used to generate
** C code from a binary file. Converts stdin to
** stdout.
*/

#include <stdio.h>

int main(int argc,char **argv)
{
  int c,comma = 0,digits = 16;
  
  printf("{");
  do {
    c = fgetc(stdin);
    if (c == EOF)
      break;    
    if (comma) {
      printf(",");
      comma = 0;
    }
    if (digits >= 16) {
      printf("\n\t");
      digits = 0;
    }
    printf("0x%02x",c);
    digits++;
    comma++;
  } while(1);
  if (digits) {
    printf("\n};\n");
  }
  return 0;
}

