/*
** Create a binary load file from a memory dump, set its load and run address.
** (c) 2013 THOR-Software, private use only.
** $Id: createbinfile.c,v 1.3 2013-04-07 20:04:16 thor Exp $
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

/*
** Compute the size of the file passed in,
** return the size of the file.
*/
int filesize(FILE *fp)
{
  int size = -1;
  
  if (fseek(fp,0,SEEK_END) >= 0) {
    size = ftell(fp);
    if (size >= 0) {
      if (fseek(fp,0,SEEK_SET) >= 0) {
	return size;
      }
    }
  }
  perror("unable to seek");
  return -1;
}

/*
** read the file contents of the given size
** into a block that is to be allocated here,
** return the memory pointer, return NULL on
** error.
*/
unsigned char *readfile(FILE *in,int itssize)
{
  char *buf;

  buf = malloc(itssize);
  if (buf) {
    if (fread(buf,1,itssize,in) == itssize) {
      return buf;
    }
    perror("failed reading the file");
    free(buf);
    buf = NULL;
  } else {
    fprintf(stderr,"out of memory\n");
  }
  return NULL;
}

int main(int argc,char **argv)
{
  unsigned char *buf = NULL;
  unsigned char *out = NULL;
  FILE *inf = NULL;
  FILE *otf = NULL;
  int size;
  int start,run;
  int rc = 10;
  int hdr = 1;
  int init = 0;

  if (argc > 1 && !strcmp(argv[1],"-n")) {
    hdr = 0;
    argc--;
    argv++;
  }

  if (argc > 1 && !strcmp(argv[1],"-i")) {
    init = 1;
    argc--;
    argv++;
  }
  
  if (argc != 5 && argc != 4) {
    printf("Usage: %s [-n] [-i] infile1 outfile startaddress [runaddress]\n",argv[0]);
    return 5;
  }

  start = strtol(argv[3],NULL,0);
  if (argc == 5)
    run = strtol(argv[4],NULL,0);

  if (inf = fopen(argv[1],"rb")) {
    size = filesize(inf);
    if (size >= 0) {
      buf = readfile(inf,size);
      if (otf = fopen(argv[2],"wb")) {
	if (hdr) {
	  fputc(0xff,otf);
	  fputc(0xff,otf);
	}
	fputc(start & 0xff,otf);
	fputc(start >> 8  ,otf);
	fputc((start + size - 1) & 0xff,otf);
	fputc((start + size - 1) >> 8  ,otf);
	fwrite(buf,sizeof(char),size,otf);
	if (argc == 5) {
	  if (init) {
	     fputc(0xe2,otf);
	     fputc(0x02,otf);
	     fputc(0xe3,otf);
	     fputc(0x02,otf);
	  } else {
	    fputc(0xe0,otf);
	    fputc(0x02,otf);
	    fputc(0xe1,otf);
	    fputc(0x02,otf);
	  }
	  fputc(run & 0xff,otf);
	  fputc(run >> 8  ,otf);
	}
	fclose(otf);
	rc = 0;
      }
      free(buf);
    }
    fclose(inf);
  } else {
    perror("unable to open the input file");
  }
  
  return rc;
}
