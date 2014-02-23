/*
** checksum.c:
** recompute the os rom checksum of the input file
** $Id: checksum.c,v 1.1 2003-04-03 16:44:37 thor Exp $
*/

#include <stdio.h>
#include <stdlib.h>

unsigned short SumArea(unsigned char *image,unsigned long lo,unsigned long hi)
{
  unsigned short sum = 0;
  unsigned char *adr;
  
  adr = image + lo - 0xc000;
  
  do {
    sum += *adr++;
    lo++;
  } while(lo<hi);
  
  return sum;
}

void SumImage(unsigned char *image)
{
  unsigned short sum;
  
  sum  = SumArea(image,0xc002,0xd000);
  sum += SumArea(image,0xd000,0xd800);
  sum += SumArea(image,0xd800,0xe000);
  image[0xc000 - 0xc000] = sum & 0xff;
  image[0xc001 - 0xc000] = sum >> 8;
  //
  sum  = SumArea(image,0xe000,0xfff8);
  sum += SumArea(image,0xfffa,0x10000);
  image[0xfff8 - 0xc000] = sum & 0xff;
  image[0xfff9 - 0xc000] = sum >> 8;
}

int main(int argc,char **argv)
{
  int rc = 10;
  char *image;
  FILE *in,*out;

  if (argc == 2) {
    image = malloc(16384);
    if (image) {
      in = fopen(argv[1],"rb");
      if (in) {
	if (fread(image,1,16384,in) == 16384) {
	  fclose(in);
	  SumImage(image);
	  out = fopen(argv[1],"wb");
	  if (out) {
	    if (fwrite(image,1,16384,out) == 16384) {
	      rc = 0;
	    } else {
	      perror("failed to write output file");
	    }
	    fclose(out);
	  } else {
	    perror("failed to open output file");
	  }
	} else {
	  fclose(in);
	  perror("failed to read input file");
	}
      } else {
	perror("failed to open input file");
      }
    } else {
      fprintf(stderr,"failed due to out of memory\n");
    }
  } else {
    fprintf(stderr,"Usage: checksum romimage\n");
    rc = 5;
  }

  return rc;
}
