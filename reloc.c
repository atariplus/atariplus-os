/*
** Compute the relocation offsets of two 650 binary files that differ by $0102
** in their start address.
** (c) 2003 THOR-Software, private use only.
** $Id: reloc.c,v 1.2 2003/05/14 20:52:10 thor Exp $
**
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#define BETTER_ENCODING 0
#define BIT_ENCODING 0

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

/*
** create the relocation data of the two files by analying the
** difference of the two files. This requires that only word
** references are here.
** Returns non-zero for success, prints an error and returns 0.
*/
int createrelocation(const unsigned char *in1,const unsigned char *in2,char *out,int size)
{
  int i;
  int d1,d2;
  int last = 0;
  
  for(i = 0;i<size;i++) {
    if (*in1 == *in2 || last) {
      /* The two data are identical, no relocation required. */
      *out = 0;
      last = 0;
    } else {
      /* We expect this to be the low-byte. Get the high-byte and
      ** check whether the difference is $0102, as it should.
      */
      if (i >= size-2) {
	/* There is none. */
	fprintf(stderr,"invalid relocation data, last byte differs.\n");
	return 0;
      }
      d1 = in1[0] | (((int)in1[1]) << 8);
      d2 = in2[0] | (((int)in2[1]) << 8);
      /*
      ** the difference should be $0102
      */
      if (d2 - d1 != 0x0102) {
	fprintf(stderr,"the address differences of the two files 0x%x is not 0x0102 as requested,\n"
		"or the file is not relocatable at offset 0x%x\n",d2-d1,i);
	return 0;
      }
      *out = 1;
      /*
      ** notice that even though the next byte differs, we do not
      ** need to test it.
      */
      last = 1;
    }
    in1++,in2++,out++;
  }
  return 1;
}

/*
** encode the relocation information: One bit
** for each byte.
*/
#if BETTER_ENCODING==0
#if BIT_ENCODING==0
int writerelocation(FILE *of,const unsigned char *out,int size)
{
  int i;
  int last = 0;

  /*
   * first put the size, the boot code needs it here
   */
  fputc(size & 0xff,of);
  fputc(size >> 8,of);

  for(i=0;i<size;i++) {
    if (*out) {
      int encode = i - last;
      if (encode > 255) {
	fprintf(stderr,"offsets too far separated\n");
	return 0;
      }
      fputc(encode & 0xff,of);
      last = i;
    }
    out++;
  }
  fputc(0,of);
  return 1;
}
#else
int writerelocation(FILE *of,const unsigned char *out,int size)
{
  unsigned char bit = 0;
  int bitcnt = 0;
  int i;
  
  for(i=0;i<size;i++) {
    if (*out) {
      bit |= (1<<bitcnt);
    }
    bitcnt++;
    if (bitcnt >= 8) {
      fputc(bit,of);
      bit = 0;
      bitcnt = 0;
    }
    out++;
  }
  if (bitcnt) {
    fputc(bit,of);
  }
  return 1;
}
#endif
#else
static int bit     = 0;
static int bitcntr = 0;

void putbit(FILE *of,int in)
{
  if (in) {
    bit |= 1<<bitcntr;
  }
  bitcntr++;
  if (bitcntr >= 8) {
    fputc(bit,of);
    bit = 0;
    bitcntr = 0;
  }
}

void flushbits(FILE *of)
{
  if (bitcntr) {
    fputc(bit,of);
  }
}

int writerelocation(FILE *of,const unsigned char *out,int size)
{
  int i;
  int cntr = 0;
  
  for(i=0;i<size;i++) {
    if (*out == 0) {
      /* advance the difference counter */
      cntr++;
    } else {
      int l = cntr - 2; /* l is at least zero, cntr is at least two */
      /*
       * first, write comma bits
       */
      while(l > 0) {
	putbit(of,1);
	l >>= 1;
      }
      l = cntr - 2;
      /*
       * now write the difference, LSB first
       */
      while(l > 0) {
	putbit(of,l & 1);
	l >>= 1;
      }
      cntr = 0;
    }
    out++;
  }
  flushbits(of);
  return 1;
}
#endif

int main(int argc,char **argv)
{
  unsigned char *buf1= NULL,*buf2= NULL;
  unsigned char *out = NULL;
  FILE *in1 = NULL,*in2 = NULL;
  FILE *otf = NULL;
  int size1,size2;
  int rc = 10;

  if (argc != 4) {
    printf("Usage: %s infile1 infile2 outfile\n",argv[0]);
    return 5;
  }

  if (in1 = fopen(argv[1],"rb")) {
    if (in2 = fopen(argv[2],"rb")) {
      size1 = filesize(in1);
      if (size1 >= 0) {
	size2 = filesize(in2);
	if (size2 >= 0) {
	  if (size1 == size2) {
	    buf1 = readfile(in1,size1);
	    if (buf1) {
	      buf2 = readfile(in2,size2);
	      if (buf2) {
		out = malloc(size1);
		if (out) {
		  if (createrelocation(buf1,buf2,out,size1)) {
		    if (otf = fopen(argv[3],"wb")) {
		      if (writerelocation(otf,out,size1)) {
			rc = 0;
			printf("done\n");
		      }
		      fclose(otf);
		    } else {
		      perror("cannot open the output file");
		    }
		  }
		  free(out);
		} else {
		  fprintf(stderr,"out of memory\n");
		}
		free(buf2);
	      }
	      free(buf1);
	    }
	  } else {
	    fprintf(stderr,"the two file sizes are different (%d <> %d),\n"
		    "unable to create relocation data\n",size1,size2);
	  }
	}
      }
      fclose(in2);
    } else {
      perror("cannot open the 2nd input file");
    }
    fclose(in1);
  }
  
  return rc;
}
