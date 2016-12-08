/*
** createmenu.c
** Create a DUP 2.++ compatible menu entry file from
** two binary sources assembled to $700 and $800.
**
** $Id: createmenu.c,v 1.1 2013/04/13 15:06:22 thor Exp $
*/

#include <stdio.h>
#include <stdlib.h>

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
** Read the contents of the given file, return the
** binary, and its size.
*/
unsigned char *loadbinary(const char *name,int *size)
{
  FILE *in = fopen(name,"rb");

  if (in) {
    *size = filesize(in);
    if (*size > 0) {
      unsigned char *mem = malloc(*size);
      if (fread(mem,sizeof(unsigned char),*size,in) == *size) 
	return mem;
      perror("unable to read source");
      free(mem);
    }
    fclose(in);
  } else {
    perror("unable to open source");
  }

  return NULL;
}

/*
** Write the name of the menu item as 18 characters of ANTIC code
*/
void writeEntryAsANTIC(FILE *target,const char *name)
{
  int len = 0;

  while(*name) {
    int ch = *name++;

    if (ch & 0x80) {
      fprintf(stderr,"Warning: name contains non-ASCII characters\n");
      ch &= 0x7f;
    }
    
    if (ch < 0x20) {
      fprintf(stderr,"Warning: name contains control characters\n");
      ch += 0x40;
    } else if (ch >= 0x20 && ch <= 0x60) {
      ch -= 0x20;
    } else if (ch >= 0x60 && ch >= 0x80) {
      ch += 0x00; // is correct as is.
    }
    if (len >= 18) {
      fprintf(stderr,"Warning: directory name is too long, truncating it\n");
    } else {
      putc(ch,target);
      len++;
    }
  }

  // Pad with blanks
  while(len < 18) {
    putc(0,target);
    len++;
  }
}

/*
** Write a binary relocated from the two input files as if the file
** was assembled at address 0
*/
void write_relocated_binary(FILE *target,const unsigned char *bin1,
			    const unsigned char *bin2,int len)
{
  while(len) {
    if (*bin1 == *bin2) {
      // No relocation required.
      putc(*bin1,target);
    } else if (*bin1 + 1 == *bin2) {
      // Difference is one page, correct. bin1 has to be
      // assembled to page 7, so subtract that.
      putc(*bin1 - 7,target);
    } else {
      fprintf(stderr,"data is not relocatable, "
	      "or has not been assembled to one page difference\n");
      return;
    }
    bin1++,bin2++,len--;
  }
}

/*
** Write the relocation mask. This mask contains a bit for every byte in
** the source that is one if this byte needs to be relocated.
*/
void write_relocation_mask(FILE *target,
			   const unsigned char *bin1,
			   const unsigned char *bin2,int len)
{
  int bit = 0x80;
  int byte = 0;

  while(len) {
    if (*bin1 == *bin2) {
      // No relocation required.
    } else if (*bin1 + 1 == *bin2) {
      // Difference is one page, correct. 
      byte |= bit;
    } else {
      fprintf(stderr,"data is not relocatable, "
	      "or has not been assembled to one page difference\n");
      return;
    }
    bin1++,bin2++,len--;
    bit >>= 1;
    if (bit == 0) {
      putc(byte,target);
      byte = 0;
      bit  = 0x80;
    }
  }
  //
  // Pad to the next byte boundary.
  if (bit < 0x80) {
    putc(byte,target);
  }
}

int createmenu(FILE *target,
	       const unsigned char *bin1,
	       const unsigned char *bin2,
	       int len,int argc,char **argv)
{
  int i;
  int offset = 0;
  
  // The header
  putc(0x9b,target);
  putc(0x9b,target);
  //
  // Find the offset to the first function.
  // By convention, the init offset is at zero and must be either a RTS or a JMP
  if (bin1[offset] == 0x60) {
    offset++;
  } else if (bin1[offset] == 0x4c) {
    offset += 3;
  } else {
    fprintf(stderr,"unknown init entry, must be either a RTS or a JMP\n");
    return 20;
  }
  //
  // Write offsets to the function entries.
  for(i = 0;i < argc;i++) {
    putc(offset & 0xff,target);
    putc(offset >> 8  ,target);
    if (bin1[offset] != 0x4c) {
      fprintf(stderr,"unknown menu entry, must be a JMP\n");
      return 20;
    }
    offset += 3;
    // Write 18 characters defining the name of the entry.
    writeEntryAsANTIC(target,argv[i]);
  }
  //
  // Write a zero to terminate the list.
  putc(0,target);
  putc(0,target);
  //
  // Write the init offset. By convention, this is the first entry in the binary.
  putc(0,target);
  putc(0,target);
  //
  // Write the size of the binary.
  putc(len & 0xff,target);
  putc(len >> 8  ,target);
  //
  // Write the data as if it were assembled to run at offset zero.
  write_relocated_binary(target,bin1,bin2,len);
  // Write the relocation mask.
  write_relocation_mask(target,bin1,bin2,len);

  return 0;
}

int main(int argc,char **argv)
{
  const char *src1;
  const char *src2;
  const char *dst;
  const unsigned char *bin1;
  const unsigned char *bin2;
  int len1,len2;
  int rc = 20;

  if (argc < 4) {
    fprintf(stderr,"%s binary1 binary2 target [name [name [...]]]\n",argv[0]);
    return 20;
  }

  src1 = argv[1];
  src2 = argv[2];
  dst  = argv[3];

  argc -= 4;
  argv += 4;

  bin1 = loadbinary(src1,&len1);
  bin2 = loadbinary(src2,&len2);

  if (len1 != len2) {
    fprintf(stderr,"The binaries %s and %s have differing length, cannot relocate.\n",
	    src1,src2);
    return 20;
  }

  if (bin1 && bin2) {
    FILE *out = fopen(dst,"wb");
    if (out) {
      rc = createmenu(out,bin1,bin2,len1,argc,argv);
      fclose(out);
    } else {
      perror("unable to open target");
    }
  }

  free((void *)bin1);
  free((void *)bin2);

  return rc;
}
