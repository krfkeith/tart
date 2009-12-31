/** POSIX file handle methods. */

//       int fseek(FILE *stream, long offset, int whence);

#include "error_codes.h"

#if HAVE_STDINT_H
#include <stdint.h>
#endif

#if HAVE_STDBOOL_H
#include <stdbool.h>
#endif

#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#if HAVE_STDIO_H
#include <stdio.h>
#endif

#if HAVE_ERRNO_H
#include <errno.h>
#endif

uint32_t translateErrCode(int code) {
  switch (code) {
    default:
      return IORESULT_UNSPECIFIED;
    case ENOENT: return IORESULT_ENOENT;
    case EEXIST: return IORESULT_EEXIST;
    case EPERM: return IORESULT_EPERM;
    case EBUSY: return IORESULT_EBUSY;
    case EIO: return IORESULT_EIO;
    case ENOSPC: return IORESULT_ENOSPC;
  }
}

#if 0

  @Extern("StdFileStream_read_byte")
  def read -> byte;

  @Extern("StdFileStream_read_bytes")
  def read(buffer:byte[], offset:int = 0, count:int = int.maxVal) -> int;

  @Extern("StdFileStream_read_chars")
  def read(buffer:char[], start:int, length:int) -> int;

  @Extern("StdFileStream_write_byte")
  def write(value:byte) -> void;

  @Extern("StdFileStream_write_bytes")
  def write(buffer:byte[], offset:int = 0, count:int = int.maxVal) -> void;

  def writeChars(chars:char[], start:int = 0, count:int = int.maxVal) -> int {
  }

  def writeString(text:String, start:int = 0, count:int = int.maxVal) -> int {
  }
}
#endif

int32_t StdFileStream_seek(int32_t from, int64_t offset) {
  (void)from;
  (void)offset;
  return 0;
}

bool StdFileStream_atEnd(FILE * fh) {
  return feof(fh) != 0;
}

int64_t StdFileStream_position(FILE * fh) {
  return ftell(fh);
}

int64_t StdFileStream_length(FILE * fh) {
  (void)fh;
  return -1;
}

unsigned StdFileStream_flush(FILE * fh) {
  if (fflush(fh)) {
    return translateErrCode(ferror(fh));
  }

  return 0;
}

unsigned StdFileStream_close(FILE * fh) {
  if (fclose(fh)) {
    return translateErrCode(ferror(fh));
  }

  return 0;
}
