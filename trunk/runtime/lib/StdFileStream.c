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

enum StreamID {
  STDIN,
  STDOUT,
  STDERR,
};

int32_t translateErrCode(int code) {
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

FILE * get_std_stream(int32_t id) {
  if (id == STDIN) {
    return stdin;
  } else if (id == STDOUT) {
    return stdout;
  } else {
    return stderr;
  }
}

int32_t StdFileStream_read_byte(FILE * fh) {
  return fgetc(fh);
}

//StdFileStream_read_bytes
//StdFileStream_read_chars

int32_t StdFileStream_write_byte(FILE * fh, int b) {
  if (fputc(b, fh) < 0) {
    return translateErrCode(ferror(fh));
  }

  return 0;
}

int32_t StdFileStream_write_bytes(FILE * fh, char * buffer, uint64_t length) {
  if (fwrite(buffer, 1, length, fh) != length) {
    return translateErrCode(ferror(fh));
  }

  return 0;
}

#if 0

  @Extern("StdFileStream_read_bytes")
  def read(buffer:byte[], offset:int = 0, count:int = int.maxVal) -> int;

  @Extern("StdFileStream_read_chars")
  def read(buffer:char[], start:int, length:int) -> int;

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
