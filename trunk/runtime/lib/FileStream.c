/** POSIX file descriptor methods. */

#include "error_codes.h"
#include "tart_string.h"

#if HAVE_STDINT_H
#include <stdint.h>
#endif

#if HAVE_INTTYPES_H
#include <inttypes.h>
#endif

#if HAVE_STDIO_H
#include <stdio.h>
#endif

#if HAVE_UNISTD_H
#include <unistd.h>
#endif

#if HAVE_ERRNO_H
#include <errno.h>
#endif

#if HAVE_FCNTL_H
#include <fcntl.h>
#endif

#if HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#if HAVE_STRING_H
#include <string.h>
#endif

enum SeekFrom {
  IOS_CURRENT = 0,
  IOS_START = 1,
  IOS_END = 2
};

enum FileAccess {
  FA_READ = (1<<0),
  FA_WRITE = (1<<1),
  FA_READWRITE = FA_READ | FA_WRITE,
};

// TODO: Except for the need to read errno and translate into error codes we recognize,
// it would be possible to re-write many of these methods in Tart and call the posix functions
// directly, particularly read() and write().

ssize_t translateErrCode(int code) {
  switch (code) {
    default:
      return IORESULT_UNSPECIFIED;
    case ENOENT: return IORESULT_ENOENT;
    case EEXIST: return IORESULT_EEXIST;
    case EPERM: return IORESULT_EPERM;
    case EBUSY: return IORESULT_EBUSY;
    case EIO: return IORESULT_EIO;
    case ENOSPC: return IORESULT_ENOSPC;
    case EBADF: return IORESULT_EBADF;
    case EINVAL: return IORESULT_EINVAL;
    case ESPIPE: return IORESULT_ESPIPE;
  }
}

ssize_t FileStream_open(const String * path, int access) {
  int flags = 0;
  if ((access & FA_READWRITE) == FA_READWRITE) {
    flags = O_RDWR;
  } else if (access & FA_WRITE) {
    flags = O_WRONLY;
  } else if (access & FA_READ) {
    flags = O_RDONLY;
  }

  char * fname = (char *)malloc(path->length + 1);
  memcpy(fname, path->start, path->length);
  fname[path->length] = 0;
  int fileDesc = open(fname, flags);
  free(fname);
  if (fileDesc < 0) {
    return translateErrCode(errno);
  } else {
    return fileDesc;
  }
}

int32_t FileStream_read_byte(int fileDes) {
  uint8_t byteVal;
  ssize_t result = read(fileDes, &byteVal, 1);
  if (result < 0) {
    return translateErrCode(errno);
  }

  return result == 0 ? IORESULT_EOF : byteVal;
}

ssize_t FileStream_read_bytes(int fileDes, char * buffer, size_t length) {
  int result = read(fileDes, buffer, length);
  if (result < 0) {
    return translateErrCode(errno);
  }

  return result;
}

ssize_t FileStream_write_byte(int fileDes, uint8_t byteVal) {
  if (read(fileDes, &byteVal, 1) < 0) {
    return translateErrCode(errno);
  }

  return IORESULT_SUCCESS;
}

ssize_t FileStream_write_bytes(int fileDes, char * buffer, size_t length) {
  int result = write(fileDes, buffer, length);
  if (result < 0) {
    return translateErrCode(errno);
  }

  return result;
}

int64_t FileStream_seek(int fileDes, int32_t from, int64_t offset) {
  int whence;
  switch (from) {
  case IOS_CURRENT: whence = SEEK_CUR; break;
  case IOS_START: whence = SEEK_SET; break;
  case IOS_END: whence = SEEK_END; break;
  default:
    return IORESULT_UNSPECIFIED;
  }

  off_t pos = lseek(fileDes, offset, whence);
  if (pos == -1) {
    return translateErrCode(errno);
  }
  return pos;
}

ssize_t FileStream_canRead(int fileDes) {
  int status = fcntl(fileDes, F_GETFL);
  if (status < 0) {
    return translateErrCode(errno);
  }

  status &= O_ACCMODE;
  return (status == O_RDONLY || status == O_RDWR) ? 1 : 0;
}

ssize_t FileStream_canWrite(int fileDes) {
  int status = fcntl(fileDes, F_GETFL);
  if (status < 0) {
    return translateErrCode(errno);
  }

  status &= O_ACCMODE;
  return (status == O_WRONLY || status == O_RDWR) ? 1 : 0;
}

ssize_t FileStream_canSeek(int fileDes) {
  off_t pos = lseek (fileDes, 0, SEEK_CUR);
  if (pos == -1) {
    if (errno == ESPIPE) {
      // ESPIPE means that seek isn't supported by this stream type.
      return 0;
    }
    return translateErrCode(errno);
  }
  return 1;
}

int32_t FileStream_isTerminal(int fileDes) {
  return isatty(fileDes) ? 1 : 0;
}

ssize_t FileStream_position(int fileDes) {
  return lseek (fileDes, 0, SEEK_CUR);
}

int64_t FileStream_length(int fileDes) {
  struct stat st;
  if (fstat(fileDes, &st) != 0) {
    return translateErrCode(errno);
  }

  return st.st_size;
}

unsigned FileStream_flush(int fileDes) {
  (void)fileDes;
  return 0;
}

unsigned FileStream_close(int fileDes) {
  if (close(fileDes)) {
    return translateErrCode(errno);
  }
  return 0;
}
