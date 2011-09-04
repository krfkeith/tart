/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/* Error codes for IOException. */

#ifndef TART_RUNTIME_ERROR_CODES_H
#define TART_RUNTIME_ERROR_CODES_H

#include "config.h"

#ifndef SUPPORT_DATATYPES_H
#include "llvm/Support/DataTypes.h"
#endif

/* Keep this in sync with IOError.tart. */
enum {
  IORESULT_SUCCESS = 0,
  IORESULT_EOF = -1,            // End of file reached.
  IORESULT_UNSPECIFIED = -2,    // Unspecified error

  // Posix error codes
  IORESULT_ENOENT = -3,         // File or directory does not exist.
  IORESULT_EEXIST = -4,         // File or directory already exists.
  IORESULT_EPERM = -5,          // Permission denied for operation.
  IORESULT_EBUSY = -6,          // Device or resource busy
  IORESULT_EIO = -7,            // I/O Error
  IORESULT_ENOSPC = -8,         // No space left on device
  IORESULT_EBADF = -9,          // Invalid file descriptor
  IORESULT_EINVAL = -10,        // Invalid argument
  IORESULT_ESPIPE = -11,        // Operation not supported by this stream type.
};

/* Function which translates from system error codes to IOError.IOResult. */
ssize_t translateErrCode(int code);

#endif
