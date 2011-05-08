/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

/* Error codes for IOException. */

#ifndef TART_RUNTIME_ERROR_CODES_H
#define TART_RUNTIME_ERROR_CODES_H

#include "config.h"

// Keep this in sync with IOException.tart
enum {
  IORESULT_SUCCESS = 0,
  IORESULT_UNSPECIFIED,        // Unspecified error

  // Posix error codes
  IORESULT_ENOENT,             // File or directory does not exist.
  IORESULT_EEXIST,             // File or directory already exists.
  IORESULT_EPERM,              // Permission denied for operation.
  IORESULT_EBUSY,              // Device or resource bust
  IORESULT_EIO,                // I/O Error
  IORESULT_ENOSPC,             // No space left on device
};

#endif
