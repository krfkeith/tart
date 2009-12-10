/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_COMMON_HASH_H
#define TART_COMMON_HASH_H

#include "config.h"

namespace tart {

uint32_t hashBytes(const void * key, int len, uint32_t seed = 0);

}

#endif
