/** Standard i/o functions. */

#include "tart_object.h"
#include "tart_string.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void Debug_write(const String * msg) {
  fprintf(stderr, "%.*s", (int32_t) msg->length, msg->start);
}

void Debug_writeLn(const String * msg) {
  fprintf(stderr, "%.*s\n", (int32_t) msg->length, msg->start);
}

void Debug_fail(const String * msg) {
  fprintf(stderr, "%.*s\n", (int32_t) msg->length, msg->start);
  exit(1);
}
