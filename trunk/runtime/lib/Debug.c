/** Standard i/o functions. */

#include "tart_object.h"
#include "tart_string.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

void Debug_write(const String * msg) {
  fprintf(stderr, "%.*s", msg->length, msg->start);
}

void Debug_writeLn(const String * msg) {
  fprintf(stderr, "%.*s\n", msg->length, msg->start);
}

void Debug_fail(const String * msg) {
  fprintf(stderr, "%.*s\n", msg->length, msg->start);
  exit(1);
}

void Debug_fail2(const String * s0, const String * s1) {
  fprintf(stderr, "%.*s%.*s\n", s0->length, s0->start, s1->length, s1->start);
  exit(1);
}

void Debug_fail3(const String * s0, const String * s1, const String * s2) {
  fprintf(stderr, "%.*s%.*s%.*s\n",
    s0->length, s0->start,
    s1->length, s1->start,
    s2->length, s2->start);
  exit(1);
}

void Debug_fail4(const String * s0, const String * s1, const String * s2, const String * s3) {
  fprintf(stderr, "%.*s%.*s%.*s%.*s\n",
    s0->length, s0->start,
    s1->length, s1->start,
    s2->length, s2->start,
    s3->length, s3->start);
  exit(1);
}

void Debug_fail5(const String * s0, const String * s1, const String * s2,
    const String * s3, const String * s4) {
  fprintf(stderr, "%.*s%.*s%.*s%.*s%.*s\n",
    s0->length, s0->start,
    s1->length, s1->start,
    s2->length, s2->start,
    s3->length, s3->start,
    s4->length, s4->start);
  exit(1);
}
