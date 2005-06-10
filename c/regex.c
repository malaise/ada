#include <sys/types.h>
#include <regex.h>
#include <stdlib.h>


extern void * malloc_regex (void);
void free_regex (void *ptr);

extern void * malloc_regex (void) {
  return malloc (sizeof(regex_t));
}

void free_regex (void *ptr) {
  if (ptr != NULL) free(ptr);
}

