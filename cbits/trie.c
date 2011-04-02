#include <stdio.h>
#include <stdint.h>
#include <Judy.h>
#include <string.h>

/* Maximum length of keys to autocomplete. */
#define MAX_LEN 1023

static Pvoid_t trie = NULL;
static uint8_t Index[MAX_LEN+1];

#define TO_INDEX(str, len) do {                 \
    memcpy(Index, str, len);                    \
    Index[len] = '\0';                          \
  } while (0);

/* Insert a non-null-terminated string and a value the size of a
   machine word. Return 0 on success. */
int insert(char *string, int len, int value) {
  Word_t *pvalue;
  TO_INDEX(string, len);
  JSLI(pvalue, trie, Index);
  if (pvalue == NULL) return -1;
  *pvalue = (Word_t)value;
  return 0;
}

/* Delete a string. Return 0 if it was found and deleted, nonzero
   otherwise, e.g. if the string wasn't there to start with. */
int delete(char *string, int len) {
  int rc_int;
  TO_INDEX(string, len);
  JSLD(rc_int, trie, Index);
  return !rc_int;
}

/* Start an autocomplete search. Returns value pointer or NULL. */
int *ac_start(char *string, int len) {
  Word_t *pvalue;
  TO_INDEX(string, len);
  JSLF(pvalue, trie, Index);
  return (int *)pvalue;
}

/* Continue an autocomplete search. Returns value pointer or NULL. */
int *ac_next(void) {
  Word_t *pvalue;
  JSLN(pvalue, trie, Index);
  return (int *)pvalue;
}

/* Get a pointer to index iff it's a prefix of string, or NULL
   otherwise. */
char *get_index(char *string, int len) {
  if (memcmp(Index, string, len) == 0)
    return (char *)Index;
  else
    return NULL;
}
