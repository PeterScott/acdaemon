#ifndef __TRIE_H
#define __TRIE_H

#include <stdint.h>

int insert(char *string, int len, int value);
int delete(char *string, int len);
int *ac_start(char *string, int len);
int *ac_next(void);
char *get_index(char *string, int len);

#endif
