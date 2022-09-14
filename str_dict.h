#pragma once

typedef struct StrDict StrDict;

StrDict *new_str_dict(void);
void *find_str_dict(StrDict *dict, char *name);
void add_str_dict(StrDict *dict, char *name, void *val);
void remove_str_dict(StrDict *dict, char *name);
