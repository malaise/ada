#ifndef _CNDBM_H_
#define _CNDBM_H_

#ifdef Linux
#  include <gdbm-ndbm.h>
#else
#  include <ndbm.h>
#endif

extern int c_dbm_store (DBM *db, datum *key, datum *content,
                 int store_mode);

extern void c_dbm_fetch (DBM *db, datum *key, datum *content);

extern int c_dbm_delete (DBM *db, datum *key);

void c_dbm_firstkey (DBM *db, datum *key);

void c_dbm_nextkey (DBM *db, datum *key);

#endif

