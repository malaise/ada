#include "cndbm.h"

extern int c_dbm_store (DBM *db, datum *key, datum *content,
                 int store_mode) {
  return dbm_store(db, *key, *content, store_mode);
}

extern void c_dbm_fetch (DBM *db, datum *key, datum *content) {
  *content = dbm_fetch(db, *key);
}

extern int c_dbm_delete (DBM *db, datum *key) {
  return dbm_delete(db, *key);
}

extern void c_dbm_firstkey (DBM *db, datum *key) {
  *key = dbm_firstkey(db);
}

extern void c_dbm_nextkey (DBM *db, datum *key) {
  *key = dbm_nextkey(db);
}

