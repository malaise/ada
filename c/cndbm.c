#ifdef Linux
#  include <gdbm/ndbm.h>
#else
#  include <ndbm.h>
#endif

int c_dbm_store (DBM *db, datum *key, datum *content,
                 int store_mode) {
  return dbm_store(db, *key, *content, store_mode);
}

void c_dbm_fetch (DBM *db, datum *key, datum *content) {
  *content = dbm_fetch(db, *key);
}

int c_dbm_delete (DBM *db, datum *key) {
  return dbm_delete(db, *key);
}

void c_dbm_firstkey (DBM *db, datum *key) {
  *key = dbm_firstkey(db);
}

void c_dbm_nextkey (DBM *db, datum *key) {
  *key = dbm_nextkey(db);
}
