/* Wrapping to OpenSSL EVP */
#include "evp_digest.h"

extern void openssl_add_all_digests (void) {
# if OPENSSL_VERSION_NUMBER < 0x10100000L
  OpenSSL_add_all_digests();
# else
   OPENSSL_init_crypto(OPENSSL_INIT_ADD_ALL_DIGESTS, NULL);
# endif
}

extern int evp_max_md_size (void) {
  return EVP_MAX_MD_SIZE;
}

extern const EVP_MD * evp_get_digestbyname (char * name) {
  return EVP_get_digestbyname(name);
}

extern EVP_MD_CTX * evp_md_ctx_create (void) {
# if OPENSSL_VERSION_NUMBER < 0x10100000L
  return EVP_MD_CTX_create();
# else
  return EVP_MD_CTX_new();
# endif
}

extern int evp_digestinit_ex (EVP_MD_CTX *ctx, const EVP_MD *type,
                              ENGINE *impl) {
  return EVP_DigestInit_ex(ctx, type, impl);
}

extern int evp_digestupdate (EVP_MD_CTX *ctx, const void *d, size_t cnt) {
  return EVP_DigestUpdate(ctx, d, cnt);
}

extern int evp_digestfinal_ex (EVP_MD_CTX *ctx, unsigned char *md,
                               unsigned int *s) {
  return EVP_DigestFinal_ex(ctx, md, s);
}

extern int evp_md_ctx_cleanup (EVP_MD_CTX *ctx) {
# if OPENSSL_VERSION_NUMBER < 0x10100000L
  return EVP_MD_CTX_cleanup(ctx);
# else
  return EVP_MD_CTX_reset(ctx);
# endif
}

extern void evp_md_ctx_destroy (EVP_MD_CTX *ctx) {
# if OPENSSL_VERSION_NUMBER < 0x10100000L
  EVP_MD_CTX_destroy(ctx);
# else
  EVP_MD_CTX_free(ctx);
# endif
}

