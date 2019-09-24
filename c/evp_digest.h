/* Wrapping to OpenSSL EVP */
#include <openssl/evp.h>

extern void openssl_add_all_digests (void);
extern int evp_max_md_size (void);
extern const EVP_MD * evp_get_digestbyname (char * name);
extern EVP_MD_CTX * evp_md_ctx_create (void);
extern int evp_digestinit_ex (EVP_MD_CTX *ctx, const EVP_MD *type,
                              ENGINE *impl);
extern int evp_digestupdate (EVP_MD_CTX *ctx, const void *d, size_t cnt);
extern int evp_digestfinal_ex (EVP_MD_CTX *ctx, unsigned char *md,
                               unsigned int *s);
extern int evp_md_ctx_cleanup (EVP_MD_CTX *ctx);
extern void evp_md_ctx_destroy (EVP_MD_CTX *ctx);

