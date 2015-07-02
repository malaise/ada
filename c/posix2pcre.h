/* PCRE0: Before PCRE V7.9, some constants (ex: REG_NOTEMPTY) where not     */
/*   supported so we need to re-define the whole pcreposix in this case     */
/* PCRE1: PCRE V7.9 and after, but before PCRE2 (before V10.00), we can use */
/*   pcreposix and pcre_version                                             */
/* PCRE2: PCRE2 (V10.00 and after) we use pcreposix but need to define      */
/*   pcre_version                                                           */

#ifdef PCRE0
/* Options */
#define REG_ICASE     0x0001
#define REG_NEWLINE   0x0002
#define REG_NOTBOL    0x0004
#define REG_NOTEOL    0x0008
#define REG_DOTALL    0x0010
#define REG_NOSUB     0x0020
#define REG_UTF8      0x0040
#define REG_STARTEND  0x0080
#define REG_NOTEMPTY  0x0100
#define REG_UNGREEDY  0x0200
#define REG_UCP       0x0400


/* Error codes */
enum {
  REG_ASSERT = 1,  /* internal error ? */
  REG_BADBR,       /* invalid repeat counts in {} */
  REG_BADPAT,      /* pattern error */
  REG_BADRPT,      /* ? * + invalid */
  REG_EBRACE,      /* unbalanced {} */
  REG_EBRACK,      /* unbalanced [] */
  REG_ECOLLATE,    /* collation error - not relevant */
  REG_ECTYPE,      /* bad class */
  REG_EESCAPE,     /* bad escape sequence */
  REG_EMPTY,       /* empty expression */
  REG_EPAREN,      /* unbalanced () */
  REG_ERANGE,      /* bad range inside [] */
  REG_ESIZE,       /* expression too big */
  REG_ESPACE,      /* failed to get memory */
  REG_ESUBREG,     /* bad back reference */
  REG_INVARG,      /* bad argument */
  REG_NOMATCH      /* match failed */
};

/* Compiled Regex */
typedef struct {
  void *re_pcre;
  size_t re_nsub;
  size_t re_erroffset;
} regex_t;

/* Matching (sub)strings */
typedef int regoff_t;

typedef struct {
  regoff_t rm_so;
  regoff_t rm_eo;
} regmatch_t;

/* POSIX2 API */
extern int regcomp(regex_t *, const char *, int);
extern int regexec(regex_t *, const char *, size_t, regmatch_t *, int);
extern size_t regerror(int, const regex_t *, char *, size_t);
extern void regfree(regex_t *);
#endif /* PCRE0 */

/* POSIX2PCRE API */
extern int posix2pcre_regcomp(regex_t *, const char *, int);
extern int posix2pcre_regexec(regex_t *, const char *, size_t, regmatch_t *, int);
extern size_t posix2pcre_regerror(int, const regex_t *, char *, size_t);
extern void posix2pcre_regfree(regex_t *);

#ifdef PCRE2
/* No more pcre_version in PCRE2 */
extern const char * pcre_version (void);
#endif /* PCRE2 */

/* Memory management */
extern void * malloc_regex (void);
extern void free_regex (void *);

