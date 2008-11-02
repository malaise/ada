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

/* POSIX API */
int regcomp(regex_t *, const char *, int);
int regexec(regex_t *, const char *, size_t, regmatch_t *, int);
size_t regerror(int, const regex_t *, char *, size_t);
void regfree(regex_t *);

/* Memory management */
extern void * malloc_regex (void);
void free_regex (void *);

