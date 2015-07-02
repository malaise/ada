#include <sys/types.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined PCRE2
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>
#include <pcre2posix.h>
#elif defined PCRE1
#include <pcre.h>
#include <pcreposix.h>
#else
#include <pcre.h>
#endif

#include "boolean.h"
#include "posix2pcre.h"

/* Mapping PCRE error codes to POSIX error codes */
#ifdef PCRE0
static const int eint[] = {
  0,           /* no error */
  REG_EESCAPE, /* \ at end of pattern */
  REG_EESCAPE, /* \c at end of pattern */
  REG_EESCAPE, /* unrecognized character follows \ */
  REG_BADBR,   /* numbers out of order in {} quantifier */
  /* 5 */
  REG_BADBR,   /* number too big in {} quantifier */
  REG_EBRACK,  /* missing terminating ] for character class */
  REG_ECTYPE,  /* invalid escape sequence in character class */
  REG_ERANGE,  /* range out of order in character class */
  REG_BADRPT,  /* nothing to repeat */
  /* 10 */
  REG_BADRPT,  /* operand of unlimited repeat could match the empty string */
  REG_ASSERT,  /* internal error: unexpected repeat */
  REG_BADPAT,  /* unrecognized character after (? */
  REG_BADPAT,  /* POSIX named classes are supported only within a class */
  REG_EPAREN,  /* missing ) */
  /* 15 */
  REG_ESUBREG, /* reference to non-existent subpattern */
  REG_INVARG,  /* erroffset passed as NULL */
  REG_INVARG,  /* unknown option bit(s) set */
  REG_EPAREN,  /* missing ) after comment */
  REG_ESIZE,   /* parentheses nested too deeply */
  /* 20 */
  REG_ESIZE,   /* regular expression too large */
  REG_ESPACE,  /* failed to get memory */
  REG_EPAREN,  /* unmatched brackets */
  REG_ASSERT,  /* internal error: code overflow */
  REG_BADPAT,  /* unrecognized character after (?< */
  REG_BADPAT,  /* lookbehind assertion is not fixed length */
  REG_BADPAT,  /* malformed number or name after (?( */
  REG_BADPAT,  /* conditional group contains more than two branches */
  REG_BADPAT,  /* assertion expected after (?( */
  REG_BADPAT,  /* (?R or (?[+-]digits must be followed by ) */
  REG_ECTYPE,  /* unknown POSIX class name */
  REG_BADPAT,  /* POSIX collating elements are not supported */
  REG_INVARG,  /* this version of PCRE is not compiled with PCRE_UTF8 support */
  REG_BADPAT,  /* spare error */
  REG_BADPAT,  /* character value in \x{...} sequence is too large */
  REG_BADPAT,  /* invalid condition (?(0) */
  REG_BADPAT,  /* \C not allowed in lookbehind assertion */
  REG_EESCAPE, /* PCRE does not support \L, \l, \N, \U, or \u */
  REG_BADPAT,  /* number after (?C is > 255 */
  REG_BADPAT,  /* closing ) for (?C expected */
  REG_BADPAT,  /* recursive call could loop indefinitely */
  REG_BADPAT,  /* unrecognized character after (?P */
  REG_BADPAT,  /* syntax error in subpattern name (missing terminator) */
  REG_BADPAT,  /* two named subpatterns have the same name */
  REG_BADPAT,  /* invalid UTF-8 string */
  REG_BADPAT,  /* support for \P, \p, and \X has not been compiled */
  REG_BADPAT,  /* malformed \P or \p sequence */
  REG_BADPAT,  /* unknown property name after \P or \p */
  REG_BADPAT,  /* subpattern name is too long (maximum 32 characters) */
  REG_BADPAT,  /* too many named subpatterns (maximum 10,000) */
  REG_BADPAT,  /* repeated subpattern is too long */
  REG_BADPAT,  /* octal value is greater than \377 (not in UTF-8 mode) */
  REG_BADPAT,  /* internal error: overran compiling workspace */
  REG_BADPAT,  /* internal error: previously-checked referenced subpattern not found */
  REG_BADPAT,  /* DEFINE group contains more than one branch */
  REG_BADPAT,  /* repeating a DEFINE group is not allowed */
  REG_INVARG,  /* inconsistent NEWLINE options */
  REG_BADPAT,  /* \g is not followed followed by an (optionally braced) non-zero number */
  REG_BADPAT,  /* (?+ or (?- must be followed by a non-zero number */
  REG_BADPAT,  /* number is too big */
  REG_BADPAT,  /* subpattern name expected */
  REG_BADPAT,  /* digit expected after (?+ */
  REG_BADPAT,  /* ] is an invalid data character in JavaScript compatibility mode */
  REG_BADPAT,  /* different names for subpatterns of the same number are not allowed */
  REG_BADPAT,  /* (*MARK) must have an argument */
  REG_INVARG,  /* this version of PCRE is not compiled with PCRE_UCP support */
  REG_BADPAT,  /* \c must be followed by an ASCII character */
};

/* POSIX error messages */
static const char *const pstring[] = {
  "",                                /* Dummy for value 0 */
  "internal error",                  /* REG_ASSERT */
  "invalid repeat counts in {}",     /* BADBR      */
  "pattern error",                   /* BADPAT     */
  "? * + invalid",                   /* BADRPT     */
  "unbalanced {}",                   /* EBRACE     */
  "unbalanced []",                   /* EBRACK     */
  "collation error - not relevant",  /* ECOLLATE   */
  "bad class",                       /* ECTYPE     */
  "bad escape sequence",             /* EESCAPE    */
  "empty expression",                /* EMPTY      */
  "unbalanced ()",                   /* EPAREN     */
  "bad range inside []",             /* ERANGE     */
  "expression too big",              /* ESIZE      */
  "failed to get memory",            /* ESPACE     */
  "bad back reference",              /* ESUBREG    */
  "bad argument",                    /* INVARG     */
  "match failed"                     /* NOMATCH    */
};

#define POSIX_MALLOC_THRESHOLD 10

/* Compile regex */
extern int regcomp(regex_t *preg, const char *pattern, int cflags) {
  const char *errorptr;
  int erroffset;
  int errorcode;
  int options = 0;

  if ((cflags & REG_ICASE) != 0)   options |= PCRE_CASELESS;
  if ((cflags & REG_NEWLINE) != 0) options |= PCRE_MULTILINE;
  if ((cflags & REG_DOTALL) != 0)  options |= PCRE_DOTALL;
  if ((cflags & REG_NOSUB) != 0)   options |= PCRE_NO_AUTO_CAPTURE;
  if ((cflags & REG_UTF8) != 0)    options |= PCRE_UTF8;
#ifdef PCRE_UCP
  if ((cflags & REG_UCP) != 0)      options |= PCRE_UCP;
#endif
#ifdef PCRE_UNGREEDY
  if ((cflags & REG_UNGREEDY) != 0) options |= PCRE_UNGREEDY;
#endif

  preg->re_pcre = pcre_compile2(pattern, options, &errorcode, &errorptr,
                                &erroffset, NULL);
  preg->re_erroffset = erroffset;

  if (preg->re_pcre == NULL) return eint[errorcode];
  pcre_fullinfo((const pcre *)preg->re_pcre, NULL, PCRE_INFO_CAPTURECOUNT,
                &(preg->re_nsub));
  return 0;
}

/* Execute regex */
extern int regexec(regex_t *preg, const char *string, size_t nmatch,
            regmatch_t pmatch[], int eflags) {
  int rc, so, eo;
  int options = 0;
  int *ovector = NULL;
  int small_ovector[POSIX_MALLOC_THRESHOLD * 3];
  boolean allocated_ovector = FALSE;

  /* Options */
  if ((eflags & REG_NOTBOL)   != 0) options |= PCRE_NOTBOL;
  if ((eflags & REG_NOTEOL)   != 0) options |= PCRE_NOTEOL;
  if ((eflags & REG_NOTEMPTY) != 0) options |= PCRE_NOTEMPTY;

  /* Init out parameters */
  preg->re_erroffset = (size_t)(-1);


  if (nmatch > 0) {
    if (nmatch <= POSIX_MALLOC_THRESHOLD) {
      ovector = &(small_ovector[0]);
    } else {
      if (nmatch > INT_MAX/(sizeof(int) * 3)) return REG_ESPACE;
      ovector = (int *)malloc(sizeof(int) * nmatch * 3);
      if (ovector == NULL) return REG_ESPACE;
      allocated_ovector = TRUE;
    }
  }

  if ((eflags & REG_STARTEND) != 0) {
    so = pmatch[0].rm_so;
    eo = pmatch[0].rm_eo;
  } else {
    so = 0;
    eo = strlen(string);
  }


  rc = pcre_exec((const pcre *)preg->re_pcre, NULL, string + so, (eo - so),
                 0, options, ovector, nmatch * 3);
  if (rc == 0) rc = nmatch;

  if (rc >= 0) {
    size_t i;
    for (i = 0; i < (size_t)rc; i++) {
      pmatch[i].rm_so = ovector[i*2];
      pmatch[i].rm_eo = ovector[i*2+1];
    }
    if (allocated_ovector) free(ovector);
    for (; i < nmatch; i++) pmatch[i].rm_so = pmatch[i].rm_eo = -1;
    return 0;
  }
  if (allocated_ovector) free(ovector);
  switch(rc) {
    case PCRE_ERROR_NOMATCH: return REG_NOMATCH;
    case PCRE_ERROR_NULL: return REG_INVARG;
    case PCRE_ERROR_BADOPTION: return REG_INVARG;
    case PCRE_ERROR_BADMAGIC: return REG_INVARG;
    case PCRE_ERROR_UNKNOWN_NODE: return REG_ASSERT;
    case PCRE_ERROR_NOMEMORY: return REG_ESPACE;
    case PCRE_ERROR_MATCHLIMIT: return REG_ESPACE;
    case PCRE_ERROR_BADUTF8: return REG_INVARG;
    case PCRE_ERROR_BADUTF8_OFFSET: return REG_INVARG;
    default: return REG_ASSERT;
  }
}

/* Translate error code to message */
extern size_t regerror(int errcode, const regex_t *preg, char *errbuf,
                size_t errbuf_size) {
  const char *message, *addmessage;
  size_t length, addlength;

  /* Message + offset */
  message = (errcode >= (int)(sizeof(pstring)/sizeof(char *))) ?
             "unknown error code"
           : pstring[errcode]; length = strlen(message) + 1;

  addmessage = " at offset ";
  addlength = (preg != NULL && (int)preg->re_erroffset != -1) ?
               strlen(addmessage) + 6
             : 0;

  /* Copy */
  if (errbuf_size > 0) {
    if (addlength > 0 && errbuf_size >= length + addlength) {
      sprintf(errbuf, "%s%s%-6d", message, addmessage, (int)preg->re_erroffset);
    } else {
      strncpy(errbuf, message, errbuf_size - 1);
      errbuf[errbuf_size-1] = 0;
    }
  }

  return length + addlength;
}

/* Free regex */
extern void regfree(regex_t *preg) {
  pcre_free(preg->re_pcre);
}
#endif /* PCRE0 */

/* POSIX2PCRE API */
extern int posix2pcre_regcomp(regex_t *preg, const char *pattern, int cflags) {
  return regcomp(preg, pattern, cflags);
}
extern int posix2pcre_regexec(regex_t *preg, const char *string, size_t nmatch,
            regmatch_t pmatch[], int eflags) {
  return regexec(preg, string, nmatch, pmatch, eflags);
}
extern size_t posix2pcre_regerror(int errcode, const regex_t *preg, char *errbuf,
                size_t errbuf_size) {
  return regerror(errcode, preg, errbuf, errbuf_size);
}
extern void posix2pcre_regfree(regex_t *preg) {
  regfree(preg);
}


/* Return current version, e.g. "10.10" */
#ifdef PCRE2
static char version[512];
extern const char * pcre_version (void) {
  sprintf (version, "%d.%d", PCRE2_MAJOR, PCRE2_MINOR);
  return version;
}
#endif /* PCRE2 */

/* Memory management */
extern void * malloc_regex (void) {
  void * ptr;
  ptr = malloc (sizeof(regex_t));
  memset (ptr, 0, sizeof(regex_t));
  return ptr;
}

extern void free_regex (void *ptr) {
  if (ptr != NULL) free (ptr);
}

