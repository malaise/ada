#include <sys/types.h>
#include <limits.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined PCRE2
#define PCRE2_CODE_UNIT_WIDTH 8
#include <pcre2.h>
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

/* Return current version, e.g. "10.10" */
#ifdef PCRE2
static char version[512];
extern const char * pcre_version (void) {
  (void) pcre2_config (PCRE2_CONFIG_VERSION, version);
  return version;
}

/*************************************************
*      Perl-Compatible Regular Expressions       *
*************************************************/

/* PCRE2 is a library of functions to support regular expressions whose syntax
and semantics are as close as possible to those of the Perl 5 language.

                       Written by Philip Hazel
     Original API code Copyright (c) 1997-2012 University of Cambridge
         New API code Copyright (c) 2016 University of Cambridge

-----------------------------------------------------------------------------
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

    * Neither the name of the University of Cambridge nor the names of its
      contributors may be used to endorse or promote products derived from
      this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-----------------------------------------------------------------------------
*/

/* From pcre2_internal.h */
#define COMPILE_ERROR_BASE 100
typedef struct pcre2_memctl {
  void *    (*malloc)(size_t, void *);
  void      (*free)(void *, void *);
  void      *memory_data;
} pcre2_memctl;

/* From pcre2_intmodedep.h */
#define CODE_BLOCKSIZE_TYPE size_t
typedef struct pcre2_real_code {
  pcre2_memctl memctl;            /* Memory control fields */
  const uint8_t *tables;          /* The character tables */
  void    *executable_jit;        /* Pointer to JIT code */
  uint8_t  start_bitmap[32];      /* Bitmap for starting code unit < 256 */
  CODE_BLOCKSIZE_TYPE blocksize;  /* Total (bytes) that was malloc-ed */
  uint32_t magic_number;          /* Paranoid and endianness check */
  uint32_t compile_options;       /* Options passed to pcre2_compile() */
  uint32_t overall_options;       /* Options after processing the pattern */
  uint32_t flags;                 /* Various state flags */
  uint32_t limit_match;           /* Limit set in the pattern */
  uint32_t limit_recursion;       /* Limit set in the pattern */
  uint32_t first_codeunit;        /* Starting code unit */
  uint32_t last_codeunit;         /* This codeunit must be seen */
  uint16_t bsr_convention;        /* What \R matches */
  uint16_t newline_convention;    /* What is a newline? */
  uint16_t max_lookbehind;        /* Longest lookbehind (characters) */
  uint16_t minlength;             /* Minimum length of match */
  uint16_t top_bracket;           /* Highest numbered group */
  uint16_t top_backref;           /* Highest numbered back reference */
  uint16_t name_entry_size;       /* Size (code units) of table entries */
  uint16_t name_count;            /* Number of name entries in the table */
} pcre2_real_code_8;

typedef struct pcre2_real_match_data {
  pcre2_memctl     memctl;
  const pcre2_real_code *code;    /* The pattern used for the match */
  PCRE2_SPTR8      subject;       /* The subject that was matched */
  PCRE2_SPTR       mark;          /* Pointer to last mark */
  PCRE2_SIZE       leftchar;      /* Offset to leftmost code unit */
  PCRE2_SIZE       rightchar;     /* Offset to rightmost code unit */
  PCRE2_SIZE       startchar;     /* Offset to starting code unit */
  uint16_t         matchedby;     /* Type of match (normal, JIT, DFA) */
  uint16_t         oveccount;     /* Number of pairs */
  int              rc;            /* The return code from the match */
  PCRE2_SIZE       ovector[1];    /* The first field */
} pcre2_real_match_data;



/* Table to translate PCRE2 compile time error codes into POSIX error codes.
Only a few PCRE2 errors with a value greater than 23 turn into special POSIX
codes: most go to REG_BADPAT. The second table lists, in pairs, those that
don't. */
static const int eint1[] = {
  0,           /* No error */
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
  REG_ASSERT,  /* internal error: unexpected repeat */
  REG_BADPAT,  /* unrecognized character after (? or (?- */
  REG_BADPAT,  /* POSIX named classes are supported only within a class */
  REG_BADPAT,  /* POSIX collating elements are not supported */
  REG_EPAREN,  /* missing ) */
  /* 15 */
  REG_ESUBREG, /* reference to non-existent subpattern */
  REG_INVARG,  /* pattern passed as NULL */
  REG_INVARG,  /* unknown compile-time option bit(s) */
  REG_EPAREN,  /* missing ) after (?# comment */
  REG_ESIZE,   /* parentheses nested too deeply */
  /* 20 */
  REG_ESIZE,   /* regular expression too large */
  REG_ESPACE,  /* failed to get memory */
  REG_EPAREN,  /* unmatched closing parenthesis */
  REG_ASSERT   /* internal error: code overflow */
  };

static const int eint2[] = {
  30, REG_ECTYPE,  /* unknown POSIX class name */
  32, REG_INVARG,  /* this version of PCRE2 does not have Unicode support */
  37, REG_EESCAPE, /* PCRE2 does not support \L, \l, \N{name}, \U, or \u */
  56, REG_INVARG,  /* internal error: unknown newline setting */
};

/* Table of texts corresponding to POSIX error codes */
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

extern size_t pcreposix_regerror(int errcode, const regex_t *preg,
                                 char *errbuf, size_t errbuf_size);
extern void pcreposix_regfree(regex_t *preg);
extern int pcreposix_regcomp(regex_t *preg, const char *pattern, int cflags);
extern int pcreposix_regexec(regex_t *preg, const char *string, size_t nmatch,
                             regmatch_t pmatch[], int eflags);
/*************************************************
*          Translate error code to string        *
*************************************************/
extern size_t pcreposix_regerror(int errcode, const regex_t *preg,
                                 char *errbuf, size_t errbuf_size) {
  int used;
  const char *message;

  message = (errcode <= 0 || errcode >= (int)(sizeof(pstring)/sizeof(char *)))?
             "unknown error code" : pstring[errcode];

  if (preg != NULL && (int)preg->re_erroffset != -1) {
    used = snprintf(errbuf, errbuf_size, "%s at offset %-6d", message,
                    (int)preg->re_erroffset);
  } else {
    used = snprintf(errbuf, errbuf_size, "%s", message);
  }

  return used + 1;
}

/*************************************************
*           Free store held by a regex           *
*************************************************/
extern void pcreposix_regfree(regex_t *preg) {
  pcre2_match_data_free(preg->re_match_data);
  pcre2_code_free(preg->re_pcre2_code);
}

/*************************************************
*            Compile a regular expression        *
*************************************************/
/*
Arguments:
  preg        points to a structure for recording the compiled expression
  pattern     the pattern to compile
  cflags      compilation flags

Returns:      0 on success
              various non-zero codes on failure
*/

extern int pcreposix_regcomp(regex_t *preg, const char *pattern, int cflags) {
  PCRE2_SIZE erroffset;
  int errorcode;
  int options = 0;
  int re_nsub = 0;

  if ((cflags & REG_ICASE) != 0)    options |= PCRE2_CASELESS;
  if ((cflags & REG_NEWLINE) != 0)  options |= PCRE2_MULTILINE;
  if ((cflags & REG_DOTALL) != 0)   options |= PCRE2_DOTALL;
  if ((cflags & REG_NOSUB) != 0)    options |= PCRE2_NO_AUTO_CAPTURE;
  if ((cflags & REG_UTF) != 0)      options |= PCRE2_UTF;
  if ((cflags & REG_UCP) != 0)      options |= PCRE2_UCP;
  if ((cflags & REG_UNGREEDY) != 0) options |= PCRE2_UNGREEDY;

  preg->re_pcre2_code = pcre2_compile((PCRE2_SPTR)pattern,
      PCRE2_ZERO_TERMINATED, options, &errorcode, &erroffset, NULL);
  preg->re_erroffset = erroffset;

  if (preg->re_pcre2_code == NULL) {
    unsigned int i;

    /* A negative value is a UTF error; otherwise all error codes are greater
    than COMPILE_ERROR_BASE, but check, just in case. */
    if (errorcode < COMPILE_ERROR_BASE) return REG_BADPAT;
    errorcode -= COMPILE_ERROR_BASE;

    if (errorcode < (int)(sizeof(eint1)/sizeof(const int)))
      return eint1[errorcode];
    for (i = 0; i < sizeof(eint2)/(2*sizeof(const int)); i += 2)
      if (errorcode == eint2[i]) return eint2[i+1];
    return REG_BADPAT;
  }

  (void)pcre2_pattern_info((const pcre2_code *)preg->re_pcre2_code,
    PCRE2_INFO_CAPTURECOUNT, &re_nsub);
  preg->re_nsub = (size_t)re_nsub;
  if ((options & PCRE2_NO_AUTO_CAPTURE) != 0) re_nsub = -1;
  preg->re_match_data = pcre2_match_data_create(re_nsub + 1, NULL);

  if (preg->re_match_data == NULL) {
    pcre2_code_free(preg->re_pcre2_code);
    return REG_ESPACE;
  }

  return 0;
}

/*************************************************
*              Match a regular expression        *
*************************************************/
/* A suitable match_data block, large enough to hold all possible captures, was
obtained when the pattern was compiled, to save having to allocate and free it
for each match. If REG_NOSUB was specified at compile time, the
PCRE_NO_AUTO_CAPTURE flag will be set. When this is the case, the nmatch and
pmatch arguments are ignored, and the only result is yes/no/error. */

extern int pcreposix_regexec(regex_t *preg, const char *string, size_t nmatch,
                             regmatch_t pmatch[], int eflags) {
  int rc, so, eo;
  int options = 0;
  pcre2_match_data *md = (pcre2_match_data *)preg->re_match_data;

  if ((eflags & REG_NOTBOL) != 0) options |= PCRE2_NOTBOL;
  if ((eflags & REG_NOTEOL) != 0) options |= PCRE2_NOTEOL;
  if ((eflags & REG_NOTEMPTY) != 0) options |= PCRE2_NOTEMPTY;

  ((regex_t *)preg)->re_erroffset = (size_t)(-1);  /* Only has meaning after compile */

  /* When no string data is being returned, or no vector has been passed in which
  to put it, ensure that nmatch is zero. */

  if ((((pcre2_real_code_8 *)(preg->re_pcre2_code))->compile_options &
    PCRE2_NO_AUTO_CAPTURE) != 0 || pmatch == NULL) nmatch = 0;

  /* REG_STARTEND is a BSD extension, to allow for non-NUL-terminated strings.
  The man page from OS X says "REG_STARTEND affects only the location of the
  string, not how it is matched". That is why the "so" value is used to bump the
  start location rather than being passed as a PCRE2 "starting offset". */

  if ((eflags & REG_STARTEND) != 0) {
    if (pmatch == NULL) return REG_INVARG;
    so = pmatch[0].rm_so;
    eo = pmatch[0].rm_eo;
  } else {
    so = 0;
    eo = (int)strlen(string);
  }

  rc = pcre2_match((const pcre2_code *)preg->re_pcre2_code,
    (PCRE2_SPTR)string + so, (eo - so), 0, options, md, NULL);

  /* Successful match */
  if (rc >= 0) {
    size_t i;
    if ((size_t)rc > nmatch) rc = (int)nmatch;
    for (i = 0; i < (size_t)rc; i++) {
      pmatch[i].rm_so = md->ovector[i*2];
      pmatch[i].rm_eo = md->ovector[i*2+1];
    }
    for (; i < nmatch; i++) pmatch[i].rm_so = pmatch[i].rm_eo = -1;
    return 0;
  }

  /* Unsuccessful match */
  if (rc <= PCRE2_ERROR_UTF8_ERR1 && rc >= PCRE2_ERROR_UTF8_ERR21)
    return REG_INVARG;

  switch(rc) {
    default: return REG_ASSERT;
    case PCRE2_ERROR_BADMODE: return REG_INVARG;
    case PCRE2_ERROR_BADMAGIC: return REG_INVARG;
    case PCRE2_ERROR_BADOPTION: return REG_INVARG;
    case PCRE2_ERROR_BADUTFOFFSET: return REG_INVARG;
    case PCRE2_ERROR_MATCHLIMIT: return REG_ESPACE;
    case PCRE2_ERROR_NOMATCH: return REG_NOMATCH;
    case PCRE2_ERROR_NOMEMORY: return REG_ESPACE;
    case PCRE2_ERROR_NULL: return REG_INVARG;
  }
}

#define regcomp pcreposix_regcomp
#define regexec pcreposix_regexec
#define regerror pcreposix_regerror
#define regfree pcreposix_regfree

#endif /* PCRE2 */

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

