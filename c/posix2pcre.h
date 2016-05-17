/* PCRE0: Before PCRE V7.9, some constants (ex: REG_NOTEMPTY) where not     */
/*   supported so we need to re-define the whole pcreposix in this case     */
/* PCRE1: PCRE V7.9 and after, but before PCRE2 (before V10.00), we can use */
/*   pcreposix and pcre_version                                             */
/* PCRE2: PCRE2 (V10.00 and after) we need to redefine all pcre2posix API,  */
/*   in order to ensure that POSIX API is not called instead.               */
/*   And we need to define pcre_version (that calls pcre2_config)           */

/* Ensure that size_t is defined */
#include <stdlib.h>

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

#ifdef PCRE2
/* No more pcre_version in PCRE2 */
extern const char * pcre_version (void);

/* Define pcre2posix API */
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

/* Options, mostly defined by POSIX, but with some extras. */
#define REG_ICASE     0x0001  /* Maps to PCRE2_CASELESS */
#define REG_NEWLINE   0x0002  /* Maps to PCRE2_MULTILINE */
#define REG_NOTBOL    0x0004  /* Maps to PCRE2_NOTBOL */
#define REG_NOTEOL    0x0008  /* Maps to PCRE2_NOTEOL */
#define REG_DOTALL    0x0010  /* NOT defined by POSIX; maps to PCRE2_DOTALL */
#define REG_NOSUB     0x0020  /* Maps to PCRE2_NO_AUTO_CAPTURE */
#define REG_UTF       0x0040  /* NOT defined by POSIX; maps to PCRE2_UTF */
#define REG_STARTEND  0x0080  /* BSD feature: pass subject string by so,eo */
#define REG_NOTEMPTY  0x0100  /* NOT defined by POSIX; maps to PCRE2_NOTEMPTY */
#define REG_UNGREEDY  0x0200  /* NOT defined by POSIX; maps to PCRE2_UNGREEDY */
#define REG_UCP       0x0400  /* NOT defined by POSIX; maps to PCRE2_UCP */

/* This is not used by PCRE2, but by defining it we make it easier
to slot PCRE2 into existing programs that make POSIX calls. */
#define REG_EXTENDED  0

/* Error values. Not all these are relevant or used by the wrapper. */
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


/* The structure representing a compiled regular expression. */
typedef struct {
  void *re_pcre2_code;
  void *re_match_data;
  size_t re_nsub;
  size_t re_erroffset;
  int re_cflags;
} regex_t;

/* The structure in which a captured offset is returned. */
typedef int regoff_t;

typedef struct {
  regoff_t rm_so;
  regoff_t rm_eo;
} regmatch_t;

/* The functions */
extern int regcomp(regex_t *, const char *, int);
extern int regexec(regex_t *, const char *, size_t, regmatch_t *, int);
extern size_t regerror(int, const regex_t *, char *, size_t);
extern void regfree(regex_t *);

#endif /* PCRE2 */

/* POSIX2PCRE API */
extern int posix2pcre_regcomp(regex_t *, const char *, int);
extern int posix2pcre_regexec(regex_t *, const char *, size_t, regmatch_t *, int);
extern size_t posix2pcre_regerror(int, const regex_t *, char *, size_t);
extern void posix2pcre_regfree(regex_t *);

/* Memory management */
extern void * malloc_regex (void);
extern void free_regex (void *);

