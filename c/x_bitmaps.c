#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include "x_bitmaps.h"

#include "bitmaps/face.bm"
#include "bitmaps/rank.bm"
#include "bitmaps/suit.bm"
#include "bitmaps/spade_lg.bm"

static void draw(const char*, int, int, int, int, char*);
static void draw180(const char*, int, int, int, int, char*);
static void drawBox(int, int, int, int, char*);

/* Bitmaps */
const unsigned char* faces[4][3] = {
  {jack_h_bits, queen_h_bits, king_h_bits},
  {jack_d_bits, queen_d_bits, king_d_bits},
  {jack_c_bits, queen_c_bits, king_c_bits},
  {jack_s_bits, queen_s_bits, king_s_bits} };

static const unsigned char reverseByte[0x100] = {
  0x00, 0x80, 0x40, 0xc0, 0x20, 0xa0, 0x60, 0xe0,
  0x10, 0x90, 0x50, 0xd0, 0x30, 0xb0, 0x70, 0xf0,
  0x08, 0x88, 0x48, 0xc8, 0x28, 0xa8, 0x68, 0xe8,
  0x18, 0x98, 0x58, 0xd8, 0x38, 0xb8, 0x78, 0xf8,
  0x04, 0x84, 0x44, 0xc4, 0x24, 0xa4, 0x64, 0xe4,
  0x14, 0x94, 0x54, 0xd4, 0x34, 0xb4, 0x74, 0xf4,
  0x0c, 0x8c, 0x4c, 0xcc, 0x2c, 0xac, 0x6c, 0xec,
  0x1c, 0x9c, 0x5c, 0xdc, 0x3c, 0xbc, 0x7c, 0xfc,
  0x02, 0x82, 0x42, 0xc2, 0x22, 0xa2, 0x62, 0xe2,
  0x12, 0x92, 0x52, 0xd2, 0x32, 0xb2, 0x72, 0xf2,
  0x0a, 0x8a, 0x4a, 0xca, 0x2a, 0xaa, 0x6a, 0xea,
  0x1a, 0x9a, 0x5a, 0xda, 0x3a, 0xba, 0x7a, 0xfa,
  0x06, 0x86, 0x46, 0xc6, 0x26, 0xa6, 0x66, 0xe6,
  0x16, 0x96, 0x56, 0xd6, 0x36, 0xb6, 0x76, 0xf6,
  0x0e, 0x8e, 0x4e, 0xce, 0x2e, 0xae, 0x6e, 0xee,
  0x1e, 0x9e, 0x5e, 0xde, 0x3e, 0xbe, 0x7e, 0xfe,
  0x01, 0x81, 0x41, 0xc1, 0x21, 0xa1, 0x61, 0xe1,
  0x11, 0x91, 0x51, 0xd1, 0x31, 0xb1, 0x71, 0xf1,
  0x09, 0x89, 0x49, 0xc9, 0x29, 0xa9, 0x69, 0xe9,
  0x19, 0x99, 0x59, 0xd9, 0x39, 0xb9, 0x79, 0xf9,
  0x05, 0x85, 0x45, 0xc5, 0x25, 0xa5, 0x65, 0xe5,
  0x15, 0x95, 0x55, 0xd5, 0x35, 0xb5, 0x75, 0xf5,
  0x0d, 0x8d, 0x4d, 0xcd, 0x2d, 0xad, 0x6d, 0xed,
  0x1d, 0x9d, 0x5d, 0xdd, 0x3d, 0xbd, 0x7d, 0xfd,
  0x03, 0x83, 0x43, 0xc3, 0x23, 0xa3, 0x63, 0xe3,
  0x13, 0x93, 0x53, 0xd3, 0x33, 0xb3, 0x73, 0xf3,
  0x0b, 0x8b, 0x4b, 0xcb, 0x2b, 0xab, 0x6b, 0xeb,
  0x1b, 0x9b, 0x5b, 0xdb, 0x3b, 0xbb, 0x7b, 0xfb,
  0x07, 0x87, 0x47, 0xc7, 0x27, 0xa7, 0x67, 0xe7,
  0x17, 0x97, 0x57, 0xd7, 0x37, 0xb7, 0x77, 0xf7,
  0x0f, 0x8f, 0x4f, 0xcf, 0x2f, 0xaf, 0x6f, 0xef,
  0x1f, 0x9f, 0x5f, 0xdf, 0x3f, 0xbf, 0x7f, 0xff
};

static const unsigned char* small_suit[4] = {
  heart_sm_bits, diamond_sm_bits, club_sm_bits, spade_sm_bits};
static const int small_suit_width[4] = {
  heart_sm_width, diamond_sm_width, club_sm_width, spade_sm_width};
static const int small_suit_height[4] = {
  heart_sm_height, diamond_sm_height, club_sm_height, spade_sm_height};
static const unsigned char* main_suit[4] = {
  heart_bits, diamond_bits, club_bits, spade_bits};
static const int suit_width[4] = {
  heart_width, diamond_width, club_width, spade_width};
static const int suit_height[4] = {
  heart_height, diamond_height, club_height, spade_height};

/* Build bitmap of a card */
extern void makeBitmap (suitList s, int v, char * bitmap) {

  /* Erase */
  memset (bitmap, 0, BMWIDTH*(CARDHEIGHT-2));
  if (v <= 9) {
    const char* b = (const char*) main_suit[s];
    int w = suit_width[s];
    int h = suit_height[s];
    if (s == Spade && v == 0) {
      /* Ace of spades */
      b = (char*) spade_lg_bits;
      w = spade_lg_width;
      h = spade_lg_height;
    }
    int n = v+1;
    if (n == 1 || n == 3 || n == 5 || n == 9) {
      /* Center */
      draw (b, (CARDWIDTH-2-w)/2, (CARDHEIGHT-2-h)/2, w, h, bitmap);
    }
    if (n == 2 || n == 3) {
      /* Top/bottom */
      draw (b, (CARDWIDTH-2-w)/2, 15, w, h, bitmap);
      draw180 (b, (CARDWIDTH-2-w)/2, 15, w, h, bitmap);
    }
    if (n > 3) {
      /* Corners */
      draw (b, 15, 15, w, h, bitmap);
      draw (b, CARDWIDTH-2-15-w, 15, w, h, bitmap);
      draw180 (b, 15, 15, w, h, bitmap);
      draw180 (b, CARDWIDTH-2-15-w, 15, w, h, bitmap);
    }
    if (n == 7 || n == 8) {
      /* Top center */
      draw (b, (CARDWIDTH-2-w)/2, 32+(19-h)/2, w, h, bitmap);
    }
    if (n == 8) {
      /* Bottom center */
      draw180 (b, (CARDWIDTH-2-w)/2, 33+(19-h)/2, w, h, bitmap);
    }
    if (n == 10) {
      draw (b, (CARDWIDTH-2-w)/2, 28+(19-h)/2, w, h, bitmap);
      draw180 (b, (CARDWIDTH-2-w)/2, 28+(19-h)/2, w, h, bitmap);
    }
    if (n > 8) {
      /* Center edges */
      int y = 39+(19-h)/2;
      draw (b, 15, y, w, h, bitmap);
      draw (b, CARDWIDTH-2-15-w, y, w, h, bitmap);
      draw180 (b, 15, y, w, h, bitmap);
      draw180(b, CARDWIDTH-2-15-w, y, w, h, bitmap);
    } else if (n >= 6) {
      /* Center edges */
      draw (b, 15, (CARDHEIGHT-2-h)/2, w, h, bitmap);
      draw (b, CARDWIDTH-2-15-w, (CARDHEIGHT-2-h)/2, w, h, bitmap);
    }
  } else {
    /* Face card */
    drawBox ((CARDWIDTH-2-king_c_width)/2-1,
             (CARDHEIGHT-2-king_c_height)/2-1,
             king_c_width+2, king_c_height+2, bitmap);
    draw ((const char*) faces[s][v-10],
          (CARDWIDTH-2-king_c_width)/2,
          (CARDHEIGHT-2-king_c_height)/2,
          king_c_width, king_c_height, bitmap);
  }
  draw ((char*)rank_bits[v], 3, 5, rank_width, rank_height, bitmap);
  draw180 ((char*)rank_bits[v], 3, 5, rank_width, rank_height, bitmap);
  int w = small_suit_width[s];
  int h = small_suit_height[s];
  int x = 0+(15-w)/2;
  int y = 18+(15-h);
  draw ((const char*)small_suit[s], x, y, w, h, bitmap);
  draw180 ((const char*)small_suit[s], x, y, w, h, bitmap);
}

extern void makeOneSymbolBitmap (suitList s, char* bitmap) {
  const char* b = (const char*)main_suit[s];
  int w = suit_width[s];
  int h = suit_height[s];

  memset (bitmap, 0, BMWIDTH*(CARDHEIGHT-2));
  draw (b, (CARDWIDTH-2-w)/2, (CARDHEIGHT-2-h)/2, w, h, bitmap);
}

static void draw (const char* from, int x, int y, int w, int h, char* bitmap) {
  char* to1 = bitmap+y*BMWIDTH+x/8;
  int shift = x%8;
  for (int j = 0; j < h; j++) {
    char* to = to1; to1 += BMWIDTH;
    char wrap = 0;
    for (int i = 0; i < w; i += 8) {
      unsigned char v = *from++;
      *to++ |= (wrap | (v << shift));
      wrap = (v >> (8-shift));
    }
    if (shift) *to |= wrap;
  }
}

static void draw180 (const char* from, int x, int y, int w, int h, char* bitmap) {
  x = CARDWIDTH-3-x;
  y = CARDHEIGHT-3-y;
  char* to1 = bitmap+y*BMWIDTH+x/8;
  int shift = 7-x%8;
  for (int j = 0; j < h; j++) {
    char* to = to1; to1 -= BMWIDTH;
    char wrap = 0;
    for (int i = 0; i < w; i += 8) {
      unsigned char v = reverseByte[(unsigned char)(*from++)];
      *to-- |= (wrap | (v >> shift));
      wrap = (v << (8-shift));
    }
    if (shift) *to |= wrap;
  }
}

static void drawBox (int x, int y, int w, int h, char* bitmap) {
  char* to1 = bitmap+y*BMWIDTH+x/8;
  char* to2 = bitmap+y*BMWIDTH+(x+w-1)/8;
  char v1 = 1 << (x%8);
  char v2 = 1 << ((x+w-1)%8);
  *to1 = *(to1+(h-1)*BMWIDTH) = ~(v1-1);
  *to2 = *(to2+(h-1)*BMWIDTH) = (v2<<1)-1;
  for (char* t = to1+1; t < to2; t++) *t = *(t+(h-1)*BMWIDTH) = (char)255;
  for (int j = 2; j < h; j++) {
    to1 += BMWIDTH; *to1 |= v1;
    to2 += BMWIDTH; *to2 |= v2;
  }
}

