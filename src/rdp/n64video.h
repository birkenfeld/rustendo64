/*
  For copyright information, look at n64video.c.
*/

#include <stdint.h>

#define debug printf
#define RDP_LOGFILE "target/rdp.log"

#ifdef __GNUC__
#define n64v_cold __attribute__((cold))
#define likely(expr) __builtin_expect(!!(expr), !0)
#define unlikely(expr) __builtin_expect(!!(expr), 0)
#else
#define n64v_cold
#define likely(expr) expr
#define unlikely(expr) expr
#endif

#if 0
// This would be relevant if we stored big-endian in a byte-wise RAM buffer,
// but in fact we use a word-wise little-endian buffer.

#define byteswap_16(x) ((uint16_t) (((uint8_t) (x >> 8)) | ((uint16_t) (x << 8))))

static inline uint32_t byteswap_32(uint32_t word) {
#ifdef BIG_ENDIAN_HOST
  return word;
#elif defined(_MSC_VER)
  return _byteswap_ulong(word);
#elif defined(__GNUC__)
  return __builtin_bswap32(word);
#else
  return
  (((((word) >> 24) & 0x000000FF) | \
    (((word) >>  8) & 0x0000FF00) | \
    (((word) <<  8) & 0x00FF0000) | \
    (((word) << 24) & 0xFF000000));
#endif
}

#else
#define byteswap_16(x) (x)
#define byteswap_32(x) (x)
#endif

#undef WORD_ADDR_XOR
#define LSB_FIRST 1
#ifdef LSB_FIRST
        #define BYTE_ADDR_XOR           3
        #define WORD_ADDR_XOR           1
        #define BYTE4_XOR_BE(a)         ((a) ^ 3)
#else
        #define BYTE_ADDR_XOR           0
        #define WORD_ADDR_XOR           0
        #define BYTE4_XOR_BE(a)         (a)
#endif

#ifdef LSB_FIRST
#define BYTE_XOR_DWORD_SWAP 7
#define WORD_XOR_DWORD_SWAP 3
#else
#define BYTE_XOR_DWORD_SWAP 4
#define WORD_XOR_DWORD_SWAP 2
#endif
#define DWORD_XOR_DWORD_SWAP 1

#define MI_INTR_DP                      5

#define DP_STATUS_XBUS_DMA              0x01
#define DP_STATUS_FREEZE                0x02
#define DP_STATUS_FLUSH                 0x04
#define DP_STATUS_START_GCLK            0x008
#define DP_STATUS_TMEM_BUSY             0x010
#define DP_STATUS_PIPE_BUSY             0x020
#define DP_STATUS_CMD_BUSY              0x040
#define DP_STATUS_CBUF_READY            0x080
#define DP_STATUS_DMA_BUSY              0x100
#define DP_STATUS_END_VALID             0x200
#define DP_STATUS_START_VALID           0x400

#define PIXEL_SIZE_4BIT                 0
#define PIXEL_SIZE_8BIT                 1
#define PIXEL_SIZE_16BIT                2
#define PIXEL_SIZE_32BIT                3

#define CYCLE_TYPE_1                    0
#define CYCLE_TYPE_2                    1
#define CYCLE_TYPE_COPY                 2
#define CYCLE_TYPE_FILL                 3

#define FORMAT_RGBA                     0
#define FORMAT_YUV                      1
#define FORMAT_CI                       2
#define FORMAT_IA                       3
#define FORMAT_I                        4

#define TEXEL_RGBA4                     0
#define TEXEL_RGBA8                     1
#define TEXEL_RGBA16                    2
#define TEXEL_RGBA32                    3
#define TEXEL_YUV4                      4
#define TEXEL_YUV8                      5
#define TEXEL_YUV16                     6
#define TEXEL_YUV32                     7
#define TEXEL_CI4                       8
#define TEXEL_CI8                       9
#define TEXEL_CI16                      0xa
#define TEXEL_CI32                      0xb
#define TEXEL_IA4                       0xc
#define TEXEL_IA8                       0xd
#define TEXEL_IA16                      0xe
#define TEXEL_IA32                      0xf
#define TEXEL_I4                        0x10
#define TEXEL_I8                        0x11
#define TEXEL_I16                       0x12
#define TEXEL_I32                       0x13

#define CVG_CLAMP                       0
#define CVG_WRAP                        1
#define CVG_ZAP                         2
#define CVG_SAVE                        3

#define ZMODE_OPAQUE                    0
#define ZMODE_INTERPENETRATING          1
#define ZMODE_TRANSPARENT               2
#define ZMODE_DECAL                     3

#define SIGN16(x)       ((int16_t)(x))
#define SIGN8(x)        ((int8_t)(x))

#define SIGN(x, numb)   (((x) & ((1 << numb) - 1)) | -((x) & (1 << (numb - 1))))
#define SIGNF(x, numb)  ((x) | -((x) & (1 << (numb - 1))))

#define GET_LOW(x)      (((x) & 0x3e) << 2)
#define GET_MED(x)      (((x) & 0x7c0) >> 3)
#define GET_HI(x)       (((x) >> 8) & 0xf8)

#define GET_LOW_RGBA16_TMEM(x)  (replicated_rgba[((x) >> 1) & 0x1f])
#define GET_MED_RGBA16_TMEM(x)  (replicated_rgba[((x) >> 6) & 0x1f])
#define GET_HI_RGBA16_TMEM(x)   (replicated_rgba[(x) >> 11])

#define PIXELS_TO_BYTES(pix, siz) (((pix) << (siz)) >> 1)

typedef struct
{
    int lx, rx;
    int unscrx;
    int validline;
    int32_t r, g, b, a, s, t, w, z;
    int32_t majorx[4];
    int32_t minorx[4];
    int32_t invalyscan[4];
} SPAN;

typedef struct {
    int32_t r, g, b, a;
} COLOR;

typedef struct {
    uint8_t r, g, b;
} FBCOLOR;

typedef struct {
    uint8_t r, g, b, cvg;
} CCVG;

typedef struct {
    uint16_t xl, yl, xh, yh;
} RECTANGLE;

typedef struct {
    int tilenum;
    uint16_t xl, yl, xh, yh;
    int16_t s, t;
    int16_t dsdx, dtdy;
    uint32_t flip;
} TEX_RECTANGLE;

typedef struct {
    int clampdiffs, clampdifft;
    int clampens, clampent;
    int masksclamped, masktclamped;
    int notlutswitch, tlutswitch;
} FAKETILE;

typedef struct {
    int format;
    int size;
    int line;
    int tmem;
    int palette;
    int ct, mt, cs, ms;
    int mask_t, shift_t, mask_s, shift_s;

    uint16_t sl, tl, sh, th;

    FAKETILE f;
} TILE;

typedef struct {
    int sub_a_rgb0;
    int sub_b_rgb0;
    int mul_rgb0;
    int add_rgb0;
    int sub_a_a0;
    int sub_b_a0;
    int mul_a0;
    int add_a0;

    int sub_a_rgb1;
    int sub_b_rgb1;
    int mul_rgb1;
    int add_rgb1;
    int sub_a_a1;
    int sub_b_a1;
    int mul_a1;
    int add_a1;
} COMBINE_MODES;

typedef struct {
    int stalederivs;
    int dolod;
    int partialreject_1cycle;
    int partialreject_2cycle;
    int special_bsel0;
    int special_bsel1;
    int rgb_alpha_dither;
    int realblendershiftersneeded;
    int interpixelblendershiftersneeded;
} MODEDERIVS;

typedef struct {
    int cycle_type;
    int persp_tex_en;
    int detail_tex_en;
    int sharpen_tex_en;
    int tex_lod_en;
    int en_tlut;
    int tlut_type;
    int sample_type;
    int mid_texel;
    int bi_lerp0;
    int bi_lerp1;
    int convert_one;
    int key_en;
    int rgb_dither_sel;
    int alpha_dither_sel;
    int blend_m1a_0;
    int blend_m1a_1;
    int blend_m1b_0;
    int blend_m1b_1;
    int blend_m2a_0;
    int blend_m2a_1;
    int blend_m2b_0;
    int blend_m2b_1;
    int force_blend;
    int alpha_cvg_select;
    int cvg_times_alpha;
    int z_mode;
    int cvg_dest;
    int color_on_cvg;
    int image_read_en;
    int z_update_en;
    int z_compare_en;
    int antialias_en;
    int z_source_sel;
    int dither_alpha_en;
    int alpha_compare_en;
    MODEDERIVS f;
} OTHER_MODES;

typedef struct{
    int startspan;
    int endspan;
    int preendspan;
    int nextspan;
    int midspan;
    int longspan;
    int onelessthanmid;
} SPANSIGS;

typedef struct{
    uint8_t cvg;
    uint8_t cvbit;
    uint8_t xoff;
    uint8_t yoff;
} CVtcmaskDERIVATIVE;

struct onetime {
    int ntscnolerp, copymstrangecrashes, fillmcrashes, fillmbitcrashes, syncfullcrash;
} onetimewarnings;
