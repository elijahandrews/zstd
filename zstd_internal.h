/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#ifndef ZSTD1_CCOMMON_H_MODULE
#define ZSTD1_CCOMMON_H_MODULE

/*-*******************************************************
*  Compiler specifics
*********************************************************/
#ifdef _MSC_VER    /* Visual Studio */
#  define FORCE_INLINE static __forceinline
#  include <intrin.h>                    /* For Visual 2005 */
#  pragma warning(disable : 4100)        /* disable: C4100: unreferenced formal parameter */
#  pragma warning(disable : 4127)        /* disable: C4127: conditional expression is constant */
#  pragma warning(disable : 4204)        /* disable: C4204: non-constant aggregate initializer */
#  pragma warning(disable : 4324)        /* disable: C4324: padded structure */
#else
#  if defined (__cplusplus) || defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L   /* C99 */
#    ifdef __GNUC__
#      define FORCE_INLINE static inline __attribute__((always_inline))
#    else
#      define FORCE_INLINE static inline
#    endif
#  else
#    define FORCE_INLINE static
#  endif /* __STDC_VERSION__ */
#endif

#ifdef _MSC_VER
#  define FORCE_NOINLINE static __declspec(noinline)
#else
#  ifdef __GNUC__
#    define FORCE_NOINLINE static __attribute__((__noinline__))
#  else
#    define FORCE_NOINLINE static
#  endif
#endif


/*-*************************************
*  Dependencies
***************************************/
#include "mem.h"
#include "error_private.h"
#define ZSTD1_STATIC_LINKING_ONLY
#include "zstd.h"
#ifndef XXH_STATIC_LINKING_ONLY
#  define XXH_STATIC_LINKING_ONLY  /* XXH64_state_t */
#endif
#include "xxhash.h"                /* XXH_reset, update, digest */


/*-*************************************
*  Debug
***************************************/
#if defined(ZSTD1_DEBUG) && (ZSTD1_DEBUG>=1)
#  include <assert.h>
#else
#  ifndef assert
#    define assert(condition) ((void)0)
#  endif
#endif

#define ZSTD1_STATIC_ASSERT(c) { enum { ZSTD1_static_assert = 1/(int)(!!(c)) }; }

#if defined(ZSTD1_DEBUG) && (ZSTD1_DEBUG>=2)
#  include <stdio.h>
/* recommended values for ZSTD1_DEBUG display levels :
 * 1 : no display, enables assert() only
 * 2 : reserved for currently active debugging path
 * 3 : events once per object lifetime (CCtx, CDict)
 * 4 : events once per frame
 * 5 : events once per block
 * 6 : events once per sequence (*very* verbose) */
#  define DEBUGLOG(l, ...) {                         \
                if (l<=ZSTD1_DEBUG) {                 \
                    fprintf(stderr, __FILE__ ": ");  \
                    fprintf(stderr, __VA_ARGS__);    \
                    fprintf(stderr, " \n");          \
            }   }
#else
#  define DEBUGLOG(l, ...)      {}    /* disabled */
#endif


/*-*************************************
*  shared macros
***************************************/
#undef MIN
#undef MAX
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)>(b) ? (a) : (b))
#define CHECK_F(f) { size_t const errcod = f; if (ERR_isError(errcod)) return errcod; }  /* check and Forward error code */
#define CHECK_E(f, e) { size_t const errcod = f; if (ERR_isError(errcod)) return ERROR(e); }  /* check and send Error code */


/*-*************************************
*  Common constants
***************************************/
#define ZSTD1_OPT_NUM    (1<<12)

#define ZSTD1_REP_NUM      3                 /* number of repcodes */
#define ZSTD1_REP_CHECK    (ZSTD1_REP_NUM)    /* number of repcodes to check by the optimal parser */
#define ZSTD1_REP_MOVE     (ZSTD1_REP_NUM-1)
#define ZSTD1_REP_MOVE_OPT (ZSTD1_REP_NUM)
static const U32 repStartValue[ZSTD1_REP_NUM] = { 1, 4, 8 };

#define KB *(1 <<10)
#define MB *(1 <<20)
#define GB *(1U<<30)

#define BIT7 128
#define BIT6  64
#define BIT5  32
#define BIT4  16
#define BIT1   2
#define BIT0   1

#define ZSTD1_WINDOWLOG_ABSOLUTEMIN 10
static const size_t ZSTD1_fcs_fieldSize[4] = { 0, 2, 4, 8 };
static const size_t ZSTD1_did_fieldSize[4] = { 0, 1, 2, 4 };

#define ZSTD1_BLOCKHEADERSIZE 3   /* C standard doesn't allow `static const` variable to be init using another `static const` variable */
static const size_t ZSTD1_blockHeaderSize = ZSTD1_BLOCKHEADERSIZE;
typedef enum { bt_raw, bt_rle, bt_compressed, bt_reserved } blockType_e;

#define MIN_SEQUENCES_SIZE 1 /* nbSeq==0 */
#define MIN_CBLOCK_SIZE (1 /*litCSize*/ + 1 /* RLE or RAW */ + MIN_SEQUENCES_SIZE /* nbSeq==0 */)   /* for a non-null block */

#define HufLog 12
typedef enum { set_basic, set_rle, set_compressed, set_repeat } symbolEncodingType_e;

#define LONGNBSEQ 0x7F00

#define MINMATCH 3

#define Litbits  8
#define MaxLit ((1<<Litbits) - 1)
#define MaxML  52
#define MaxLL  35
#define MaxOff 28
#define MaxSeq MAX(MaxLL, MaxML)   /* Assumption : MaxOff < MaxLL,MaxML */
#define MLFSE1Log    9
#define LLFSE1Log    9
#define OffFSE1Log   8

static const U32 LL_bits[MaxLL+1] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9,10,11,12,
                                     13,14,15,16 };
static const S16 LL_defaultNorm[MaxLL+1] = { 4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1,
                                             2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1,
                                            -1,-1,-1,-1 };
#define LL_DEFAULTNORMLOG 6  /* for static allocation */
static const U32 LL_defaultNormLog = LL_DEFAULTNORMLOG;

static const U32 ML_bits[MaxML+1] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9,10,11,
                                     12,13,14,15,16 };
static const S16 ML_defaultNorm[MaxML+1] = { 1, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,-1,-1,
                                            -1,-1,-1,-1,-1 };
#define ML_DEFAULTNORMLOG 6  /* for static allocation */
static const U32 ML_defaultNormLog = ML_DEFAULTNORMLOG;

static const S16 OF_defaultNorm[MaxOff+1] = { 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 1, 1, 1,-1,-1,-1,-1,-1 };
#define OF_DEFAULTNORMLOG 5  /* for static allocation */
static const U32 OF_defaultNormLog = OF_DEFAULTNORMLOG;


/*-*******************************************
*  Shared functions to include for inlining
*********************************************/
static void ZSTD1_copy8(void* dst, const void* src) { memcpy(dst, src, 8); }
#define COPY8(d,s) { ZSTD1_copy8(d,s); d+=8; s+=8; }

/*! ZSTD1_wildcopy() :
*   custom version of memcpy(), can copy up to 7 bytes too many (8 bytes if length==0) */
#define WILDCOPY_OVERLENGTH 8
MEM_STATIC void ZSTD1_wildcopy(void* dst, const void* src, ptrdiff_t length)
{
    const BYTE* ip = (const BYTE*)src;
    BYTE* op = (BYTE*)dst;
    BYTE* const oend = op + length;
    do
        COPY8(op, ip)
    while (op < oend);
}

MEM_STATIC void ZSTD1_wildcopy_e(void* dst, const void* src, void* dstEnd)   /* should be faster for decoding, but strangely, not verified on all platform */
{
    const BYTE* ip = (const BYTE*)src;
    BYTE* op = (BYTE*)dst;
    BYTE* const oend = (BYTE*)dstEnd;
    do
        COPY8(op, ip)
    while (op < oend);
}


/*-*******************************************
*  Private interfaces
*********************************************/
typedef struct ZSTD1_stats_s ZSTD1_stats_t;

typedef struct {
    U32 off;
    U32 len;
} ZSTD1_match_t;

typedef struct {
    U32 price;
    U32 off;
    U32 mlen;
    U32 litlen;
    U32 rep[ZSTD1_REP_NUM];
} ZSTD1_optimal_t;


typedef struct seqDef_s {
    U32 offset;
    U16 litLength;
    U16 matchLength;
} seqDef;


typedef struct {
    seqDef* sequencesStart;
    seqDef* sequences;
    BYTE* litStart;
    BYTE* lit;
    BYTE* llCode;
    BYTE* mlCode;
    BYTE* ofCode;
    U32   longLengthID;   /* 0 == no longLength; 1 == Lit.longLength; 2 == Match.longLength; */
    U32   longLengthPos;
    /* opt */
    ZSTD1_optimal_t* priceTable;
    ZSTD1_match_t* matchTable;
    U32* matchLengthFreq;
    U32* litLengthFreq;
    U32* litFreq;
    U32* offCodeFreq;
    U32  matchLengthSum;
    U32  matchSum;
    U32  litLengthSum;
    U32  litSum;
    U32  offCodeSum;
    U32  log2matchLengthSum;
    U32  log2matchSum;
    U32  log2litLengthSum;
    U32  log2litSum;
    U32  log2offCodeSum;
    U32  factor;
    U32  staticPrices;
    U32  cachedPrice;
    U32  cachedLitLength;
    const BYTE* cachedLiterals;
} seqStore_t;

const seqStore_t* ZSTD1_getSeqStore(const ZSTD1_CCtx* ctx);
void ZSTD1_seqToCodes(const seqStore_t* seqStorePtr);

/* custom memory allocation functions */
void* ZSTD1_malloc(size_t size, ZSTD1_customMem customMem);
void* ZSTD1_calloc(size_t size, ZSTD1_customMem customMem);
void ZSTD1_free(void* ptr, ZSTD1_customMem customMem);


/*======  common function  ======*/

MEM_STATIC U32 ZSTD1_highbit32(U32 val)
{
#   if defined(_MSC_VER)   /* Visual */
    unsigned long r=0;
    _BitScanReverse(&r, val);
    return (unsigned)r;
#   elif defined(__GNUC__) && (__GNUC__ >= 3)   /* GCC Intrinsic */
    return 31 - __builtin_clz(val);
#   else   /* Software version */
    static const int DeBruijnClz[32] = { 0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30, 8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31 };
    U32 v = val;
    int r;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    r = DeBruijnClz[(U32)(v * 0x07C4ACDDU) >> 27];
    return r;
#   endif
}


/* hidden functions */

/* ZSTD1_invalidateRepCodes() :
 * ensures next compression will not use repcodes from previous block.
 * Note : only works with regular variant;
 *        do not use with extDict variant ! */
void ZSTD1_invalidateRepCodes(ZSTD1_CCtx* cctx);


/*! ZSTD1_initCStream_internal() :
 *  Private use only. Init streaming operation.
 *  expects params to be valid.
 *  must receive dict, or cdict, or none, but not both.
 *  @return : 0, or an error code */
size_t ZSTD1_initCStream_internal(ZSTD1_CStream* zcs,
                     const void* dict, size_t dictSize,
                     const ZSTD1_CDict* cdict,
                     ZSTD1_parameters params, unsigned long long pledgedSrcSize);

/*! ZSTD1_compressStream_generic() :
 *  Private use only. To be called from zstdmt_compress.c in single-thread mode. */
size_t ZSTD1_compressStream_generic(ZSTD1_CStream* zcs,
                                   ZSTD1_outBuffer* output,
                                   ZSTD1_inBuffer* input,
                                   ZSTD1_EndDirective const flushMode);

/*! ZSTD1_getParamsFromCDict() :
 *  as the name implies */
ZSTD1_parameters ZSTD1_getParamsFromCDict(const ZSTD1_CDict* cdict);


#endif   /* ZSTD1_CCOMMON_H_MODULE */
