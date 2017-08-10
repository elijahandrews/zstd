/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */


/*- Dependencies -*/
#include "zstd_v06.h"
#include <stddef.h>    /* size_t, ptrdiff_t */
#include <string.h>    /* memcpy */
#include <stdlib.h>    /* malloc, free, qsort */
#include "error_private.h"



/* ******************************************************************
   mem.h
   low-level memory access routines
   Copyright (C) 2013-2015, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - FSE1 source repository : https://github.com/Cyan4973/FiniteStateEntropy
    - Public forum : https://groups.google.com/forum/#!forum/lz4c
****************************************************************** */
#ifndef MEM_H_MODULE
#define MEM_H_MODULE

#if defined (__cplusplus)
extern "C" {
#endif


/*-****************************************
*  Compiler specifics
******************************************/
#if defined(_MSC_VER)   /* Visual Studio */
#   include <stdlib.h>  /* _byteswap_ulong */
#   include <intrin.h>  /* _byteswap_* */
#endif
#if defined(__GNUC__)
#  define MEM_STATIC static __attribute__((unused))
#elif defined (__cplusplus) || (defined (__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) /* C99 */)
#  define MEM_STATIC static inline
#elif defined(_MSC_VER)
#  define MEM_STATIC static __inline
#else
#  define MEM_STATIC static  /* this version may generate warnings for unused static functions; disable the relevant warning */
#endif


/*-**************************************************************
*  Basic Types
*****************************************************************/
#if  !defined (__VMS) && (defined (__cplusplus) || (defined (__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) /* C99 */) )
# include <stdint.h>
  typedef  uint8_t BYTE;
  typedef uint16_t U16;
  typedef  int16_t S16;
  typedef uint32_t U32;
  typedef  int32_t S32;
  typedef uint64_t U64;
  typedef  int64_t S64;
#else
  typedef unsigned char       BYTE;
  typedef unsigned short      U16;
  typedef   signed short      S16;
  typedef unsigned int        U32;
  typedef   signed int        S32;
  typedef unsigned long long  U64;
  typedef   signed long long  S64;
#endif


/*-**************************************************************
*  Memory I/O
*****************************************************************/
/* MEM_FORCE_MEMORY_ACCESS :
 * By default, access to unaligned memory is controlled by `memcpy()`, which is safe and portable.
 * Unfortunately, on some target/compiler combinations, the generated assembly is sub-optimal.
 * The below switch allow to select different access method for improved performance.
 * Method 0 (default) : use `memcpy()`. Safe and portable.
 * Method 1 : `__packed` statement. It depends on compiler extension (ie, not portable).
 *            This method is safe if your compiler supports it, and *generally* as fast or faster than `memcpy`.
 * Method 2 : direct access. This method is portable but violate C standard.
 *            It can generate buggy code on targets depending on alignment.
 *            In some circumstances, it's the only known way to get the most performance (ie GCC + ARMv6)
 * See http://fastcompression.blogspot.fr/2015/08/accessing-unaligned-memory.html for details.
 * Prefer these methods in priority order (0 > 1 > 2)
 */
#ifndef MEM_FORCE_MEMORY_ACCESS   /* can be defined externally, on command line for example */
#  if defined(__GNUC__) && ( defined(__ARM_ARCH_6__) || defined(__ARM_ARCH_6J__) || defined(__ARM_ARCH_6K__) || defined(__ARM_ARCH_6Z__) || defined(__ARM_ARCH_6ZK__) || defined(__ARM_ARCH_6T2__) )
#    define MEM_FORCE_MEMORY_ACCESS 2
#  elif (defined(__INTEL_COMPILER) && !defined(WIN32)) || \
  (defined(__GNUC__) && ( defined(__ARM_ARCH_7__) || defined(__ARM_ARCH_7A__) || defined(__ARM_ARCH_7R__) || defined(__ARM_ARCH_7M__) || defined(__ARM_ARCH_7S__) ))
#    define MEM_FORCE_MEMORY_ACCESS 1
#  endif
#endif

MEM_STATIC unsigned MEM_32bits(void) { return sizeof(size_t)==4; }
MEM_STATIC unsigned MEM_64bits(void) { return sizeof(size_t)==8; }

MEM_STATIC unsigned MEM_isLittleEndian(void)
{
    const union { U32 u; BYTE c[4]; } one = { 1 };   /* don't use static : performance detrimental  */
    return one.c[0];
}

#if defined(MEM_FORCE_MEMORY_ACCESS) && (MEM_FORCE_MEMORY_ACCESS==2)

/* violates C standard, by lying on structure alignment.
Only use if no other choice to achieve best performance on target platform */
MEM_STATIC U16 MEM_read16(const void* memPtr) { return *(const U16*) memPtr; }
MEM_STATIC U32 MEM_read32(const void* memPtr) { return *(const U32*) memPtr; }
MEM_STATIC U64 MEM_read64(const void* memPtr) { return *(const U64*) memPtr; }

MEM_STATIC void MEM_write16(void* memPtr, U16 value) { *(U16*)memPtr = value; }

#elif defined(MEM_FORCE_MEMORY_ACCESS) && (MEM_FORCE_MEMORY_ACCESS==1)

/* __pack instructions are safer, but compiler specific, hence potentially problematic for some compilers */
/* currently only defined for gcc and icc */
typedef union { U16 u16; U32 u32; U64 u64; size_t st; } __attribute__((packed)) unalign;

MEM_STATIC U16 MEM_read16(const void* ptr) { return ((const unalign*)ptr)->u16; }
MEM_STATIC U32 MEM_read32(const void* ptr) { return ((const unalign*)ptr)->u32; }
MEM_STATIC U64 MEM_read64(const void* ptr) { return ((const unalign*)ptr)->u64; }

MEM_STATIC void MEM_write16(void* memPtr, U16 value) { ((unalign*)memPtr)->u16 = value; }

#else

/* default method, safe and standard.
   can sometimes prove slower */

MEM_STATIC U16 MEM_read16(const void* memPtr)
{
    U16 val; memcpy(&val, memPtr, sizeof(val)); return val;
}

MEM_STATIC U32 MEM_read32(const void* memPtr)
{
    U32 val; memcpy(&val, memPtr, sizeof(val)); return val;
}

MEM_STATIC U64 MEM_read64(const void* memPtr)
{
    U64 val; memcpy(&val, memPtr, sizeof(val)); return val;
}

MEM_STATIC void MEM_write16(void* memPtr, U16 value)
{
    memcpy(memPtr, &value, sizeof(value));
}


#endif /* MEM_FORCE_MEMORY_ACCESS */

MEM_STATIC U32 MEM_swap32(U32 in)
{
#if defined(_MSC_VER)     /* Visual Studio */
    return _byteswap_ulong(in);
#elif defined (__GNUC__)
    return __builtin_bswap32(in);
#else
    return  ((in << 24) & 0xff000000 ) |
            ((in <<  8) & 0x00ff0000 ) |
            ((in >>  8) & 0x0000ff00 ) |
            ((in >> 24) & 0x000000ff );
#endif
}

MEM_STATIC U64 MEM_swap64(U64 in)
{
#if defined(_MSC_VER)     /* Visual Studio */
    return _byteswap_uint64(in);
#elif defined (__GNUC__)
    return __builtin_bswap64(in);
#else
    return  ((in << 56) & 0xff00000000000000ULL) |
            ((in << 40) & 0x00ff000000000000ULL) |
            ((in << 24) & 0x0000ff0000000000ULL) |
            ((in << 8)  & 0x000000ff00000000ULL) |
            ((in >> 8)  & 0x00000000ff000000ULL) |
            ((in >> 24) & 0x0000000000ff0000ULL) |
            ((in >> 40) & 0x000000000000ff00ULL) |
            ((in >> 56) & 0x00000000000000ffULL);
#endif
}


/*=== Little endian r/w ===*/

MEM_STATIC U16 MEM_readLE16(const void* memPtr)
{
    if (MEM_isLittleEndian())
        return MEM_read16(memPtr);
    else {
        const BYTE* p = (const BYTE*)memPtr;
        return (U16)(p[0] + (p[1]<<8));
    }
}

MEM_STATIC void MEM_writeLE16(void* memPtr, U16 val)
{
    if (MEM_isLittleEndian()) {
        MEM_write16(memPtr, val);
    } else {
        BYTE* p = (BYTE*)memPtr;
        p[0] = (BYTE)val;
        p[1] = (BYTE)(val>>8);
    }
}

MEM_STATIC U32 MEM_readLE32(const void* memPtr)
{
    if (MEM_isLittleEndian())
        return MEM_read32(memPtr);
    else
        return MEM_swap32(MEM_read32(memPtr));
}


MEM_STATIC U64 MEM_readLE64(const void* memPtr)
{
    if (MEM_isLittleEndian())
        return MEM_read64(memPtr);
    else
        return MEM_swap64(MEM_read64(memPtr));
}


MEM_STATIC size_t MEM_readLEST(const void* memPtr)
{
    if (MEM_32bits())
        return (size_t)MEM_readLE32(memPtr);
    else
        return (size_t)MEM_readLE64(memPtr);
}



#if defined (__cplusplus)
}
#endif

#endif /* MEM_H_MODULE */

/*
    zstd - standard compression library
    Header File for static linking only
    Copyright (C) 2014-2016, Yann Collet.

    BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:
    * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following disclaimer
    in the documentation and/or other materials provided with the
    distribution.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - zstd homepage : http://www.zstd.net
*/
#ifndef ZSTD1v06_STATIC_H
#define ZSTD1v06_STATIC_H

/* The prototypes defined within this file are considered experimental.
 * They should not be used in the context DLL as they may change in the future.
 * Prefer static linking if you need them, to control breaking version changes issues.
 */

#if defined (__cplusplus)
extern "C" {
#endif



/*- Advanced Decompression functions -*/

/*! ZSTD1v06_decompress_usingPreparedDCtx() :
*   Same as ZSTD1v06_decompress_usingDict, but using a reference context `preparedDCtx`, where dictionary has been loaded.
*   It avoids reloading the dictionary each time.
*   `preparedDCtx` must have been properly initialized using ZSTD1v06_decompressBegin_usingDict().
*   Requires 2 contexts : 1 for reference (preparedDCtx), which will not be modified, and 1 to run the decompression operation (dctx) */
ZSTD1LIBv06_API size_t ZSTD1v06_decompress_usingPreparedDCtx(
                                           ZSTD1v06_DCtx* dctx, const ZSTD1v06_DCtx* preparedDCtx,
                                           void* dst, size_t dstCapacity,
                                     const void* src, size_t srcSize);



#define ZSTD1v06_FRAMEHEADERSIZE_MAX 13    /* for static allocation */
static const size_t ZSTD1v06_frameHeaderSize_min = 5;
static const size_t ZSTD1v06_frameHeaderSize_max = ZSTD1v06_FRAMEHEADERSIZE_MAX;

ZSTD1LIBv06_API size_t ZSTD1v06_decompressBegin(ZSTD1v06_DCtx* dctx);

/*
  Streaming decompression, direct mode (bufferless)

  A ZSTD1v06_DCtx object is required to track streaming operations.
  Use ZSTD1v06_createDCtx() / ZSTD1v06_freeDCtx() to manage it.
  A ZSTD1v06_DCtx object can be re-used multiple times.

  First optional operation is to retrieve frame parameters, using ZSTD1v06_getFrameParams(), which doesn't consume the input.
  It can provide the minimum size of rolling buffer required to properly decompress data,
  and optionally the final size of uncompressed content.
  (Note : content size is an optional info that may not be present. 0 means : content size unknown)
  Frame parameters are extracted from the beginning of compressed frame.
  The amount of data to read is variable, from ZSTD1v06_frameHeaderSize_min to ZSTD1v06_frameHeaderSize_max (so if `srcSize` >= ZSTD1v06_frameHeaderSize_max, it will always work)
  If `srcSize` is too small for operation to succeed, function will return the minimum size it requires to produce a result.
  Result : 0 when successful, it means the ZSTD1v06_frameParams structure has been filled.
          >0 : means there is not enough data into `src`. Provides the expected size to successfully decode header.
           errorCode, which can be tested using ZSTD1v06_isError()

  Start decompression, with ZSTD1v06_decompressBegin() or ZSTD1v06_decompressBegin_usingDict().
  Alternatively, you can copy a prepared context, using ZSTD1v06_copyDCtx().

  Then use ZSTD1v06_nextSrcSizeToDecompress() and ZSTD1v06_decompressContinue() alternatively.
  ZSTD1v06_nextSrcSizeToDecompress() tells how much bytes to provide as 'srcSize' to ZSTD1v06_decompressContinue().
  ZSTD1v06_decompressContinue() requires this exact amount of bytes, or it will fail.
  ZSTD1v06_decompressContinue() needs previous data blocks during decompression, up to (1 << windowlog).
  They should preferably be located contiguously, prior to current block. Alternatively, a round buffer is also possible.

  @result of ZSTD1v06_decompressContinue() is the number of bytes regenerated within 'dst' (necessarily <= dstCapacity)
  It can be zero, which is not an error; it just means ZSTD1v06_decompressContinue() has decoded some header.

  A frame is fully decoded when ZSTD1v06_nextSrcSizeToDecompress() returns zero.
  Context can then be reset to start a new decompression.
*/


/* **************************************
*  Block functions
****************************************/
/*! Block functions produce and decode raw zstd blocks, without frame metadata.
    User will have to take in charge required information to regenerate data, such as compressed and content sizes.

    A few rules to respect :
    - Uncompressed block size must be <= ZSTD1v06_BLOCKSIZE_MAX (128 KB)
    - Compressing or decompressing requires a context structure
      + Use ZSTD1v06_createCCtx() and ZSTD1v06_createDCtx()
    - It is necessary to init context before starting
      + compression : ZSTD1v06_compressBegin()
      + decompression : ZSTD1v06_decompressBegin()
      + variants _usingDict() are also allowed
      + copyCCtx() and copyDCtx() work too
    - When a block is considered not compressible enough, ZSTD1v06_compressBlock() result will be zero.
      In which case, nothing is produced into `dst`.
      + User must test for such outcome and deal directly with uncompressed data
      + ZSTD1v06_decompressBlock() doesn't accept uncompressed data as input !!
*/

#define ZSTD1v06_BLOCKSIZE_MAX (128 * 1024)   /* define, for static allocation */
ZSTD1LIBv06_API size_t ZSTD1v06_decompressBlock(ZSTD1v06_DCtx* dctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize);



#if defined (__cplusplus)
}
#endif

#endif  /* ZSTD1v06_STATIC_H */
/*
    zstd_internal - common functions to include
    Header File for include
    Copyright (C) 2014-2016, Yann Collet.

    BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:
    * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following disclaimer
    in the documentation and/or other materials provided with the
    distribution.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - zstd homepage : https://www.zstd.net
*/
#ifndef ZSTD1v06_CCOMMON_H_MODULE
#define ZSTD1v06_CCOMMON_H_MODULE


/*-*************************************
*  Common macros
***************************************/
#define MIN(a,b) ((a)<(b) ? (a) : (b))
#define MAX(a,b) ((a)>(b) ? (a) : (b))


/*-*************************************
*  Common constants
***************************************/
#define ZSTD1v06_DICT_MAGIC  0xEC30A436

#define ZSTD1v06_REP_NUM    3
#define ZSTD1v06_REP_INIT   ZSTD1v06_REP_NUM
#define ZSTD1v06_REP_MOVE   (ZSTD1v06_REP_NUM-1)

#define KB *(1 <<10)
#define MB *(1 <<20)
#define GB *(1U<<30)

#define BIT7 128
#define BIT6  64
#define BIT5  32
#define BIT4  16
#define BIT1   2
#define BIT0   1

#define ZSTD1v06_WINDOWLOG_ABSOLUTEMIN 12
static const size_t ZSTD1v06_fcs_fieldSize[4] = { 0, 1, 2, 8 };

#define ZSTD1v06_BLOCKHEADERSIZE 3   /* because C standard does not allow a static const value to be defined using another static const value .... :( */
static const size_t ZSTD1v06_blockHeaderSize = ZSTD1v06_BLOCKHEADERSIZE;
typedef enum { bt_compressed, bt_raw, bt_rle, bt_end } blockType_t;

#define MIN_SEQUENCES_SIZE 1 /* nbSeq==0 */
#define MIN_CBLOCK_SIZE (1 /*litCSize*/ + 1 /* RLE or RAW */ + MIN_SEQUENCES_SIZE /* nbSeq==0 */)   /* for a non-null block */

#define HufLog 12

#define IS_HUF1 0
#define IS_PCH 1
#define IS_RAW 2
#define IS_RLE 3

#define LONGNBSEQ 0x7F00

#define MINMATCH 3
#define EQUAL_READ32 4
#define REPCODE_STARTVALUE 1

#define Litbits  8
#define MaxLit ((1<<Litbits) - 1)
#define MaxML  52
#define MaxLL  35
#define MaxOff 28
#define MaxSeq MAX(MaxLL, MaxML)   /* Assumption : MaxOff < MaxLL,MaxML */
#define MLFSE1Log    9
#define LLFSE1Log    9
#define OffFSE1Log   8

#define FSE1v06_ENCODING_RAW     0
#define FSE1v06_ENCODING_RLE     1
#define FSE1v06_ENCODING_STATIC  2
#define FSE1v06_ENCODING_DYNAMIC 3

static const U32 LL_bits[MaxLL+1] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      1, 1, 1, 1, 2, 2, 3, 3, 4, 6, 7, 8, 9,10,11,12,
                                     13,14,15,16 };
static const S16 LL_defaultNorm[MaxLL+1] = { 4, 3, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1,
                                             2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 1, 1, 1, 1, 1,
                                            -1,-1,-1,-1 };
static const U32 LL_defaultNormLog = 6;

static const U32 ML_bits[MaxML+1] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                      1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5, 7, 8, 9,10,11,
                                     12,13,14,15,16 };
static const S16 ML_defaultNorm[MaxML+1] = { 1, 4, 3, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                                             1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,-1,-1,
                                            -1,-1,-1,-1,-1 };
static const U32 ML_defaultNormLog = 6;

static const S16 OF_defaultNorm[MaxOff+1] = { 1, 1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1,
                                              1, 1, 1, 1, 1, 1, 1, 1,-1,-1,-1,-1,-1 };
static const U32 OF_defaultNormLog = 5;


/*-*******************************************
*  Shared functions to include for inlining
*********************************************/
static void ZSTD1v06_copy8(void* dst, const void* src) { memcpy(dst, src, 8); }
#define COPY8(d,s) { ZSTD1v06_copy8(d,s); d+=8; s+=8; }

/*! ZSTD1v06_wildcopy() :
*   custom version of memcpy(), can copy up to 7 bytes too many (8 bytes if length==0) */
#define WILDCOPY_OVERLENGTH 8
MEM_STATIC void ZSTD1v06_wildcopy(void* dst, const void* src, ptrdiff_t length)
{
    const BYTE* ip = (const BYTE*)src;
    BYTE* op = (BYTE*)dst;
    BYTE* const oend = op + length;
    do
        COPY8(op, ip)
    while (op < oend);
}



/*-*******************************************
*  Private interfaces
*********************************************/
typedef struct {
    U32 off;
    U32 len;
} ZSTD1v06_match_t;

typedef struct {
    U32 price;
    U32 off;
    U32 mlen;
    U32 litlen;
    U32 rep[ZSTD1v06_REP_INIT];
} ZSTD1v06_optimal_t;

typedef struct { U32  unused; } ZSTD1v06_stats_t;

typedef struct {
    void* buffer;
    U32*  offsetStart;
    U32*  offset;
    BYTE* offCodeStart;
    BYTE* litStart;
    BYTE* lit;
    U16*  litLengthStart;
    U16*  litLength;
    BYTE* llCodeStart;
    U16*  matchLengthStart;
    U16*  matchLength;
    BYTE* mlCodeStart;
    U32   longLengthID;   /* 0 == no longLength; 1 == Lit.longLength; 2 == Match.longLength; */
    U32   longLengthPos;
    /* opt */
    ZSTD1v06_optimal_t* priceTable;
    ZSTD1v06_match_t* matchTable;
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
    U32  cachedPrice;
    U32  cachedLitLength;
    const BYTE* cachedLiterals;
    ZSTD1v06_stats_t stats;
} seqStore_t;

void ZSTD1v06_seqToCodes(const seqStore_t* seqStorePtr, size_t const nbSeq);


#endif   /* ZSTD1v06_CCOMMON_H_MODULE */
/* ******************************************************************
   FSE1 : Finite State Entropy codec
   Public Prototypes declaration
   Copyright (C) 2013-2016, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   You can contact the author at :
   - Source repository : https://github.com/Cyan4973/FiniteStateEntropy
****************************************************************** */
#ifndef FSE1v06_H
#define FSE1v06_H

#if defined (__cplusplus)
extern "C" {
#endif



/*-****************************************
*  FSE1 simple functions
******************************************/
/*! FSE1v06_decompress():
    Decompress FSE1 data from buffer 'cSrc', of size 'cSrcSize',
    into already allocated destination buffer 'dst', of size 'dstCapacity'.
    @return : size of regenerated data (<= maxDstSize),
              or an error code, which can be tested using FSE1v06_isError() .

    ** Important ** : FSE1v06_decompress() does not decompress non-compressible nor RLE data !!!
    Why ? : making this distinction requires a header.
    Header management is intentionally delegated to the user layer, which can better manage special cases.
*/
size_t FSE1v06_decompress(void* dst,  size_t dstCapacity,
                const void* cSrc, size_t cSrcSize);


/*-*****************************************
*  Tool functions
******************************************/
size_t FSE1v06_compressBound(size_t size);       /* maximum compressed size */

/* Error Management */
unsigned    FSE1v06_isError(size_t code);        /* tells if a return value is an error code */
const char* FSE1v06_getErrorName(size_t code);   /* provides error code string (useful for debugging) */



/*-*****************************************
*  FSE1 detailed API
******************************************/
/*!

FSE1v06_decompress() does the following:
1. read normalized counters with readNCount()
2. build decoding table 'DTable' from normalized counters
3. decode the data stream using decoding table 'DTable'

The following API allows targeting specific sub-functions for advanced tasks.
For example, it's possible to compress several blocks using the same 'CTable',
or to save and provide normalized distribution using external method.
*/


/* *** DECOMPRESSION *** */

/*! FSE1v06_readNCount():
    Read compactly saved 'normalizedCounter' from 'rBuffer'.
    @return : size read from 'rBuffer',
              or an errorCode, which can be tested using FSE1v06_isError().
              maxSymbolValuePtr[0] and tableLogPtr[0] will also be updated with their respective values */
size_t FSE1v06_readNCount (short* normalizedCounter, unsigned* maxSymbolValuePtr, unsigned* tableLogPtr, const void* rBuffer, size_t rBuffSize);

/*! Constructor and Destructor of FSE1v06_DTable.
    Note that its size depends on 'tableLog' */
typedef unsigned FSE1v06_DTable;   /* don't allocate that. It's just a way to be more restrictive than void* */
FSE1v06_DTable* FSE1v06_createDTable(unsigned tableLog);
void        FSE1v06_freeDTable(FSE1v06_DTable* dt);

/*! FSE1v06_buildDTable():
    Builds 'dt', which must be already allocated, using FSE1v06_createDTable().
    return : 0, or an errorCode, which can be tested using FSE1v06_isError() */
size_t FSE1v06_buildDTable (FSE1v06_DTable* dt, const short* normalizedCounter, unsigned maxSymbolValue, unsigned tableLog);

/*! FSE1v06_decompress_usingDTable():
    Decompress compressed source `cSrc` of size `cSrcSize` using `dt`
    into `dst` which must be already allocated.
    @return : size of regenerated data (necessarily <= `dstCapacity`),
              or an errorCode, which can be tested using FSE1v06_isError() */
size_t FSE1v06_decompress_usingDTable(void* dst, size_t dstCapacity, const void* cSrc, size_t cSrcSize, const FSE1v06_DTable* dt);

/*!
Tutorial :
----------
(Note : these functions only decompress FSE1-compressed blocks.
 If block is uncompressed, use memcpy() instead
 If block is a single repeated byte, use memset() instead )

The first step is to obtain the normalized frequencies of symbols.
This can be performed by FSE1v06_readNCount() if it was saved using FSE1v06_writeNCount().
'normalizedCounter' must be already allocated, and have at least 'maxSymbolValuePtr[0]+1' cells of signed short.
In practice, that means it's necessary to know 'maxSymbolValue' beforehand,
or size the table to handle worst case situations (typically 256).
FSE1v06_readNCount() will provide 'tableLog' and 'maxSymbolValue'.
The result of FSE1v06_readNCount() is the number of bytes read from 'rBuffer'.
Note that 'rBufferSize' must be at least 4 bytes, even if useful information is less than that.
If there is an error, the function will return an error code, which can be tested using FSE1v06_isError().

The next step is to build the decompression tables 'FSE1v06_DTable' from 'normalizedCounter'.
This is performed by the function FSE1v06_buildDTable().
The space required by 'FSE1v06_DTable' must be already allocated using FSE1v06_createDTable().
If there is an error, the function will return an error code, which can be tested using FSE1v06_isError().

`FSE1v06_DTable` can then be used to decompress `cSrc`, with FSE1v06_decompress_usingDTable().
`cSrcSize` must be strictly correct, otherwise decompression will fail.
FSE1v06_decompress_usingDTable() result will tell how many bytes were regenerated (<=`dstCapacity`).
If there is an error, the function will return an error code, which can be tested using FSE1v06_isError(). (ex: dst buffer too small)
*/


#if defined (__cplusplus)
}
#endif

#endif  /* FSE1v06_H */
/* ******************************************************************
   bitstream
   Part of FSE1 library
   header file (to include)
   Copyright (C) 2013-2016, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   You can contact the author at :
   - Source repository : https://github.com/Cyan4973/FiniteStateEntropy
****************************************************************** */
#ifndef BITSTREAM_H_MODULE
#define BITSTREAM_H_MODULE

#if defined (__cplusplus)
extern "C" {
#endif


/*
*  This API consists of small unitary functions, which must be inlined for best performance.
*  Since link-time-optimization is not available for all compilers,
*  these functions are defined into a .h to be included.
*/


/*=========================================
*  Target specific
=========================================*/
#if defined(__BMI__) && defined(__GNUC__)
#  include <immintrin.h>   /* support for bextr (experimental) */
#endif



/*-********************************************
*  bitStream decoding API (read backward)
**********************************************/
typedef struct
{
    size_t   bitContainer;
    unsigned bitsConsumed;
    const char* ptr;
    const char* start;
} BITv06_DStream_t;

typedef enum { BITv06_DStream_unfinished = 0,
               BITv06_DStream_endOfBuffer = 1,
               BITv06_DStream_completed = 2,
               BITv06_DStream_overflow = 3 } BITv06_DStream_status;  /* result of BITv06_reloadDStream() */
               /* 1,2,4,8 would be better for bitmap combinations, but slows down performance a bit ... :( */

MEM_STATIC size_t   BITv06_initDStream(BITv06_DStream_t* bitD, const void* srcBuffer, size_t srcSize);
MEM_STATIC size_t   BITv06_readBits(BITv06_DStream_t* bitD, unsigned nbBits);
MEM_STATIC BITv06_DStream_status BITv06_reloadDStream(BITv06_DStream_t* bitD);
MEM_STATIC unsigned BITv06_endOfDStream(const BITv06_DStream_t* bitD);


/* Start by invoking BITv06_initDStream().
*  A chunk of the bitStream is then stored into a local register.
*  Local register size is 64-bits on 64-bits systems, 32-bits on 32-bits systems (size_t).
*  You can then retrieve bitFields stored into the local register, **in reverse order**.
*  Local register is explicitly reloaded from memory by the BITv06_reloadDStream() method.
*  A reload guarantee a minimum of ((8*sizeof(bitD->bitContainer))-7) bits when its result is BITv06_DStream_unfinished.
*  Otherwise, it can be less than that, so proceed accordingly.
*  Checking if DStream has reached its end can be performed with BITv06_endOfDStream().
*/


/*-****************************************
*  unsafe API
******************************************/
MEM_STATIC size_t BITv06_readBitsFast(BITv06_DStream_t* bitD, unsigned nbBits);
/* faster, but works only if nbBits >= 1 */



/*-**************************************************************
*  Internal functions
****************************************************************/
MEM_STATIC unsigned BITv06_highbit32 (register U32 val)
{
#   if defined(_MSC_VER)   /* Visual */
    unsigned long r=0;
    _BitScanReverse ( &r, val );
    return (unsigned) r;
#   elif defined(__GNUC__) && (__GNUC__ >= 3)   /* Use GCC Intrinsic */
    return 31 - __builtin_clz (val);
#   else   /* Software version */
    static const unsigned DeBruijnClz[32] = { 0, 9, 1, 10, 13, 21, 2, 29, 11, 14, 16, 18, 22, 25, 3, 30, 8, 12, 20, 28, 15, 17, 24, 7, 19, 27, 23, 6, 26, 5, 4, 31 };
    U32 v = val;
    unsigned r;
    v |= v >> 1;
    v |= v >> 2;
    v |= v >> 4;
    v |= v >> 8;
    v |= v >> 16;
    r = DeBruijnClz[ (U32) (v * 0x07C4ACDDU) >> 27];
    return r;
#   endif
}



/*-********************************************************
* bitStream decoding
**********************************************************/
/*! BITv06_initDStream() :
*   Initialize a BITv06_DStream_t.
*   `bitD` : a pointer to an already allocated BITv06_DStream_t structure.
*   `srcSize` must be the *exact* size of the bitStream, in bytes.
*   @return : size of stream (== srcSize) or an errorCode if a problem is detected
*/
MEM_STATIC size_t BITv06_initDStream(BITv06_DStream_t* bitD, const void* srcBuffer, size_t srcSize)
{
    if (srcSize < 1) { memset(bitD, 0, sizeof(*bitD)); return ERROR(srcSize_wrong); }

    if (srcSize >=  sizeof(bitD->bitContainer)) {  /* normal case */
        bitD->start = (const char*)srcBuffer;
        bitD->ptr   = (const char*)srcBuffer + srcSize - sizeof(bitD->bitContainer);
        bitD->bitContainer = MEM_readLEST(bitD->ptr);
        { BYTE const lastByte = ((const BYTE*)srcBuffer)[srcSize-1];
          if (lastByte == 0) return ERROR(GENERIC);   /* endMark not present */
          bitD->bitsConsumed = 8 - BITv06_highbit32(lastByte); }
    } else {
        bitD->start = (const char*)srcBuffer;
        bitD->ptr   = bitD->start;
        bitD->bitContainer = *(const BYTE*)(bitD->start);
        switch(srcSize)
        {
            case 7: bitD->bitContainer += (size_t)(((const BYTE*)(srcBuffer))[6]) << (sizeof(bitD->bitContainer)*8 - 16);/* fall-through */
            case 6: bitD->bitContainer += (size_t)(((const BYTE*)(srcBuffer))[5]) << (sizeof(bitD->bitContainer)*8 - 24);/* fall-through */
            case 5: bitD->bitContainer += (size_t)(((const BYTE*)(srcBuffer))[4]) << (sizeof(bitD->bitContainer)*8 - 32);/* fall-through */
            case 4: bitD->bitContainer += (size_t)(((const BYTE*)(srcBuffer))[3]) << 24; /* fall-through */
            case 3: bitD->bitContainer += (size_t)(((const BYTE*)(srcBuffer))[2]) << 16; /* fall-through */
            case 2: bitD->bitContainer += (size_t)(((const BYTE*)(srcBuffer))[1]) <<  8; /* fall-through */
            default: break;
        }
        { BYTE const lastByte = ((const BYTE*)srcBuffer)[srcSize-1];
          if (lastByte == 0) return ERROR(GENERIC);   /* endMark not present */
          bitD->bitsConsumed = 8 - BITv06_highbit32(lastByte); }
        bitD->bitsConsumed += (U32)(sizeof(bitD->bitContainer) - srcSize)*8;
    }

    return srcSize;
}


/*! BITv06_lookBits() :
 *  Provides next n bits from local register.
 *  local register is not modified.
 *  On 32-bits, maxNbBits==24.
 *  On 64-bits, maxNbBits==56.
 *  @return : value extracted
 */
 MEM_STATIC size_t BITv06_lookBits(const BITv06_DStream_t* bitD, U32 nbBits)
{
    U32 const bitMask = sizeof(bitD->bitContainer)*8 - 1;
    return ((bitD->bitContainer << (bitD->bitsConsumed & bitMask)) >> 1) >> ((bitMask-nbBits) & bitMask);
}

/*! BITv06_lookBitsFast() :
*   unsafe version; only works only if nbBits >= 1 */
MEM_STATIC size_t BITv06_lookBitsFast(const BITv06_DStream_t* bitD, U32 nbBits)
{
    U32 const bitMask = sizeof(bitD->bitContainer)*8 - 1;
    return (bitD->bitContainer << (bitD->bitsConsumed & bitMask)) >> (((bitMask+1)-nbBits) & bitMask);
}

MEM_STATIC void BITv06_skipBits(BITv06_DStream_t* bitD, U32 nbBits)
{
    bitD->bitsConsumed += nbBits;
}

/*! BITv06_readBits() :
 *  Read (consume) next n bits from local register and update.
 *  Pay attention to not read more than nbBits contained into local register.
 *  @return : extracted value.
 */
MEM_STATIC size_t BITv06_readBits(BITv06_DStream_t* bitD, U32 nbBits)
{
    size_t const value = BITv06_lookBits(bitD, nbBits);
    BITv06_skipBits(bitD, nbBits);
    return value;
}

/*! BITv06_readBitsFast() :
*   unsafe version; only works only if nbBits >= 1 */
MEM_STATIC size_t BITv06_readBitsFast(BITv06_DStream_t* bitD, U32 nbBits)
{
    size_t const value = BITv06_lookBitsFast(bitD, nbBits);
    BITv06_skipBits(bitD, nbBits);
    return value;
}

/*! BITv06_reloadDStream() :
*   Refill `BITv06_DStream_t` from src buffer previously defined (see BITv06_initDStream() ).
*   This function is safe, it guarantees it will not read beyond src buffer.
*   @return : status of `BITv06_DStream_t` internal register.
              if status == unfinished, internal register is filled with >= (sizeof(bitD->bitContainer)*8 - 7) bits */
MEM_STATIC BITv06_DStream_status BITv06_reloadDStream(BITv06_DStream_t* bitD)
{
    if (bitD->bitsConsumed > (sizeof(bitD->bitContainer)*8))  /* should never happen */
        return BITv06_DStream_overflow;

    if (bitD->ptr >= bitD->start + sizeof(bitD->bitContainer)) {
        bitD->ptr -= bitD->bitsConsumed >> 3;
        bitD->bitsConsumed &= 7;
        bitD->bitContainer = MEM_readLEST(bitD->ptr);
        return BITv06_DStream_unfinished;
    }
    if (bitD->ptr == bitD->start) {
        if (bitD->bitsConsumed < sizeof(bitD->bitContainer)*8) return BITv06_DStream_endOfBuffer;
        return BITv06_DStream_completed;
    }
    {   U32 nbBytes = bitD->bitsConsumed >> 3;
        BITv06_DStream_status result = BITv06_DStream_unfinished;
        if (bitD->ptr - nbBytes < bitD->start) {
            nbBytes = (U32)(bitD->ptr - bitD->start);  /* ptr > start */
            result = BITv06_DStream_endOfBuffer;
        }
        bitD->ptr -= nbBytes;
        bitD->bitsConsumed -= nbBytes*8;
        bitD->bitContainer = MEM_readLEST(bitD->ptr);   /* reminder : srcSize > sizeof(bitD) */
        return result;
    }
}

/*! BITv06_endOfDStream() :
*   @return Tells if DStream has exactly reached its end (all bits consumed).
*/
MEM_STATIC unsigned BITv06_endOfDStream(const BITv06_DStream_t* DStream)
{
    return ((DStream->ptr == DStream->start) && (DStream->bitsConsumed == sizeof(DStream->bitContainer)*8));
}

#if defined (__cplusplus)
}
#endif

#endif /* BITSTREAM_H_MODULE */
/* ******************************************************************
   FSE1 : Finite State Entropy coder
   header file for static linking (only)
   Copyright (C) 2013-2015, Yann Collet

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   You can contact the author at :
   - Source repository : https://github.com/Cyan4973/FiniteStateEntropy
   - Public forum : https://groups.google.com/forum/#!forum/lz4c
****************************************************************** */
#ifndef FSE1v06_STATIC_H
#define FSE1v06_STATIC_H

#if defined (__cplusplus)
extern "C" {
#endif


/* *****************************************
*  Static allocation
*******************************************/
/* FSE1 buffer bounds */
#define FSE1v06_NCOUNTBOUND 512
#define FSE1v06_BLOCKBOUND(size) (size + (size>>7))
#define FSE1v06_COMPRESSBOUND(size) (FSE1v06_NCOUNTBOUND + FSE1v06_BLOCKBOUND(size))   /* Macro version, useful for static allocation */

/* It is possible to statically allocate FSE1 CTable/DTable as a table of unsigned using below macros */
#define FSE1v06_DTABLE_SIZE_U32(maxTableLog)                   (1 + (1<<maxTableLog))


/* *****************************************
*  FSE1 advanced API
*******************************************/
size_t FSE1v06_countFast(unsigned* count, unsigned* maxSymbolValuePtr, const void* src, size_t srcSize);
/* same as FSE1v06_count(), but blindly trusts that all byte values within src are <= *maxSymbolValuePtr  */

size_t FSE1v06_buildDTable_raw (FSE1v06_DTable* dt, unsigned nbBits);
/* build a fake FSE1v06_DTable, designed to read an uncompressed bitstream where each symbol uses nbBits */

size_t FSE1v06_buildDTable_rle (FSE1v06_DTable* dt, unsigned char symbolValue);
/* build a fake FSE1v06_DTable, designed to always generate the same symbolValue */


/* *****************************************
*  FSE1 symbol decompression API
*******************************************/
typedef struct
{
    size_t      state;
    const void* table;   /* precise table may vary, depending on U16 */
} FSE1v06_DState_t;


static void     FSE1v06_initDState(FSE1v06_DState_t* DStatePtr, BITv06_DStream_t* bitD, const FSE1v06_DTable* dt);

static unsigned char FSE1v06_decodeSymbol(FSE1v06_DState_t* DStatePtr, BITv06_DStream_t* bitD);

/*!
Let's now decompose FSE1v06_decompress_usingDTable() into its unitary components.
You will decode FSE1-encoded symbols from the bitStream,
and also any other bitFields you put in, **in reverse order**.

You will need a few variables to track your bitStream. They are :

BITv06_DStream_t DStream;    // Stream context
FSE1v06_DState_t  DState;     // State context. Multiple ones are possible
FSE1v06_DTable*   DTablePtr;  // Decoding table, provided by FSE1v06_buildDTable()

The first thing to do is to init the bitStream.
    errorCode = BITv06_initDStream(&DStream, srcBuffer, srcSize);

You should then retrieve your initial state(s)
(in reverse flushing order if you have several ones) :
    errorCode = FSE1v06_initDState(&DState, &DStream, DTablePtr);

You can then decode your data, symbol after symbol.
For information the maximum number of bits read by FSE1v06_decodeSymbol() is 'tableLog'.
Keep in mind that symbols are decoded in reverse order, like a LIFO stack (last in, first out).
    unsigned char symbol = FSE1v06_decodeSymbol(&DState, &DStream);

You can retrieve any bitfield you eventually stored into the bitStream (in reverse order)
Note : maximum allowed nbBits is 25, for 32-bits compatibility
    size_t bitField = BITv06_readBits(&DStream, nbBits);

All above operations only read from local register (which size depends on size_t).
Refueling the register from memory is manually performed by the reload method.
    endSignal = FSE1v06_reloadDStream(&DStream);

BITv06_reloadDStream() result tells if there is still some more data to read from DStream.
BITv06_DStream_unfinished : there is still some data left into the DStream.
BITv06_DStream_endOfBuffer : Dstream reached end of buffer. Its container may no longer be completely filled.
BITv06_DStream_completed : Dstream reached its exact end, corresponding in general to decompression completed.
BITv06_DStream_tooFar : Dstream went too far. Decompression result is corrupted.

When reaching end of buffer (BITv06_DStream_endOfBuffer), progress slowly, notably if you decode multiple symbols per loop,
to properly detect the exact end of stream.
After each decoded symbol, check if DStream is fully consumed using this simple test :
    BITv06_reloadDStream(&DStream) >= BITv06_DStream_completed

When it's done, verify decompression is fully completed, by checking both DStream and the relevant states.
Checking if DStream has reached its end is performed by :
    BITv06_endOfDStream(&DStream);
Check also the states. There might be some symbols left there, if some high probability ones (>50%) are possible.
    FSE1v06_endOfDState(&DState);
*/


/* *****************************************
*  FSE1 unsafe API
*******************************************/
static unsigned char FSE1v06_decodeSymbolFast(FSE1v06_DState_t* DStatePtr, BITv06_DStream_t* bitD);
/* faster, but works only if nbBits is always >= 1 (otherwise, result will be corrupted) */


/* *****************************************
*  Implementation of inlined functions
*******************************************/


/* ======    Decompression    ====== */

typedef struct {
    U16 tableLog;
    U16 fastMode;
} FSE1v06_DTableHeader;   /* sizeof U32 */

typedef struct
{
    unsigned short newState;
    unsigned char  symbol;
    unsigned char  nbBits;
} FSE1v06_decode_t;   /* size == U32 */

MEM_STATIC void FSE1v06_initDState(FSE1v06_DState_t* DStatePtr, BITv06_DStream_t* bitD, const FSE1v06_DTable* dt)
{
    const void* ptr = dt;
    const FSE1v06_DTableHeader* const DTableH = (const FSE1v06_DTableHeader*)ptr;
    DStatePtr->state = BITv06_readBits(bitD, DTableH->tableLog);
    BITv06_reloadDStream(bitD);
    DStatePtr->table = dt + 1;
}

MEM_STATIC BYTE FSE1v06_peekSymbol(const FSE1v06_DState_t* DStatePtr)
{
    FSE1v06_decode_t const DInfo = ((const FSE1v06_decode_t*)(DStatePtr->table))[DStatePtr->state];
    return DInfo.symbol;
}

MEM_STATIC void FSE1v06_updateState(FSE1v06_DState_t* DStatePtr, BITv06_DStream_t* bitD)
{
    FSE1v06_decode_t const DInfo = ((const FSE1v06_decode_t*)(DStatePtr->table))[DStatePtr->state];
    U32 const nbBits = DInfo.nbBits;
    size_t const lowBits = BITv06_readBits(bitD, nbBits);
    DStatePtr->state = DInfo.newState + lowBits;
}

MEM_STATIC BYTE FSE1v06_decodeSymbol(FSE1v06_DState_t* DStatePtr, BITv06_DStream_t* bitD)
{
    FSE1v06_decode_t const DInfo = ((const FSE1v06_decode_t*)(DStatePtr->table))[DStatePtr->state];
    U32 const nbBits = DInfo.nbBits;
    BYTE const symbol = DInfo.symbol;
    size_t const lowBits = BITv06_readBits(bitD, nbBits);

    DStatePtr->state = DInfo.newState + lowBits;
    return symbol;
}

/*! FSE1v06_decodeSymbolFast() :
    unsafe, only works if no symbol has a probability > 50% */
MEM_STATIC BYTE FSE1v06_decodeSymbolFast(FSE1v06_DState_t* DStatePtr, BITv06_DStream_t* bitD)
{
    FSE1v06_decode_t const DInfo = ((const FSE1v06_decode_t*)(DStatePtr->table))[DStatePtr->state];
    U32 const nbBits = DInfo.nbBits;
    BYTE const symbol = DInfo.symbol;
    size_t const lowBits = BITv06_readBitsFast(bitD, nbBits);

    DStatePtr->state = DInfo.newState + lowBits;
    return symbol;
}



#ifndef FSE1v06_COMMONDEFS_ONLY

/* **************************************************************
*  Tuning parameters
****************************************************************/
/*!MEMORY_USAGE :
*  Memory usage formula : N->2^N Bytes (examples : 10 -> 1KB; 12 -> 4KB ; 16 -> 64KB; 20 -> 1MB; etc.)
*  Increasing memory usage improves compression ratio
*  Reduced memory usage can improve speed, due to cache effect
*  Recommended max value is 14, for 16KB, which nicely fits into Intel x86 L1 cache */
#define FSE1v06_MAX_MEMORY_USAGE 14
#define FSE1v06_DEFAULT_MEMORY_USAGE 13

/*!FSE1v06_MAX_SYMBOL_VALUE :
*  Maximum symbol value authorized.
*  Required for proper stack allocation */
#define FSE1v06_MAX_SYMBOL_VALUE 255


/* **************************************************************
*  template functions type & suffix
****************************************************************/
#define FSE1v06_FUNCTION_TYPE BYTE
#define FSE1v06_FUNCTION_EXTENSION
#define FSE1v06_DECODE_TYPE FSE1v06_decode_t


#endif   /* !FSE1v06_COMMONDEFS_ONLY */


/* ***************************************************************
*  Constants
*****************************************************************/
#define FSE1v06_MAX_TABLELOG  (FSE1v06_MAX_MEMORY_USAGE-2)
#define FSE1v06_MAX_TABLESIZE (1U<<FSE1v06_MAX_TABLELOG)
#define FSE1v06_MAXTABLESIZE_MASK (FSE1v06_MAX_TABLESIZE-1)
#define FSE1v06_DEFAULT_TABLELOG (FSE1v06_DEFAULT_MEMORY_USAGE-2)
#define FSE1v06_MIN_TABLELOG 5

#define FSE1v06_TABLELOG_ABSOLUTE_MAX 15
#if FSE1v06_MAX_TABLELOG > FSE1v06_TABLELOG_ABSOLUTE_MAX
#error "FSE1v06_MAX_TABLELOG > FSE1v06_TABLELOG_ABSOLUTE_MAX is not supported"
#endif

#define FSE1v06_TABLESTEP(tableSize) ((tableSize>>1) + (tableSize>>3) + 3)


#if defined (__cplusplus)
}
#endif

#endif  /* FSE1v06_STATIC_H */
/*
   Common functions of New Generation Entropy library
   Copyright (C) 2016, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - FSE1+HUF1 source repository : https://github.com/Cyan4973/FiniteStateEntropy
    - Public forum : https://groups.google.com/forum/#!forum/lz4c
*************************************************************************** */


/*-****************************************
*  FSE1 Error Management
******************************************/
unsigned FSE1v06_isError(size_t code) { return ERR_isError(code); }

const char* FSE1v06_getErrorName(size_t code) { return ERR_getErrorName(code); }


/* **************************************************************
*  HUF1 Error Management
****************************************************************/
unsigned HUF1v06_isError(size_t code) { return ERR_isError(code); }

const char* HUF1v06_getErrorName(size_t code) { return ERR_getErrorName(code); }


/*-**************************************************************
*  FSE1 NCount encoding-decoding
****************************************************************/
static short FSE1v06_abs(short a) { return a<0 ? -a : a; }

size_t FSE1v06_readNCount (short* normalizedCounter, unsigned* maxSVPtr, unsigned* tableLogPtr,
                 const void* headerBuffer, size_t hbSize)
{
    const BYTE* const istart = (const BYTE*) headerBuffer;
    const BYTE* const iend = istart + hbSize;
    const BYTE* ip = istart;
    int nbBits;
    int remaining;
    int threshold;
    U32 bitStream;
    int bitCount;
    unsigned charnum = 0;
    int previous0 = 0;

    if (hbSize < 4) return ERROR(srcSize_wrong);
    bitStream = MEM_readLE32(ip);
    nbBits = (bitStream & 0xF) + FSE1v06_MIN_TABLELOG;   /* extract tableLog */
    if (nbBits > FSE1v06_TABLELOG_ABSOLUTE_MAX) return ERROR(tableLog_tooLarge);
    bitStream >>= 4;
    bitCount = 4;
    *tableLogPtr = nbBits;
    remaining = (1<<nbBits)+1;
    threshold = 1<<nbBits;
    nbBits++;

    while ((remaining>1) && (charnum<=*maxSVPtr)) {
        if (previous0) {
            unsigned n0 = charnum;
            while ((bitStream & 0xFFFF) == 0xFFFF) {
                n0+=24;
                if (ip < iend-5) {
                    ip+=2;
                    bitStream = MEM_readLE32(ip) >> bitCount;
                } else {
                    bitStream >>= 16;
                    bitCount+=16;
            }   }
            while ((bitStream & 3) == 3) {
                n0+=3;
                bitStream>>=2;
                bitCount+=2;
            }
            n0 += bitStream & 3;
            bitCount += 2;
            if (n0 > *maxSVPtr) return ERROR(maxSymbolValue_tooSmall);
            while (charnum < n0) normalizedCounter[charnum++] = 0;
            if ((ip <= iend-7) || (ip + (bitCount>>3) <= iend-4)) {
                ip += bitCount>>3;
                bitCount &= 7;
                bitStream = MEM_readLE32(ip) >> bitCount;
            }
            else
                bitStream >>= 2;
        }
        {   short const max = (short)((2*threshold-1)-remaining);
            short count;

            if ((bitStream & (threshold-1)) < (U32)max) {
                count = (short)(bitStream & (threshold-1));
                bitCount   += nbBits-1;
            } else {
                count = (short)(bitStream & (2*threshold-1));
                if (count >= threshold) count -= max;
                bitCount   += nbBits;
            }

            count--;   /* extra accuracy */
            remaining -= FSE1v06_abs(count);
            normalizedCounter[charnum++] = count;
            previous0 = !count;
            while (remaining < threshold) {
                nbBits--;
                threshold >>= 1;
            }

            if ((ip <= iend-7) || (ip + (bitCount>>3) <= iend-4)) {
                ip += bitCount>>3;
                bitCount &= 7;
            } else {
                bitCount -= (int)(8 * (iend - 4 - ip));
                ip = iend - 4;
            }
            bitStream = MEM_readLE32(ip) >> (bitCount & 31);
    }   }   /* while ((remaining>1) && (charnum<=*maxSVPtr)) */
    if (remaining != 1) return ERROR(GENERIC);
    *maxSVPtr = charnum-1;

    ip += (bitCount+7)>>3;
    if ((size_t)(ip-istart) > hbSize) return ERROR(srcSize_wrong);
    return ip-istart;
}
/* ******************************************************************
   FSE1 : Finite State Entropy decoder
   Copyright (C) 2013-2015, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - FSE1 source repository : https://github.com/Cyan4973/FiniteStateEntropy
    - Public forum : https://groups.google.com/forum/#!forum/lz4c
****************************************************************** */


/* **************************************************************
*  Compiler specifics
****************************************************************/
#ifdef _MSC_VER    /* Visual Studio */
#  define FORCE_INLINE static __forceinline
#  include <intrin.h>                    /* For Visual 2005 */
#  pragma warning(disable : 4127)        /* disable: C4127: conditional expression is constant */
#  pragma warning(disable : 4214)        /* disable: C4214: non-int bitfields */
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


/* **************************************************************
*  Error Management
****************************************************************/
#define FSE1v06_isError ERR_isError
#define FSE1v06_STATIC_ASSERT(c) { enum { FSE1v06_static_assert = 1/(int)(!!(c)) }; }   /* use only *after* variable declarations */


/* **************************************************************
*  Complex types
****************************************************************/
typedef U32 DTable_max_t[FSE1v06_DTABLE_SIZE_U32(FSE1v06_MAX_TABLELOG)];


/* **************************************************************
*  Templates
****************************************************************/
/*
  designed to be included
  for type-specific functions (template emulation in C)
  Objective is to write these functions only once, for improved maintenance
*/

/* safety checks */
#ifndef FSE1v06_FUNCTION_EXTENSION
#  error "FSE1v06_FUNCTION_EXTENSION must be defined"
#endif
#ifndef FSE1v06_FUNCTION_TYPE
#  error "FSE1v06_FUNCTION_TYPE must be defined"
#endif

/* Function names */
#define FSE1v06_CAT(X,Y) X##Y
#define FSE1v06_FUNCTION_NAME(X,Y) FSE1v06_CAT(X,Y)
#define FSE1v06_TYPE_NAME(X,Y) FSE1v06_CAT(X,Y)


/* Function templates */
FSE1v06_DTable* FSE1v06_createDTable (unsigned tableLog)
{
    if (tableLog > FSE1v06_TABLELOG_ABSOLUTE_MAX) tableLog = FSE1v06_TABLELOG_ABSOLUTE_MAX;
    return (FSE1v06_DTable*)malloc( FSE1v06_DTABLE_SIZE_U32(tableLog) * sizeof (U32) );
}

void FSE1v06_freeDTable (FSE1v06_DTable* dt)
{
    free(dt);
}

size_t FSE1v06_buildDTable(FSE1v06_DTable* dt, const short* normalizedCounter, unsigned maxSymbolValue, unsigned tableLog)
{
    void* const tdPtr = dt+1;   /* because *dt is unsigned, 32-bits aligned on 32-bits */
    FSE1v06_DECODE_TYPE* const tableDecode = (FSE1v06_DECODE_TYPE*) (tdPtr);
    U16 symbolNext[FSE1v06_MAX_SYMBOL_VALUE+1];

    U32 const maxSV1 = maxSymbolValue + 1;
    U32 const tableSize = 1 << tableLog;
    U32 highThreshold = tableSize-1;

    /* Sanity Checks */
    if (maxSymbolValue > FSE1v06_MAX_SYMBOL_VALUE) return ERROR(maxSymbolValue_tooLarge);
    if (tableLog > FSE1v06_MAX_TABLELOG) return ERROR(tableLog_tooLarge);

    /* Init, lay down lowprob symbols */
    {   FSE1v06_DTableHeader DTableH;
        DTableH.tableLog = (U16)tableLog;
        DTableH.fastMode = 1;
        {   S16 const largeLimit= (S16)(1 << (tableLog-1));
            U32 s;
            for (s=0; s<maxSV1; s++) {
                if (normalizedCounter[s]==-1) {
                    tableDecode[highThreshold--].symbol = (FSE1v06_FUNCTION_TYPE)s;
                    symbolNext[s] = 1;
                } else {
                    if (normalizedCounter[s] >= largeLimit) DTableH.fastMode=0;
                    symbolNext[s] = normalizedCounter[s];
        }   }   }
        memcpy(dt, &DTableH, sizeof(DTableH));
    }

    /* Spread symbols */
    {   U32 const tableMask = tableSize-1;
        U32 const step = FSE1v06_TABLESTEP(tableSize);
        U32 s, position = 0;
        for (s=0; s<maxSV1; s++) {
            int i;
            for (i=0; i<normalizedCounter[s]; i++) {
                tableDecode[position].symbol = (FSE1v06_FUNCTION_TYPE)s;
                position = (position + step) & tableMask;
                while (position > highThreshold) position = (position + step) & tableMask;   /* lowprob area */
        }   }

        if (position!=0) return ERROR(GENERIC);   /* position must reach all cells once, otherwise normalizedCounter is incorrect */
    }

    /* Build Decoding table */
    {   U32 u;
        for (u=0; u<tableSize; u++) {
            FSE1v06_FUNCTION_TYPE const symbol = (FSE1v06_FUNCTION_TYPE)(tableDecode[u].symbol);
            U16 nextState = symbolNext[symbol]++;
            tableDecode[u].nbBits = (BYTE) (tableLog - BITv06_highbit32 ((U32)nextState) );
            tableDecode[u].newState = (U16) ( (nextState << tableDecode[u].nbBits) - tableSize);
    }   }

    return 0;
}



#ifndef FSE1v06_COMMONDEFS_ONLY

/*-*******************************************************
*  Decompression (Byte symbols)
*********************************************************/
size_t FSE1v06_buildDTable_rle (FSE1v06_DTable* dt, BYTE symbolValue)
{
    void* ptr = dt;
    FSE1v06_DTableHeader* const DTableH = (FSE1v06_DTableHeader*)ptr;
    void* dPtr = dt + 1;
    FSE1v06_decode_t* const cell = (FSE1v06_decode_t*)dPtr;

    DTableH->tableLog = 0;
    DTableH->fastMode = 0;

    cell->newState = 0;
    cell->symbol = symbolValue;
    cell->nbBits = 0;

    return 0;
}


size_t FSE1v06_buildDTable_raw (FSE1v06_DTable* dt, unsigned nbBits)
{
    void* ptr = dt;
    FSE1v06_DTableHeader* const DTableH = (FSE1v06_DTableHeader*)ptr;
    void* dPtr = dt + 1;
    FSE1v06_decode_t* const dinfo = (FSE1v06_decode_t*)dPtr;
    const unsigned tableSize = 1 << nbBits;
    const unsigned tableMask = tableSize - 1;
    const unsigned maxSV1 = tableMask+1;
    unsigned s;

    /* Sanity checks */
    if (nbBits < 1) return ERROR(GENERIC);         /* min size */

    /* Build Decoding Table */
    DTableH->tableLog = (U16)nbBits;
    DTableH->fastMode = 1;
    for (s=0; s<maxSV1; s++) {
        dinfo[s].newState = 0;
        dinfo[s].symbol = (BYTE)s;
        dinfo[s].nbBits = (BYTE)nbBits;
    }

    return 0;
}

FORCE_INLINE size_t FSE1v06_decompress_usingDTable_generic(
          void* dst, size_t maxDstSize,
    const void* cSrc, size_t cSrcSize,
    const FSE1v06_DTable* dt, const unsigned fast)
{
    BYTE* const ostart = (BYTE*) dst;
    BYTE* op = ostart;
    BYTE* const omax = op + maxDstSize;
    BYTE* const olimit = omax-3;

    BITv06_DStream_t bitD;
    FSE1v06_DState_t state1;
    FSE1v06_DState_t state2;

    /* Init */
    { size_t const errorCode = BITv06_initDStream(&bitD, cSrc, cSrcSize);   /* replaced last arg by maxCompressed Size */
      if (FSE1v06_isError(errorCode)) return errorCode; }

    FSE1v06_initDState(&state1, &bitD, dt);
    FSE1v06_initDState(&state2, &bitD, dt);

#define FSE1v06_GETSYMBOL(statePtr) fast ? FSE1v06_decodeSymbolFast(statePtr, &bitD) : FSE1v06_decodeSymbol(statePtr, &bitD)

    /* 4 symbols per loop */
    for ( ; (BITv06_reloadDStream(&bitD)==BITv06_DStream_unfinished) && (op<olimit) ; op+=4) {
        op[0] = FSE1v06_GETSYMBOL(&state1);

        if (FSE1v06_MAX_TABLELOG*2+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            BITv06_reloadDStream(&bitD);

        op[1] = FSE1v06_GETSYMBOL(&state2);

        if (FSE1v06_MAX_TABLELOG*4+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            { if (BITv06_reloadDStream(&bitD) > BITv06_DStream_unfinished) { op+=2; break; } }

        op[2] = FSE1v06_GETSYMBOL(&state1);

        if (FSE1v06_MAX_TABLELOG*2+7 > sizeof(bitD.bitContainer)*8)    /* This test must be static */
            BITv06_reloadDStream(&bitD);

        op[3] = FSE1v06_GETSYMBOL(&state2);
    }

    /* tail */
    /* note : BITv06_reloadDStream(&bitD) >= FSE1v06_DStream_partiallyFilled; Ends at exactly BITv06_DStream_completed */
    while (1) {
        if (op>(omax-2)) return ERROR(dstSize_tooSmall);

        *op++ = FSE1v06_GETSYMBOL(&state1);

        if (BITv06_reloadDStream(&bitD)==BITv06_DStream_overflow) {
            *op++ = FSE1v06_GETSYMBOL(&state2);
            break;
        }

        if (op>(omax-2)) return ERROR(dstSize_tooSmall);

        *op++ = FSE1v06_GETSYMBOL(&state2);

        if (BITv06_reloadDStream(&bitD)==BITv06_DStream_overflow) {
            *op++ = FSE1v06_GETSYMBOL(&state1);
            break;
    }   }

    return op-ostart;
}


size_t FSE1v06_decompress_usingDTable(void* dst, size_t originalSize,
                            const void* cSrc, size_t cSrcSize,
                            const FSE1v06_DTable* dt)
{
    const void* ptr = dt;
    const FSE1v06_DTableHeader* DTableH = (const FSE1v06_DTableHeader*)ptr;
    const U32 fastMode = DTableH->fastMode;

    /* select fast mode (static) */
    if (fastMode) return FSE1v06_decompress_usingDTable_generic(dst, originalSize, cSrc, cSrcSize, dt, 1);
    return FSE1v06_decompress_usingDTable_generic(dst, originalSize, cSrc, cSrcSize, dt, 0);
}


size_t FSE1v06_decompress(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize)
{
    const BYTE* const istart = (const BYTE*)cSrc;
    const BYTE* ip = istart;
    short counting[FSE1v06_MAX_SYMBOL_VALUE+1];
    DTable_max_t dt;   /* Static analyzer seems unable to understand this table will be properly initialized later */
    unsigned tableLog;
    unsigned maxSymbolValue = FSE1v06_MAX_SYMBOL_VALUE;

    if (cSrcSize<2) return ERROR(srcSize_wrong);   /* too small input size */

    /* normal FSE1 decoding mode */
    {   size_t const NCountLength = FSE1v06_readNCount (counting, &maxSymbolValue, &tableLog, istart, cSrcSize);
        if (FSE1v06_isError(NCountLength)) return NCountLength;
        if (NCountLength >= cSrcSize) return ERROR(srcSize_wrong);   /* too small input size */
        ip += NCountLength;
        cSrcSize -= NCountLength;
    }

    { size_t const errorCode = FSE1v06_buildDTable (dt, counting, maxSymbolValue, tableLog);
      if (FSE1v06_isError(errorCode)) return errorCode; }

    return FSE1v06_decompress_usingDTable (dst, maxDstSize, ip, cSrcSize, dt);   /* always return, even if it is an error code */
}



#endif   /* FSE1v06_COMMONDEFS_ONLY */
/* ******************************************************************
   Huffman coder, part of New Generation Entropy library
   header file
   Copyright (C) 2013-2016, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   You can contact the author at :
   - Source repository : https://github.com/Cyan4973/FiniteStateEntropy
****************************************************************** */
#ifndef HUF1v06_H
#define HUF1v06_H

#if defined (__cplusplus)
extern "C" {
#endif


/* ****************************************
*  HUF1 simple functions
******************************************/
size_t HUF1v06_decompress(void* dst,  size_t dstSize,
                const void* cSrc, size_t cSrcSize);
/*
HUF1v06_decompress() :
    Decompress HUF1 data from buffer 'cSrc', of size 'cSrcSize',
    into already allocated destination buffer 'dst', of size 'dstSize'.
    `dstSize` : must be the **exact** size of original (uncompressed) data.
    Note : in contrast with FSE1, HUF1v06_decompress can regenerate
           RLE (cSrcSize==1) and uncompressed (cSrcSize==dstSize) data,
           because it knows size to regenerate.
    @return : size of regenerated data (== dstSize)
              or an error code, which can be tested using HUF1v06_isError()
*/


/* ****************************************
*  Tool functions
******************************************/
size_t HUF1v06_compressBound(size_t size);       /**< maximum compressed size */


#if defined (__cplusplus)
}
#endif

#endif   /* HUF1v06_H */
/* ******************************************************************
   Huffman codec, part of New Generation Entropy library
   header file, for static linking only
   Copyright (C) 2013-2016, Yann Collet

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

   You can contact the author at :
   - Source repository : https://github.com/Cyan4973/FiniteStateEntropy
****************************************************************** */
#ifndef HUF1v06_STATIC_H
#define HUF1v06_STATIC_H

#if defined (__cplusplus)
extern "C" {
#endif


/* ****************************************
*  Static allocation
******************************************/
/* HUF1 buffer bounds */
#define HUF1v06_CTABLEBOUND 129
#define HUF1v06_BLOCKBOUND(size) (size + (size>>8) + 8)   /* only true if incompressible pre-filtered with fast heuristic */
#define HUF1v06_COMPRESSBOUND(size) (HUF1v06_CTABLEBOUND + HUF1v06_BLOCKBOUND(size))   /* Macro version, useful for static allocation */

/* static allocation of HUF1's DTable */
#define HUF1v06_DTABLE_SIZE(maxTableLog)   (1 + (1<<maxTableLog))
#define HUF1v06_CREATE_STATIC_DTABLEX2(DTable, maxTableLog) \
        unsigned short DTable[HUF1v06_DTABLE_SIZE(maxTableLog)] = { maxTableLog }
#define HUF1v06_CREATE_STATIC_DTABLEX4(DTable, maxTableLog) \
        unsigned int DTable[HUF1v06_DTABLE_SIZE(maxTableLog)] = { maxTableLog }
#define HUF1v06_CREATE_STATIC_DTABLEX6(DTable, maxTableLog) \
        unsigned int DTable[HUF1v06_DTABLE_SIZE(maxTableLog) * 3 / 2] = { maxTableLog }


/* ****************************************
*  Advanced decompression functions
******************************************/
size_t HUF1v06_decompress4X2 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /* single-symbol decoder */
size_t HUF1v06_decompress4X4 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /* double-symbols decoder */



/*!
HUF1v06_decompress() does the following:
1. select the decompression algorithm (X2, X4, X6) based on pre-computed heuristics
2. build Huffman table from save, using HUF1v06_readDTableXn()
3. decode 1 or 4 segments in parallel using HUF1v06_decompressSXn_usingDTable
*/
size_t HUF1v06_readDTableX2 (unsigned short* DTable, const void* src, size_t srcSize);
size_t HUF1v06_readDTableX4 (unsigned* DTable, const void* src, size_t srcSize);

size_t HUF1v06_decompress4X2_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const unsigned short* DTable);
size_t HUF1v06_decompress4X4_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const unsigned* DTable);


/* single stream variants */
size_t HUF1v06_decompress1X2 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /* single-symbol decoder */
size_t HUF1v06_decompress1X4 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /* double-symbol decoder */

size_t HUF1v06_decompress1X2_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const unsigned short* DTable);
size_t HUF1v06_decompress1X4_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const unsigned* DTable);



/* **************************************************************
*  Constants
****************************************************************/
#define HUF1v06_ABSOLUTEMAX_TABLELOG  16   /* absolute limit of HUF1v06_MAX_TABLELOG. Beyond that value, code does not work */
#define HUF1v06_MAX_TABLELOG  12           /* max configured tableLog (for static allocation); can be modified up to HUF1v06_ABSOLUTEMAX_TABLELOG */
#define HUF1v06_DEFAULT_TABLELOG  HUF1v06_MAX_TABLELOG   /* tableLog by default, when not specified */
#define HUF1v06_MAX_SYMBOL_VALUE 255
#if (HUF1v06_MAX_TABLELOG > HUF1v06_ABSOLUTEMAX_TABLELOG)
#  error "HUF1v06_MAX_TABLELOG is too large !"
#endif



/*! HUF1v06_readStats() :
    Read compact Huffman tree, saved by HUF1v06_writeCTable().
    `huffWeight` is destination buffer.
    @return : size read from `src`
*/
MEM_STATIC size_t HUF1v06_readStats(BYTE* huffWeight, size_t hwSize, U32* rankStats,
                            U32* nbSymbolsPtr, U32* tableLogPtr,
                            const void* src, size_t srcSize)
{
    U32 weightTotal;
    const BYTE* ip = (const BYTE*) src;
    size_t iSize;
    size_t oSize;

    if (!srcSize) return ERROR(srcSize_wrong);
    iSize = ip[0];
    //memset(huffWeight, 0, hwSize);   /* is not necessary, even though some analyzer complain ... */

    if (iSize >= 128)  { /* special header */
        if (iSize >= (242)) {  /* RLE */
            static U32 l[14] = { 1, 2, 3, 4, 7, 8, 15, 16, 31, 32, 63, 64, 127, 128 };
            oSize = l[iSize-242];
            memset(huffWeight, 1, hwSize);
            iSize = 0;
        }
        else {   /* Incompressible */
            oSize = iSize - 127;
            iSize = ((oSize+1)/2);
            if (iSize+1 > srcSize) return ERROR(srcSize_wrong);
            if (oSize >= hwSize) return ERROR(corruption_detected);
            ip += 1;
            {   U32 n;
                for (n=0; n<oSize; n+=2) {
                    huffWeight[n]   = ip[n/2] >> 4;
                    huffWeight[n+1] = ip[n/2] & 15;
    }   }   }   }
    else  {   /* header compressed with FSE1 (normal case) */
        if (iSize+1 > srcSize) return ERROR(srcSize_wrong);
        oSize = FSE1v06_decompress(huffWeight, hwSize-1, ip+1, iSize);   /* max (hwSize-1) values decoded, as last one is implied */
        if (FSE1v06_isError(oSize)) return oSize;
    }

    /* collect weight stats */
    memset(rankStats, 0, (HUF1v06_ABSOLUTEMAX_TABLELOG + 1) * sizeof(U32));
    weightTotal = 0;
    {   U32 n; for (n=0; n<oSize; n++) {
            if (huffWeight[n] >= HUF1v06_ABSOLUTEMAX_TABLELOG) return ERROR(corruption_detected);
            rankStats[huffWeight[n]]++;
            weightTotal += (1 << huffWeight[n]) >> 1;
    }   }
    if (weightTotal == 0) return ERROR(corruption_detected);

    /* get last non-null symbol weight (implied, total must be 2^n) */
    {   U32 const tableLog = BITv06_highbit32(weightTotal) + 1;
        if (tableLog > HUF1v06_ABSOLUTEMAX_TABLELOG) return ERROR(corruption_detected);
        *tableLogPtr = tableLog;
        /* determine last weight */
        {   U32 const total = 1 << tableLog;
            U32 const rest = total - weightTotal;
            U32 const verif = 1 << BITv06_highbit32(rest);
            U32 const lastWeight = BITv06_highbit32(rest) + 1;
            if (verif != rest) return ERROR(corruption_detected);    /* last value must be a clean power of 2 */
            huffWeight[oSize] = (BYTE)lastWeight;
            rankStats[lastWeight]++;
    }   }

    /* check tree construction validity */
    if ((rankStats[1] < 2) || (rankStats[1] & 1)) return ERROR(corruption_detected);   /* by construction : at least 2 elts of rank 1, must be even */

    /* results */
    *nbSymbolsPtr = (U32)(oSize+1);
    return iSize+1;
}



#if defined (__cplusplus)
}
#endif

#endif /* HUF1v06_STATIC_H */
/* ******************************************************************
   Huffman decoder, part of New Generation Entropy library
   Copyright (C) 2013-2016, Yann Collet.

   BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions are
   met:

       * Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
       * Redistributions in binary form must reproduce the above
   copyright notice, this list of conditions and the following disclaimer
   in the documentation and/or other materials provided with the
   distribution.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - FSE1+HUF1 source repository : https://github.com/Cyan4973/FiniteStateEntropy
    - Public forum : https://groups.google.com/forum/#!forum/lz4c
****************************************************************** */

/* **************************************************************
*  Compiler specifics
****************************************************************/
#if defined (__cplusplus) || (defined (__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L) /* C99 */)
/* inline is defined */
#elif defined(_MSC_VER)
#  define inline __inline
#else
#  define inline /* disable inline */
#endif


#ifdef _MSC_VER    /* Visual Studio */
#  pragma warning(disable : 4127)        /* disable: C4127: conditional expression is constant */
#endif



/* **************************************************************
*  Error Management
****************************************************************/
#define HUF1v06_STATIC_ASSERT(c) { enum { HUF1v06_static_assert = 1/(int)(!!(c)) }; }   /* use only *after* variable declarations */



/* *******************************************************
*  HUF1 : Huffman block decompression
*********************************************************/
typedef struct { BYTE byte; BYTE nbBits; } HUF1v06_DEltX2;   /* single-symbol decoding */

typedef struct { U16 sequence; BYTE nbBits; BYTE length; } HUF1v06_DEltX4;  /* double-symbols decoding */

typedef struct { BYTE symbol; BYTE weight; } sortedSymbol_t;



/*-***************************/
/*  single-symbol decoding   */
/*-***************************/

size_t HUF1v06_readDTableX2 (U16* DTable, const void* src, size_t srcSize)
{
    BYTE huffWeight[HUF1v06_MAX_SYMBOL_VALUE + 1];
    U32 rankVal[HUF1v06_ABSOLUTEMAX_TABLELOG + 1];   /* large enough for values from 0 to 16 */
    U32 tableLog = 0;
    size_t iSize;
    U32 nbSymbols = 0;
    U32 n;
    U32 nextRankStart;
    void* const dtPtr = DTable + 1;
    HUF1v06_DEltX2* const dt = (HUF1v06_DEltX2*)dtPtr;

    HUF1v06_STATIC_ASSERT(sizeof(HUF1v06_DEltX2) == sizeof(U16));   /* if compilation fails here, assertion is false */
    //memset(huffWeight, 0, sizeof(huffWeight));   /* is not necessary, even though some analyzer complain ... */

    iSize = HUF1v06_readStats(huffWeight, HUF1v06_MAX_SYMBOL_VALUE + 1, rankVal, &nbSymbols, &tableLog, src, srcSize);
    if (HUF1v06_isError(iSize)) return iSize;

    /* check result */
    if (tableLog > DTable[0]) return ERROR(tableLog_tooLarge);   /* DTable is too small */
    DTable[0] = (U16)tableLog;   /* maybe should separate sizeof allocated DTable, from used size of DTable, in case of re-use */

    /* Prepare ranks */
    nextRankStart = 0;
    for (n=1; n<tableLog+1; n++) {
        U32 current = nextRankStart;
        nextRankStart += (rankVal[n] << (n-1));
        rankVal[n] = current;
    }

    /* fill DTable */
    for (n=0; n<nbSymbols; n++) {
        const U32 w = huffWeight[n];
        const U32 length = (1 << w) >> 1;
        U32 i;
        HUF1v06_DEltX2 D;
        D.byte = (BYTE)n; D.nbBits = (BYTE)(tableLog + 1 - w);
        for (i = rankVal[w]; i < rankVal[w] + length; i++)
            dt[i] = D;
        rankVal[w] += length;
    }

    return iSize;
}


static BYTE HUF1v06_decodeSymbolX2(BITv06_DStream_t* Dstream, const HUF1v06_DEltX2* dt, const U32 dtLog)
{
    const size_t val = BITv06_lookBitsFast(Dstream, dtLog); /* note : dtLog >= 1 */
    const BYTE c = dt[val].byte;
    BITv06_skipBits(Dstream, dt[val].nbBits);
    return c;
}

#define HUF1v06_DECODE_SYMBOLX2_0(ptr, DStreamPtr) \
    *ptr++ = HUF1v06_decodeSymbolX2(DStreamPtr, dt, dtLog)

#define HUF1v06_DECODE_SYMBOLX2_1(ptr, DStreamPtr) \
    if (MEM_64bits() || (HUF1v06_MAX_TABLELOG<=12)) \
        HUF1v06_DECODE_SYMBOLX2_0(ptr, DStreamPtr)

#define HUF1v06_DECODE_SYMBOLX2_2(ptr, DStreamPtr) \
    if (MEM_64bits()) \
        HUF1v06_DECODE_SYMBOLX2_0(ptr, DStreamPtr)

static inline size_t HUF1v06_decodeStreamX2(BYTE* p, BITv06_DStream_t* const bitDPtr, BYTE* const pEnd, const HUF1v06_DEltX2* const dt, const U32 dtLog)
{
    BYTE* const pStart = p;

    /* up to 4 symbols at a time */
    while ((BITv06_reloadDStream(bitDPtr) == BITv06_DStream_unfinished) && (p <= pEnd-4)) {
        HUF1v06_DECODE_SYMBOLX2_2(p, bitDPtr);
        HUF1v06_DECODE_SYMBOLX2_1(p, bitDPtr);
        HUF1v06_DECODE_SYMBOLX2_2(p, bitDPtr);
        HUF1v06_DECODE_SYMBOLX2_0(p, bitDPtr);
    }

    /* closer to the end */
    while ((BITv06_reloadDStream(bitDPtr) == BITv06_DStream_unfinished) && (p < pEnd))
        HUF1v06_DECODE_SYMBOLX2_0(p, bitDPtr);

    /* no more data to retrieve from bitstream, hence no need to reload */
    while (p < pEnd)
        HUF1v06_DECODE_SYMBOLX2_0(p, bitDPtr);

    return pEnd-pStart;
}

size_t HUF1v06_decompress1X2_usingDTable(
          void* dst,  size_t dstSize,
    const void* cSrc, size_t cSrcSize,
    const U16* DTable)
{
    BYTE* op = (BYTE*)dst;
    BYTE* const oend = op + dstSize;
    const U32 dtLog = DTable[0];
    const void* dtPtr = DTable;
    const HUF1v06_DEltX2* const dt = ((const HUF1v06_DEltX2*)dtPtr)+1;
    BITv06_DStream_t bitD;

    { size_t const errorCode = BITv06_initDStream(&bitD, cSrc, cSrcSize);
      if (HUF1v06_isError(errorCode)) return errorCode; }

    HUF1v06_decodeStreamX2(op, &bitD, oend, dt, dtLog);

    /* check */
    if (!BITv06_endOfDStream(&bitD)) return ERROR(corruption_detected);

    return dstSize;
}

size_t HUF1v06_decompress1X2 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize)
{
    HUF1v06_CREATE_STATIC_DTABLEX2(DTable, HUF1v06_MAX_TABLELOG);
    const BYTE* ip = (const BYTE*) cSrc;

    size_t const errorCode = HUF1v06_readDTableX2 (DTable, cSrc, cSrcSize);
    if (HUF1v06_isError(errorCode)) return errorCode;
    if (errorCode >= cSrcSize) return ERROR(srcSize_wrong);
    ip += errorCode;
    cSrcSize -= errorCode;

    return HUF1v06_decompress1X2_usingDTable (dst, dstSize, ip, cSrcSize, DTable);
}


size_t HUF1v06_decompress4X2_usingDTable(
          void* dst,  size_t dstSize,
    const void* cSrc, size_t cSrcSize,
    const U16* DTable)
{
    /* Check */
    if (cSrcSize < 10) return ERROR(corruption_detected);  /* strict minimum : jump table + 1 byte per stream */

    {   const BYTE* const istart = (const BYTE*) cSrc;
        BYTE* const ostart = (BYTE*) dst;
        BYTE* const oend = ostart + dstSize;
        const void* const dtPtr = DTable;
        const HUF1v06_DEltX2* const dt = ((const HUF1v06_DEltX2*)dtPtr) +1;
        const U32 dtLog = DTable[0];
        size_t errorCode;

        /* Init */
        BITv06_DStream_t bitD1;
        BITv06_DStream_t bitD2;
        BITv06_DStream_t bitD3;
        BITv06_DStream_t bitD4;
        const size_t length1 = MEM_readLE16(istart);
        const size_t length2 = MEM_readLE16(istart+2);
        const size_t length3 = MEM_readLE16(istart+4);
        size_t length4;
        const BYTE* const istart1 = istart + 6;  /* jumpTable */
        const BYTE* const istart2 = istart1 + length1;
        const BYTE* const istart3 = istart2 + length2;
        const BYTE* const istart4 = istart3 + length3;
        const size_t segmentSize = (dstSize+3) / 4;
        BYTE* const opStart2 = ostart + segmentSize;
        BYTE* const opStart3 = opStart2 + segmentSize;
        BYTE* const opStart4 = opStart3 + segmentSize;
        BYTE* op1 = ostart;
        BYTE* op2 = opStart2;
        BYTE* op3 = opStart3;
        BYTE* op4 = opStart4;
        U32 endSignal;

        length4 = cSrcSize - (length1 + length2 + length3 + 6);
        if (length4 > cSrcSize) return ERROR(corruption_detected);   /* overflow */
        errorCode = BITv06_initDStream(&bitD1, istart1, length1);
        if (HUF1v06_isError(errorCode)) return errorCode;
        errorCode = BITv06_initDStream(&bitD2, istart2, length2);
        if (HUF1v06_isError(errorCode)) return errorCode;
        errorCode = BITv06_initDStream(&bitD3, istart3, length3);
        if (HUF1v06_isError(errorCode)) return errorCode;
        errorCode = BITv06_initDStream(&bitD4, istart4, length4);
        if (HUF1v06_isError(errorCode)) return errorCode;

        /* 16-32 symbols per loop (4-8 symbols per stream) */
        endSignal = BITv06_reloadDStream(&bitD1) | BITv06_reloadDStream(&bitD2) | BITv06_reloadDStream(&bitD3) | BITv06_reloadDStream(&bitD4);
        for ( ; (endSignal==BITv06_DStream_unfinished) && (op4<(oend-7)) ; ) {
            HUF1v06_DECODE_SYMBOLX2_2(op1, &bitD1);
            HUF1v06_DECODE_SYMBOLX2_2(op2, &bitD2);
            HUF1v06_DECODE_SYMBOLX2_2(op3, &bitD3);
            HUF1v06_DECODE_SYMBOLX2_2(op4, &bitD4);
            HUF1v06_DECODE_SYMBOLX2_1(op1, &bitD1);
            HUF1v06_DECODE_SYMBOLX2_1(op2, &bitD2);
            HUF1v06_DECODE_SYMBOLX2_1(op3, &bitD3);
            HUF1v06_DECODE_SYMBOLX2_1(op4, &bitD4);
            HUF1v06_DECODE_SYMBOLX2_2(op1, &bitD1);
            HUF1v06_DECODE_SYMBOLX2_2(op2, &bitD2);
            HUF1v06_DECODE_SYMBOLX2_2(op3, &bitD3);
            HUF1v06_DECODE_SYMBOLX2_2(op4, &bitD4);
            HUF1v06_DECODE_SYMBOLX2_0(op1, &bitD1);
            HUF1v06_DECODE_SYMBOLX2_0(op2, &bitD2);
            HUF1v06_DECODE_SYMBOLX2_0(op3, &bitD3);
            HUF1v06_DECODE_SYMBOLX2_0(op4, &bitD4);
            endSignal = BITv06_reloadDStream(&bitD1) | BITv06_reloadDStream(&bitD2) | BITv06_reloadDStream(&bitD3) | BITv06_reloadDStream(&bitD4);
        }

        /* check corruption */
        if (op1 > opStart2) return ERROR(corruption_detected);
        if (op2 > opStart3) return ERROR(corruption_detected);
        if (op3 > opStart4) return ERROR(corruption_detected);
        /* note : op4 supposed already verified within main loop */

        /* finish bitStreams one by one */
        HUF1v06_decodeStreamX2(op1, &bitD1, opStart2, dt, dtLog);
        HUF1v06_decodeStreamX2(op2, &bitD2, opStart3, dt, dtLog);
        HUF1v06_decodeStreamX2(op3, &bitD3, opStart4, dt, dtLog);
        HUF1v06_decodeStreamX2(op4, &bitD4, oend,     dt, dtLog);

        /* check */
        endSignal = BITv06_endOfDStream(&bitD1) & BITv06_endOfDStream(&bitD2) & BITv06_endOfDStream(&bitD3) & BITv06_endOfDStream(&bitD4);
        if (!endSignal) return ERROR(corruption_detected);

        /* decoded size */
        return dstSize;
    }
}


size_t HUF1v06_decompress4X2 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize)
{
    HUF1v06_CREATE_STATIC_DTABLEX2(DTable, HUF1v06_MAX_TABLELOG);
    const BYTE* ip = (const BYTE*) cSrc;

    size_t const errorCode = HUF1v06_readDTableX2 (DTable, cSrc, cSrcSize);
    if (HUF1v06_isError(errorCode)) return errorCode;
    if (errorCode >= cSrcSize) return ERROR(srcSize_wrong);
    ip += errorCode;
    cSrcSize -= errorCode;

    return HUF1v06_decompress4X2_usingDTable (dst, dstSize, ip, cSrcSize, DTable);
}


/* *************************/
/* double-symbols decoding */
/* *************************/

static void HUF1v06_fillDTableX4Level2(HUF1v06_DEltX4* DTable, U32 sizeLog, const U32 consumed,
                           const U32* rankValOrigin, const int minWeight,
                           const sortedSymbol_t* sortedSymbols, const U32 sortedListSize,
                           U32 nbBitsBaseline, U16 baseSeq)
{
    HUF1v06_DEltX4 DElt;
    U32 rankVal[HUF1v06_ABSOLUTEMAX_TABLELOG + 1];

    /* get pre-calculated rankVal */
    memcpy(rankVal, rankValOrigin, sizeof(rankVal));

    /* fill skipped values */
    if (minWeight>1) {
        U32 i, skipSize = rankVal[minWeight];
        MEM_writeLE16(&(DElt.sequence), baseSeq);
        DElt.nbBits   = (BYTE)(consumed);
        DElt.length   = 1;
        for (i = 0; i < skipSize; i++)
            DTable[i] = DElt;
    }

    /* fill DTable */
    { U32 s; for (s=0; s<sortedListSize; s++) {   /* note : sortedSymbols already skipped */
        const U32 symbol = sortedSymbols[s].symbol;
        const U32 weight = sortedSymbols[s].weight;
        const U32 nbBits = nbBitsBaseline - weight;
        const U32 length = 1 << (sizeLog-nbBits);
        const U32 start = rankVal[weight];
        U32 i = start;
        const U32 end = start + length;

        MEM_writeLE16(&(DElt.sequence), (U16)(baseSeq + (symbol << 8)));
        DElt.nbBits = (BYTE)(nbBits + consumed);
        DElt.length = 2;
        do { DTable[i++] = DElt; } while (i<end);   /* since length >= 1 */

        rankVal[weight] += length;
    }}
}

typedef U32 rankVal_t[HUF1v06_ABSOLUTEMAX_TABLELOG][HUF1v06_ABSOLUTEMAX_TABLELOG + 1];

static void HUF1v06_fillDTableX4(HUF1v06_DEltX4* DTable, const U32 targetLog,
                           const sortedSymbol_t* sortedList, const U32 sortedListSize,
                           const U32* rankStart, rankVal_t rankValOrigin, const U32 maxWeight,
                           const U32 nbBitsBaseline)
{
    U32 rankVal[HUF1v06_ABSOLUTEMAX_TABLELOG + 1];
    const int scaleLog = nbBitsBaseline - targetLog;   /* note : targetLog >= srcLog, hence scaleLog <= 1 */
    const U32 minBits  = nbBitsBaseline - maxWeight;
    U32 s;

    memcpy(rankVal, rankValOrigin, sizeof(rankVal));

    /* fill DTable */
    for (s=0; s<sortedListSize; s++) {
        const U16 symbol = sortedList[s].symbol;
        const U32 weight = sortedList[s].weight;
        const U32 nbBits = nbBitsBaseline - weight;
        const U32 start = rankVal[weight];
        const U32 length = 1 << (targetLog-nbBits);

        if (targetLog-nbBits >= minBits) {   /* enough room for a second symbol */
            U32 sortedRank;
            int minWeight = nbBits + scaleLog;
            if (minWeight < 1) minWeight = 1;
            sortedRank = rankStart[minWeight];
            HUF1v06_fillDTableX4Level2(DTable+start, targetLog-nbBits, nbBits,
                           rankValOrigin[nbBits], minWeight,
                           sortedList+sortedRank, sortedListSize-sortedRank,
                           nbBitsBaseline, symbol);
        } else {
            HUF1v06_DEltX4 DElt;
            MEM_writeLE16(&(DElt.sequence), symbol);
            DElt.nbBits = (BYTE)(nbBits);
            DElt.length = 1;
            {   U32 u;
                const U32 end = start + length;
                for (u = start; u < end; u++) DTable[u] = DElt;
        }   }
        rankVal[weight] += length;
    }
}

size_t HUF1v06_readDTableX4 (U32* DTable, const void* src, size_t srcSize)
{
    BYTE weightList[HUF1v06_MAX_SYMBOL_VALUE + 1];
    sortedSymbol_t sortedSymbol[HUF1v06_MAX_SYMBOL_VALUE + 1];
    U32 rankStats[HUF1v06_ABSOLUTEMAX_TABLELOG + 1] = { 0 };
    U32 rankStart0[HUF1v06_ABSOLUTEMAX_TABLELOG + 2] = { 0 };
    U32* const rankStart = rankStart0+1;
    rankVal_t rankVal;
    U32 tableLog, maxW, sizeOfSort, nbSymbols;
    const U32 memLog = DTable[0];
    size_t iSize;
    void* dtPtr = DTable;
    HUF1v06_DEltX4* const dt = ((HUF1v06_DEltX4*)dtPtr) + 1;

    HUF1v06_STATIC_ASSERT(sizeof(HUF1v06_DEltX4) == sizeof(U32));   /* if compilation fails here, assertion is false */
    if (memLog > HUF1v06_ABSOLUTEMAX_TABLELOG) return ERROR(tableLog_tooLarge);
    //memset(weightList, 0, sizeof(weightList));   /* is not necessary, even though some analyzer complain ... */

    iSize = HUF1v06_readStats(weightList, HUF1v06_MAX_SYMBOL_VALUE + 1, rankStats, &nbSymbols, &tableLog, src, srcSize);
    if (HUF1v06_isError(iSize)) return iSize;

    /* check result */
    if (tableLog > memLog) return ERROR(tableLog_tooLarge);   /* DTable can't fit code depth */

    /* find maxWeight */
    for (maxW = tableLog; rankStats[maxW]==0; maxW--) {}  /* necessarily finds a solution before 0 */

    /* Get start index of each weight */
    {   U32 w, nextRankStart = 0;
        for (w=1; w<maxW+1; w++) {
            U32 current = nextRankStart;
            nextRankStart += rankStats[w];
            rankStart[w] = current;
        }
        rankStart[0] = nextRankStart;   /* put all 0w symbols at the end of sorted list*/
        sizeOfSort = nextRankStart;
    }

    /* sort symbols by weight */
    {   U32 s;
        for (s=0; s<nbSymbols; s++) {
            U32 const w = weightList[s];
            U32 const r = rankStart[w]++;
            sortedSymbol[r].symbol = (BYTE)s;
            sortedSymbol[r].weight = (BYTE)w;
        }
        rankStart[0] = 0;   /* forget 0w symbols; this is beginning of weight(1) */
    }

    /* Build rankVal */
    {   U32* const rankVal0 = rankVal[0];
        {   int const rescale = (memLog-tableLog) - 1;   /* tableLog <= memLog */
            U32 nextRankVal = 0;
            U32 w;
            for (w=1; w<maxW+1; w++) {
                U32 current = nextRankVal;
                nextRankVal += rankStats[w] << (w+rescale);
                rankVal0[w] = current;
        }   }
        {   U32 const minBits = tableLog+1 - maxW;
            U32 consumed;
            for (consumed = minBits; consumed < memLog - minBits + 1; consumed++) {
                U32* const rankValPtr = rankVal[consumed];
                U32 w;
                for (w = 1; w < maxW+1; w++) {
                    rankValPtr[w] = rankVal0[w] >> consumed;
    }   }   }   }

    HUF1v06_fillDTableX4(dt, memLog,
                   sortedSymbol, sizeOfSort,
                   rankStart0, rankVal, maxW,
                   tableLog+1);

    return iSize;
}


static U32 HUF1v06_decodeSymbolX4(void* op, BITv06_DStream_t* DStream, const HUF1v06_DEltX4* dt, const U32 dtLog)
{
    const size_t val = BITv06_lookBitsFast(DStream, dtLog);   /* note : dtLog >= 1 */
    memcpy(op, dt+val, 2);
    BITv06_skipBits(DStream, dt[val].nbBits);
    return dt[val].length;
}

static U32 HUF1v06_decodeLastSymbolX4(void* op, BITv06_DStream_t* DStream, const HUF1v06_DEltX4* dt, const U32 dtLog)
{
    const size_t val = BITv06_lookBitsFast(DStream, dtLog);   /* note : dtLog >= 1 */
    memcpy(op, dt+val, 1);
    if (dt[val].length==1) BITv06_skipBits(DStream, dt[val].nbBits);
    else {
        if (DStream->bitsConsumed < (sizeof(DStream->bitContainer)*8)) {
            BITv06_skipBits(DStream, dt[val].nbBits);
            if (DStream->bitsConsumed > (sizeof(DStream->bitContainer)*8))
                DStream->bitsConsumed = (sizeof(DStream->bitContainer)*8);   /* ugly hack; works only because it's the last symbol. Note : can't easily extract nbBits from just this symbol */
    }   }
    return 1;
}


#define HUF1v06_DECODE_SYMBOLX4_0(ptr, DStreamPtr) \
    ptr += HUF1v06_decodeSymbolX4(ptr, DStreamPtr, dt, dtLog)

#define HUF1v06_DECODE_SYMBOLX4_1(ptr, DStreamPtr) \
    if (MEM_64bits() || (HUF1v06_MAX_TABLELOG<=12)) \
        ptr += HUF1v06_decodeSymbolX4(ptr, DStreamPtr, dt, dtLog)

#define HUF1v06_DECODE_SYMBOLX4_2(ptr, DStreamPtr) \
    if (MEM_64bits()) \
        ptr += HUF1v06_decodeSymbolX4(ptr, DStreamPtr, dt, dtLog)

static inline size_t HUF1v06_decodeStreamX4(BYTE* p, BITv06_DStream_t* bitDPtr, BYTE* const pEnd, const HUF1v06_DEltX4* const dt, const U32 dtLog)
{
    BYTE* const pStart = p;

    /* up to 8 symbols at a time */
    while ((BITv06_reloadDStream(bitDPtr) == BITv06_DStream_unfinished) && (p < pEnd-7)) {
        HUF1v06_DECODE_SYMBOLX4_2(p, bitDPtr);
        HUF1v06_DECODE_SYMBOLX4_1(p, bitDPtr);
        HUF1v06_DECODE_SYMBOLX4_2(p, bitDPtr);
        HUF1v06_DECODE_SYMBOLX4_0(p, bitDPtr);
    }

    /* closer to the end */
    while ((BITv06_reloadDStream(bitDPtr) == BITv06_DStream_unfinished) && (p <= pEnd-2))
        HUF1v06_DECODE_SYMBOLX4_0(p, bitDPtr);

    while (p <= pEnd-2)
        HUF1v06_DECODE_SYMBOLX4_0(p, bitDPtr);   /* no need to reload : reached the end of DStream */

    if (p < pEnd)
        p += HUF1v06_decodeLastSymbolX4(p, bitDPtr, dt, dtLog);

    return p-pStart;
}


size_t HUF1v06_decompress1X4_usingDTable(
          void* dst,  size_t dstSize,
    const void* cSrc, size_t cSrcSize,
    const U32* DTable)
{
    const BYTE* const istart = (const BYTE*) cSrc;
    BYTE* const ostart = (BYTE*) dst;
    BYTE* const oend = ostart + dstSize;

    const U32 dtLog = DTable[0];
    const void* const dtPtr = DTable;
    const HUF1v06_DEltX4* const dt = ((const HUF1v06_DEltX4*)dtPtr) +1;

    /* Init */
    BITv06_DStream_t bitD;
    { size_t const errorCode = BITv06_initDStream(&bitD, istart, cSrcSize);
      if (HUF1v06_isError(errorCode)) return errorCode; }

    /* decode */
    HUF1v06_decodeStreamX4(ostart, &bitD, oend, dt, dtLog);

    /* check */
    if (!BITv06_endOfDStream(&bitD)) return ERROR(corruption_detected);

    /* decoded size */
    return dstSize;
}

size_t HUF1v06_decompress1X4 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize)
{
    HUF1v06_CREATE_STATIC_DTABLEX4(DTable, HUF1v06_MAX_TABLELOG);
    const BYTE* ip = (const BYTE*) cSrc;

    size_t const hSize = HUF1v06_readDTableX4 (DTable, cSrc, cSrcSize);
    if (HUF1v06_isError(hSize)) return hSize;
    if (hSize >= cSrcSize) return ERROR(srcSize_wrong);
    ip += hSize;
    cSrcSize -= hSize;

    return HUF1v06_decompress1X4_usingDTable (dst, dstSize, ip, cSrcSize, DTable);
}

size_t HUF1v06_decompress4X4_usingDTable(
          void* dst,  size_t dstSize,
    const void* cSrc, size_t cSrcSize,
    const U32* DTable)
{
    if (cSrcSize < 10) return ERROR(corruption_detected);   /* strict minimum : jump table + 1 byte per stream */

    {   const BYTE* const istart = (const BYTE*) cSrc;
        BYTE* const ostart = (BYTE*) dst;
        BYTE* const oend = ostart + dstSize;
        const void* const dtPtr = DTable;
        const HUF1v06_DEltX4* const dt = ((const HUF1v06_DEltX4*)dtPtr) +1;
        const U32 dtLog = DTable[0];
        size_t errorCode;

        /* Init */
        BITv06_DStream_t bitD1;
        BITv06_DStream_t bitD2;
        BITv06_DStream_t bitD3;
        BITv06_DStream_t bitD4;
        const size_t length1 = MEM_readLE16(istart);
        const size_t length2 = MEM_readLE16(istart+2);
        const size_t length3 = MEM_readLE16(istart+4);
        size_t length4;
        const BYTE* const istart1 = istart + 6;  /* jumpTable */
        const BYTE* const istart2 = istart1 + length1;
        const BYTE* const istart3 = istart2 + length2;
        const BYTE* const istart4 = istart3 + length3;
        const size_t segmentSize = (dstSize+3) / 4;
        BYTE* const opStart2 = ostart + segmentSize;
        BYTE* const opStart3 = opStart2 + segmentSize;
        BYTE* const opStart4 = opStart3 + segmentSize;
        BYTE* op1 = ostart;
        BYTE* op2 = opStart2;
        BYTE* op3 = opStart3;
        BYTE* op4 = opStart4;
        U32 endSignal;

        length4 = cSrcSize - (length1 + length2 + length3 + 6);
        if (length4 > cSrcSize) return ERROR(corruption_detected);   /* overflow */
        errorCode = BITv06_initDStream(&bitD1, istart1, length1);
        if (HUF1v06_isError(errorCode)) return errorCode;
        errorCode = BITv06_initDStream(&bitD2, istart2, length2);
        if (HUF1v06_isError(errorCode)) return errorCode;
        errorCode = BITv06_initDStream(&bitD3, istart3, length3);
        if (HUF1v06_isError(errorCode)) return errorCode;
        errorCode = BITv06_initDStream(&bitD4, istart4, length4);
        if (HUF1v06_isError(errorCode)) return errorCode;

        /* 16-32 symbols per loop (4-8 symbols per stream) */
        endSignal = BITv06_reloadDStream(&bitD1) | BITv06_reloadDStream(&bitD2) | BITv06_reloadDStream(&bitD3) | BITv06_reloadDStream(&bitD4);
        for ( ; (endSignal==BITv06_DStream_unfinished) && (op4<(oend-7)) ; ) {
            HUF1v06_DECODE_SYMBOLX4_2(op1, &bitD1);
            HUF1v06_DECODE_SYMBOLX4_2(op2, &bitD2);
            HUF1v06_DECODE_SYMBOLX4_2(op3, &bitD3);
            HUF1v06_DECODE_SYMBOLX4_2(op4, &bitD4);
            HUF1v06_DECODE_SYMBOLX4_1(op1, &bitD1);
            HUF1v06_DECODE_SYMBOLX4_1(op2, &bitD2);
            HUF1v06_DECODE_SYMBOLX4_1(op3, &bitD3);
            HUF1v06_DECODE_SYMBOLX4_1(op4, &bitD4);
            HUF1v06_DECODE_SYMBOLX4_2(op1, &bitD1);
            HUF1v06_DECODE_SYMBOLX4_2(op2, &bitD2);
            HUF1v06_DECODE_SYMBOLX4_2(op3, &bitD3);
            HUF1v06_DECODE_SYMBOLX4_2(op4, &bitD4);
            HUF1v06_DECODE_SYMBOLX4_0(op1, &bitD1);
            HUF1v06_DECODE_SYMBOLX4_0(op2, &bitD2);
            HUF1v06_DECODE_SYMBOLX4_0(op3, &bitD3);
            HUF1v06_DECODE_SYMBOLX4_0(op4, &bitD4);

            endSignal = BITv06_reloadDStream(&bitD1) | BITv06_reloadDStream(&bitD2) | BITv06_reloadDStream(&bitD3) | BITv06_reloadDStream(&bitD4);
        }

        /* check corruption */
        if (op1 > opStart2) return ERROR(corruption_detected);
        if (op2 > opStart3) return ERROR(corruption_detected);
        if (op3 > opStart4) return ERROR(corruption_detected);
        /* note : op4 supposed already verified within main loop */

        /* finish bitStreams one by one */
        HUF1v06_decodeStreamX4(op1, &bitD1, opStart2, dt, dtLog);
        HUF1v06_decodeStreamX4(op2, &bitD2, opStart3, dt, dtLog);
        HUF1v06_decodeStreamX4(op3, &bitD3, opStart4, dt, dtLog);
        HUF1v06_decodeStreamX4(op4, &bitD4, oend,     dt, dtLog);

        /* check */
        endSignal = BITv06_endOfDStream(&bitD1) & BITv06_endOfDStream(&bitD2) & BITv06_endOfDStream(&bitD3) & BITv06_endOfDStream(&bitD4);
        if (!endSignal) return ERROR(corruption_detected);

        /* decoded size */
        return dstSize;
    }
}


size_t HUF1v06_decompress4X4 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize)
{
    HUF1v06_CREATE_STATIC_DTABLEX4(DTable, HUF1v06_MAX_TABLELOG);
    const BYTE* ip = (const BYTE*) cSrc;

    size_t hSize = HUF1v06_readDTableX4 (DTable, cSrc, cSrcSize);
    if (HUF1v06_isError(hSize)) return hSize;
    if (hSize >= cSrcSize) return ERROR(srcSize_wrong);
    ip += hSize;
    cSrcSize -= hSize;

    return HUF1v06_decompress4X4_usingDTable (dst, dstSize, ip, cSrcSize, DTable);
}




/* ********************************/
/* Generic decompression selector */
/* ********************************/

typedef struct { U32 tableTime; U32 decode256Time; } algo_time_t;
static const algo_time_t algoTime[16 /* Quantization */][3 /* single, double, quad */] =
{
    /* single, double, quad */
    {{0,0}, {1,1}, {2,2}},  /* Q==0 : impossible */
    {{0,0}, {1,1}, {2,2}},  /* Q==1 : impossible */
    {{  38,130}, {1313, 74}, {2151, 38}},   /* Q == 2 : 12-18% */
    {{ 448,128}, {1353, 74}, {2238, 41}},   /* Q == 3 : 18-25% */
    {{ 556,128}, {1353, 74}, {2238, 47}},   /* Q == 4 : 25-32% */
    {{ 714,128}, {1418, 74}, {2436, 53}},   /* Q == 5 : 32-38% */
    {{ 883,128}, {1437, 74}, {2464, 61}},   /* Q == 6 : 38-44% */
    {{ 897,128}, {1515, 75}, {2622, 68}},   /* Q == 7 : 44-50% */
    {{ 926,128}, {1613, 75}, {2730, 75}},   /* Q == 8 : 50-56% */
    {{ 947,128}, {1729, 77}, {3359, 77}},   /* Q == 9 : 56-62% */
    {{1107,128}, {2083, 81}, {4006, 84}},   /* Q ==10 : 62-69% */
    {{1177,128}, {2379, 87}, {4785, 88}},   /* Q ==11 : 69-75% */
    {{1242,128}, {2415, 93}, {5155, 84}},   /* Q ==12 : 75-81% */
    {{1349,128}, {2644,106}, {5260,106}},   /* Q ==13 : 81-87% */
    {{1455,128}, {2422,124}, {4174,124}},   /* Q ==14 : 87-93% */
    {{ 722,128}, {1891,145}, {1936,146}},   /* Q ==15 : 93-99% */
};

typedef size_t (*decompressionAlgo)(void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);

size_t HUF1v06_decompress (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize)
{
    static const decompressionAlgo decompress[3] = { HUF1v06_decompress4X2, HUF1v06_decompress4X4, NULL };
    U32 Dtime[3];   /* decompression time estimation */

    /* validation checks */
    if (dstSize == 0) return ERROR(dstSize_tooSmall);
    if (cSrcSize > dstSize) return ERROR(corruption_detected);   /* invalid */
    if (cSrcSize == dstSize) { memcpy(dst, cSrc, dstSize); return dstSize; }   /* not compressed */
    if (cSrcSize == 1) { memset(dst, *(const BYTE*)cSrc, dstSize); return dstSize; }   /* RLE */

    /* decoder timing evaluation */
    {   U32 const Q = (U32)(cSrcSize * 16 / dstSize);   /* Q < 16 since dstSize > cSrcSize */
        U32 const D256 = (U32)(dstSize >> 8);
        U32 n; for (n=0; n<3; n++)
            Dtime[n] = algoTime[Q][n].tableTime + (algoTime[Q][n].decode256Time * D256);
    }

    Dtime[1] += Dtime[1] >> 4; Dtime[2] += Dtime[2] >> 3; /* advantage to algorithms using less memory, for cache eviction */

    {   U32 algoNb = 0;
        if (Dtime[1] < Dtime[0]) algoNb = 1;
        // if (Dtime[2] < Dtime[algoNb]) algoNb = 2;   /* current speed of HUF1v06_decompress4X6 is not good */
        return decompress[algoNb](dst, dstSize, cSrc, cSrcSize);
    }

    //return HUF1v06_decompress4X2(dst, dstSize, cSrc, cSrcSize);   /* multi-streams single-symbol decoding */
    //return HUF1v06_decompress4X4(dst, dstSize, cSrc, cSrcSize);   /* multi-streams double-symbols decoding */
    //return HUF1v06_decompress4X6(dst, dstSize, cSrc, cSrcSize);   /* multi-streams quad-symbols decoding */
}
/*
    Common functions of Zstd compression library
    Copyright (C) 2015-2016, Yann Collet.

    BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:
    * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following disclaimer
    in the documentation and/or other materials provided with the
    distribution.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - zstd homepage : http://www.zstd.net/
*/


/*-****************************************
*  Version
******************************************/

/*-****************************************
*  ZSTD1 Error Management
******************************************/
/*! ZSTD1v06_isError() :
*   tells if a return value is an error code */
unsigned ZSTD1v06_isError(size_t code) { return ERR_isError(code); }

/*! ZSTD1v06_getErrorName() :
*   provides error code string from function result (useful for debugging) */
const char* ZSTD1v06_getErrorName(size_t code) { return ERR_getErrorName(code); }


/* **************************************************************
*  ZBUFF1 Error Management
****************************************************************/
unsigned ZBUFF1v06_isError(size_t errorCode) { return ERR_isError(errorCode); }

const char* ZBUFF1v06_getErrorName(size_t errorCode) { return ERR_getErrorName(errorCode); }
/*
    zstd - standard compression library
    Copyright (C) 2014-2016, Yann Collet.

    BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:
    * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following disclaimer
    in the documentation and/or other materials provided with the
    distribution.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - zstd homepage : http://www.zstd.net
*/

/* ***************************************************************
*  Tuning parameters
*****************************************************************/
/*!
 * HEAPMODE :
 * Select how default decompression function ZSTD1v06_decompress() will allocate memory,
 * in memory stack (0), or in memory heap (1, requires malloc())
 */
#ifndef ZSTD1v06_HEAPMODE
#  define ZSTD1v06_HEAPMODE 1
#endif



/*-*******************************************************
*  Compiler specifics
*********************************************************/
#ifdef _MSC_VER    /* Visual Studio */
#  include <intrin.h>                    /* For Visual 2005 */
#  pragma warning(disable : 4127)        /* disable: C4127: conditional expression is constant */
#  pragma warning(disable : 4324)        /* disable: C4324: padded structure */
#endif


/*-*************************************
*  Macros
***************************************/
#define ZSTD1v06_isError ERR_isError   /* for inlining */
#define FSE1v06_isError  ERR_isError
#define HUF1v06_isError  ERR_isError


/*_*******************************************************
*  Memory operations
**********************************************************/
static void ZSTD1v06_copy4(void* dst, const void* src) { memcpy(dst, src, 4); }


/*-*************************************************************
*   Context management
***************************************************************/
typedef enum { ZSTD1ds_getFrameHeaderSize, ZSTD1ds_decodeFrameHeader,
               ZSTD1ds_decodeBlockHeader, ZSTD1ds_decompressBlock } ZSTD1v06_dStage;

struct ZSTD1v06_DCtx_s
{
    FSE1v06_DTable LLTable[FSE1v06_DTABLE_SIZE_U32(LLFSE1Log)];
    FSE1v06_DTable OffTable[FSE1v06_DTABLE_SIZE_U32(OffFSE1Log)];
    FSE1v06_DTable MLTable[FSE1v06_DTABLE_SIZE_U32(MLFSE1Log)];
    unsigned   hufTableX4[HUF1v06_DTABLE_SIZE(HufLog)];
    const void* previousDstEnd;
    const void* base;
    const void* vBase;
    const void* dictEnd;
    size_t expected;
    size_t headerSize;
    ZSTD1v06_frameParams fParams;
    blockType_t bType;   /* used in ZSTD1v06_decompressContinue(), to transfer blockType between header decoding and block decoding stages */
    ZSTD1v06_dStage stage;
    U32 flagRepeatTable;
    const BYTE* litPtr;
    size_t litSize;
    BYTE litBuffer[ZSTD1v06_BLOCKSIZE_MAX + WILDCOPY_OVERLENGTH];
    BYTE headerBuffer[ZSTD1v06_FRAMEHEADERSIZE_MAX];
};  /* typedef'd to ZSTD1v06_DCtx within "zstd_static.h" */

size_t ZSTD1v06_sizeofDCtx (void) { return sizeof(ZSTD1v06_DCtx); }   /* non published interface */

size_t ZSTD1v06_decompressBegin(ZSTD1v06_DCtx* dctx)
{
    dctx->expected = ZSTD1v06_frameHeaderSize_min;
    dctx->stage = ZSTD1ds_getFrameHeaderSize;
    dctx->previousDstEnd = NULL;
    dctx->base = NULL;
    dctx->vBase = NULL;
    dctx->dictEnd = NULL;
    dctx->hufTableX4[0] = HufLog;
    dctx->flagRepeatTable = 0;
    return 0;
}

ZSTD1v06_DCtx* ZSTD1v06_createDCtx(void)
{
    ZSTD1v06_DCtx* dctx = (ZSTD1v06_DCtx*)malloc(sizeof(ZSTD1v06_DCtx));
    if (dctx==NULL) return NULL;
    ZSTD1v06_decompressBegin(dctx);
    return dctx;
}

size_t ZSTD1v06_freeDCtx(ZSTD1v06_DCtx* dctx)
{
    free(dctx);
    return 0;   /* reserved as a potential error code in the future */
}

void ZSTD1v06_copyDCtx(ZSTD1v06_DCtx* dstDCtx, const ZSTD1v06_DCtx* srcDCtx)
{
    memcpy(dstDCtx, srcDCtx,
           sizeof(ZSTD1v06_DCtx) - (ZSTD1v06_BLOCKSIZE_MAX+WILDCOPY_OVERLENGTH + ZSTD1v06_frameHeaderSize_max));  /* no need to copy workspace */
}


/*-*************************************************************
*   Decompression section
***************************************************************/

/* Frame format description
   Frame Header -  [ Block Header - Block ] - Frame End
   1) Frame Header
      - 4 bytes - Magic Number : ZSTD1v06_MAGICNUMBER (defined within zstd_static.h)
      - 1 byte  - Frame Descriptor
   2) Block Header
      - 3 bytes, starting with a 2-bits descriptor
                 Uncompressed, Compressed, Frame End, unused
   3) Block
      See Block Format Description
   4) Frame End
      - 3 bytes, compatible with Block Header
*/


/* Frame descriptor

   1 byte, using :
   bit 0-3 : windowLog - ZSTD1v06_WINDOWLOG_ABSOLUTEMIN   (see zstd_internal.h)
   bit 4   : minmatch 4(0) or 3(1)
   bit 5   : reserved (must be zero)
   bit 6-7 : Frame content size : unknown, 1 byte, 2 bytes, 8 bytes

   Optional : content size (0, 1, 2 or 8 bytes)
   0 : unknown
   1 : 0-255 bytes
   2 : 256 - 65535+256
   8 : up to 16 exa
*/


/* Compressed Block, format description

   Block = Literal Section - Sequences Section
   Prerequisite : size of (compressed) block, maximum size of regenerated data

   1) Literal Section

   1.1) Header : 1-5 bytes
        flags: 2 bits
            00 compressed by Huff0
            01 unused
            10 is Raw (uncompressed)
            11 is Rle
            Note : using 01 => Huff0 with precomputed table ?
            Note : delta map ? => compressed ?

   1.1.1) Huff0-compressed literal block : 3-5 bytes
            srcSize < 1 KB => 3 bytes (2-2-10-10) => single stream
            srcSize < 1 KB => 3 bytes (2-2-10-10)
            srcSize < 16KB => 4 bytes (2-2-14-14)
            else           => 5 bytes (2-2-18-18)
            big endian convention

   1.1.2) Raw (uncompressed) literal block header : 1-3 bytes
        size :  5 bits: (IS_RAW<<6) + (0<<4) + size
               12 bits: (IS_RAW<<6) + (2<<4) + (size>>8)
                        size&255
               20 bits: (IS_RAW<<6) + (3<<4) + (size>>16)
                        size>>8&255
                        size&255

   1.1.3) Rle (repeated single byte) literal block header : 1-3 bytes
        size :  5 bits: (IS_RLE<<6) + (0<<4) + size
               12 bits: (IS_RLE<<6) + (2<<4) + (size>>8)
                        size&255
               20 bits: (IS_RLE<<6) + (3<<4) + (size>>16)
                        size>>8&255
                        size&255

   1.1.4) Huff0-compressed literal block, using precomputed CTables : 3-5 bytes
            srcSize < 1 KB => 3 bytes (2-2-10-10) => single stream
            srcSize < 1 KB => 3 bytes (2-2-10-10)
            srcSize < 16KB => 4 bytes (2-2-14-14)
            else           => 5 bytes (2-2-18-18)
            big endian convention

        1- CTable available (stored into workspace ?)
        2- Small input (fast heuristic ? Full comparison ? depend on clevel ?)


   1.2) Literal block content

   1.2.1) Huff0 block, using sizes from header
        See Huff0 format

   1.2.2) Huff0 block, using prepared table

   1.2.3) Raw content

   1.2.4) single byte


   2) Sequences section
      TO DO
*/

/** ZSTD1v06_frameHeaderSize() :
*   srcSize must be >= ZSTD1v06_frameHeaderSize_min.
*   @return : size of the Frame Header */
static size_t ZSTD1v06_frameHeaderSize(const void* src, size_t srcSize)
{
    if (srcSize < ZSTD1v06_frameHeaderSize_min) return ERROR(srcSize_wrong);
    { U32 const fcsId = (((const BYTE*)src)[4]) >> 6;
      return ZSTD1v06_frameHeaderSize_min + ZSTD1v06_fcs_fieldSize[fcsId]; }
}


/** ZSTD1v06_getFrameParams() :
*   decode Frame Header, or provide expected `srcSize`.
*   @return : 0, `fparamsPtr` is correctly filled,
*            >0, `srcSize` is too small, result is expected `srcSize`,
*             or an error code, which can be tested using ZSTD1v06_isError() */
size_t ZSTD1v06_getFrameParams(ZSTD1v06_frameParams* fparamsPtr, const void* src, size_t srcSize)
{
    const BYTE* ip = (const BYTE*)src;

    if (srcSize < ZSTD1v06_frameHeaderSize_min) return ZSTD1v06_frameHeaderSize_min;
    if (MEM_readLE32(src) != ZSTD1v06_MAGICNUMBER) return ERROR(prefix_unknown);

    /* ensure there is enough `srcSize` to fully read/decode frame header */
    { size_t const fhsize = ZSTD1v06_frameHeaderSize(src, srcSize);
      if (srcSize < fhsize) return fhsize; }

    memset(fparamsPtr, 0, sizeof(*fparamsPtr));
    {   BYTE const frameDesc = ip[4];
        fparamsPtr->windowLog = (frameDesc & 0xF) + ZSTD1v06_WINDOWLOG_ABSOLUTEMIN;
        if ((frameDesc & 0x20) != 0) return ERROR(frameParameter_unsupported);   /* reserved 1 bit */
        switch(frameDesc >> 6)  /* fcsId */
        {
            default:   /* impossible */
            case 0 : fparamsPtr->frameContentSize = 0; break;
            case 1 : fparamsPtr->frameContentSize = ip[5]; break;
            case 2 : fparamsPtr->frameContentSize = MEM_readLE16(ip+5)+256; break;
            case 3 : fparamsPtr->frameContentSize = MEM_readLE64(ip+5); break;
    }   }
    return 0;
}


/** ZSTD1v06_decodeFrameHeader() :
*   `srcSize` must be the size provided by ZSTD1v06_frameHeaderSize().
*   @return : 0 if success, or an error code, which can be tested using ZSTD1v06_isError() */
static size_t ZSTD1v06_decodeFrameHeader(ZSTD1v06_DCtx* zc, const void* src, size_t srcSize)
{
    size_t const result = ZSTD1v06_getFrameParams(&(zc->fParams), src, srcSize);
    if ((MEM_32bits()) && (zc->fParams.windowLog > 25)) return ERROR(frameParameter_unsupportedBy32bits);
    return result;
}


typedef struct
{
    blockType_t blockType;
    U32 origSize;
} blockProperties_t;

/*! ZSTD1v06_getcBlockSize() :
*   Provides the size of compressed block from block header `src` */
size_t ZSTD1v06_getcBlockSize(const void* src, size_t srcSize, blockProperties_t* bpPtr)
{
    const BYTE* const in = (const BYTE* const)src;
    U32 cSize;

    if (srcSize < ZSTD1v06_blockHeaderSize) return ERROR(srcSize_wrong);

    bpPtr->blockType = (blockType_t)((*in) >> 6);
    cSize = in[2] + (in[1]<<8) + ((in[0] & 7)<<16);
    bpPtr->origSize = (bpPtr->blockType == bt_rle) ? cSize : 0;

    if (bpPtr->blockType == bt_end) return 0;
    if (bpPtr->blockType == bt_rle) return 1;
    return cSize;
}


static size_t ZSTD1v06_copyRawBlock(void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
    if (srcSize > dstCapacity) return ERROR(dstSize_tooSmall);
    memcpy(dst, src, srcSize);
    return srcSize;
}


/*! ZSTD1v06_decodeLiteralsBlock() :
    @return : nb of bytes read from src (< srcSize ) */
size_t ZSTD1v06_decodeLiteralsBlock(ZSTD1v06_DCtx* dctx,
                          const void* src, size_t srcSize)   /* note : srcSize < BLOCKSIZE */
{
    const BYTE* const istart = (const BYTE*) src;

    /* any compressed block with literals segment must be at least this size */
    if (srcSize < MIN_CBLOCK_SIZE) return ERROR(corruption_detected);

    switch(istart[0]>> 6)
    {
    case IS_HUF1:
        {   size_t litSize, litCSize, singleStream=0;
            U32 lhSize = ((istart[0]) >> 4) & 3;
            if (srcSize < 5) return ERROR(corruption_detected);   /* srcSize >= MIN_CBLOCK_SIZE == 3; here we need up to 5 for lhSize, + cSize (+nbSeq) */
            switch(lhSize)
            {
            case 0: case 1: default:   /* note : default is impossible, since lhSize into [0..3] */
                /* 2 - 2 - 10 - 10 */
                lhSize=3;
                singleStream = istart[0] & 16;
                litSize  = ((istart[0] & 15) << 6) + (istart[1] >> 2);
                litCSize = ((istart[1] &  3) << 8) + istart[2];
                break;
            case 2:
                /* 2 - 2 - 14 - 14 */
                lhSize=4;
                litSize  = ((istart[0] & 15) << 10) + (istart[1] << 2) + (istart[2] >> 6);
                litCSize = ((istart[2] & 63) <<  8) + istart[3];
                break;
            case 3:
                /* 2 - 2 - 18 - 18 */
                lhSize=5;
                litSize  = ((istart[0] & 15) << 14) + (istart[1] << 6) + (istart[2] >> 2);
                litCSize = ((istart[2] &  3) << 16) + (istart[3] << 8) + istart[4];
                break;
            }
            if (litSize > ZSTD1v06_BLOCKSIZE_MAX) return ERROR(corruption_detected);
            if (litCSize + lhSize > srcSize) return ERROR(corruption_detected);

            if (HUF1v06_isError(singleStream ?
                            HUF1v06_decompress1X2(dctx->litBuffer, litSize, istart+lhSize, litCSize) :
                            HUF1v06_decompress   (dctx->litBuffer, litSize, istart+lhSize, litCSize) ))
                return ERROR(corruption_detected);

            dctx->litPtr = dctx->litBuffer;
            dctx->litSize = litSize;
            memset(dctx->litBuffer + dctx->litSize, 0, WILDCOPY_OVERLENGTH);
            return litCSize + lhSize;
        }
    case IS_PCH:
        {   size_t litSize, litCSize;
            U32 lhSize = ((istart[0]) >> 4) & 3;
            if (lhSize != 1)  /* only case supported for now : small litSize, single stream */
                return ERROR(corruption_detected);
            if (!dctx->flagRepeatTable)
                return ERROR(dictionary_corrupted);

            /* 2 - 2 - 10 - 10 */
            lhSize=3;
            litSize  = ((istart[0] & 15) << 6) + (istart[1] >> 2);
            litCSize = ((istart[1] &  3) << 8) + istart[2];
            if (litCSize + lhSize > srcSize) return ERROR(corruption_detected);

            {   size_t const errorCode = HUF1v06_decompress1X4_usingDTable(dctx->litBuffer, litSize, istart+lhSize, litCSize, dctx->hufTableX4);
                if (HUF1v06_isError(errorCode)) return ERROR(corruption_detected);
            }
            dctx->litPtr = dctx->litBuffer;
            dctx->litSize = litSize;
            memset(dctx->litBuffer + dctx->litSize, 0, WILDCOPY_OVERLENGTH);
            return litCSize + lhSize;
        }
    case IS_RAW:
        {   size_t litSize;
            U32 lhSize = ((istart[0]) >> 4) & 3;
            switch(lhSize)
            {
            case 0: case 1: default:   /* note : default is impossible, since lhSize into [0..3] */
                lhSize=1;
                litSize = istart[0] & 31;
                break;
            case 2:
                litSize = ((istart[0] & 15) << 8) + istart[1];
                break;
            case 3:
                litSize = ((istart[0] & 15) << 16) + (istart[1] << 8) + istart[2];
                break;
            }

            if (lhSize+litSize+WILDCOPY_OVERLENGTH > srcSize) {  /* risk reading beyond src buffer with wildcopy */
                if (litSize+lhSize > srcSize) return ERROR(corruption_detected);
                memcpy(dctx->litBuffer, istart+lhSize, litSize);
                dctx->litPtr = dctx->litBuffer;
                dctx->litSize = litSize;
                memset(dctx->litBuffer + dctx->litSize, 0, WILDCOPY_OVERLENGTH);
                return lhSize+litSize;
            }
            /* direct reference into compressed stream */
            dctx->litPtr = istart+lhSize;
            dctx->litSize = litSize;
            return lhSize+litSize;
        }
    case IS_RLE:
        {   size_t litSize;
            U32 lhSize = ((istart[0]) >> 4) & 3;
            switch(lhSize)
            {
            case 0: case 1: default:   /* note : default is impossible, since lhSize into [0..3] */
                lhSize = 1;
                litSize = istart[0] & 31;
                break;
            case 2:
                litSize = ((istart[0] & 15) << 8) + istart[1];
                break;
            case 3:
                litSize = ((istart[0] & 15) << 16) + (istart[1] << 8) + istart[2];
                if (srcSize<4) return ERROR(corruption_detected);   /* srcSize >= MIN_CBLOCK_SIZE == 3; here we need lhSize+1 = 4 */
                break;
            }
            if (litSize > ZSTD1v06_BLOCKSIZE_MAX) return ERROR(corruption_detected);
            memset(dctx->litBuffer, istart[lhSize], litSize + WILDCOPY_OVERLENGTH);
            dctx->litPtr = dctx->litBuffer;
            dctx->litSize = litSize;
            return lhSize+1;
        }
    default:
        return ERROR(corruption_detected);   /* impossible */
    }
}


/*! ZSTD1v06_buildSeqTable() :
    @return : nb bytes read from src,
              or an error code if it fails, testable with ZSTD1v06_isError()
*/
size_t ZSTD1v06_buildSeqTable(FSE1v06_DTable* DTable, U32 type, U32 max, U32 maxLog,
                                 const void* src, size_t srcSize,
                                 const S16* defaultNorm, U32 defaultLog, U32 flagRepeatTable)
{
    switch(type)
    {
    case FSE1v06_ENCODING_RLE :
        if (!srcSize) return ERROR(srcSize_wrong);
        if ( (*(const BYTE*)src) > max) return ERROR(corruption_detected);
        FSE1v06_buildDTable_rle(DTable, *(const BYTE*)src);   /* if *src > max, data is corrupted */
        return 1;
    case FSE1v06_ENCODING_RAW :
        FSE1v06_buildDTable(DTable, defaultNorm, max, defaultLog);
        return 0;
    case FSE1v06_ENCODING_STATIC:
        if (!flagRepeatTable) return ERROR(corruption_detected);
        return 0;
    default :   /* impossible */
    case FSE1v06_ENCODING_DYNAMIC :
        {   U32 tableLog;
            S16 norm[MaxSeq+1];
            size_t const headerSize = FSE1v06_readNCount(norm, &max, &tableLog, src, srcSize);
            if (FSE1v06_isError(headerSize)) return ERROR(corruption_detected);
            if (tableLog > maxLog) return ERROR(corruption_detected);
            FSE1v06_buildDTable(DTable, norm, max, tableLog);
            return headerSize;
    }   }
}


size_t ZSTD1v06_decodeSeqHeaders(int* nbSeqPtr,
                             FSE1v06_DTable* DTableLL, FSE1v06_DTable* DTableML, FSE1v06_DTable* DTableOffb, U32 flagRepeatTable,
                             const void* src, size_t srcSize)
{
    const BYTE* const istart = (const BYTE* const)src;
    const BYTE* const iend = istart + srcSize;
    const BYTE* ip = istart;

    /* check */
    if (srcSize < MIN_SEQUENCES_SIZE) return ERROR(srcSize_wrong);

    /* SeqHead */
    {   int nbSeq = *ip++;
        if (!nbSeq) { *nbSeqPtr=0; return 1; }
        if (nbSeq > 0x7F) {
            if (nbSeq == 0xFF) {
                if (ip+2 > iend) return ERROR(srcSize_wrong);
                nbSeq = MEM_readLE16(ip) + LONGNBSEQ, ip+=2;
            } else {
                if (ip >= iend) return ERROR(srcSize_wrong);
                nbSeq = ((nbSeq-0x80)<<8) + *ip++;
            }
        }
        *nbSeqPtr = nbSeq;
    }

    /* FSE1 table descriptors */
    {   U32 const LLtype  = *ip >> 6;
        U32 const Offtype = (*ip >> 4) & 3;
        U32 const MLtype  = (*ip >> 2) & 3;
        ip++;

        /* check */
        if (ip > iend-3) return ERROR(srcSize_wrong); /* min : all 3 are "raw", hence no header, but at least xxLog bits per type */

        /* Build DTables */
        {   size_t const bhSize = ZSTD1v06_buildSeqTable(DTableLL, LLtype, MaxLL, LLFSE1Log, ip, iend-ip, LL_defaultNorm, LL_defaultNormLog, flagRepeatTable);
            if (ZSTD1v06_isError(bhSize)) return ERROR(corruption_detected);
            ip += bhSize;
        }
        {   size_t const bhSize = ZSTD1v06_buildSeqTable(DTableOffb, Offtype, MaxOff, OffFSE1Log, ip, iend-ip, OF_defaultNorm, OF_defaultNormLog, flagRepeatTable);
            if (ZSTD1v06_isError(bhSize)) return ERROR(corruption_detected);
            ip += bhSize;
        }
        {   size_t const bhSize = ZSTD1v06_buildSeqTable(DTableML, MLtype, MaxML, MLFSE1Log, ip, iend-ip, ML_defaultNorm, ML_defaultNormLog, flagRepeatTable);
            if (ZSTD1v06_isError(bhSize)) return ERROR(corruption_detected);
            ip += bhSize;
    }   }

    return ip-istart;
}


typedef struct {
    size_t litLength;
    size_t matchLength;
    size_t offset;
} seq_t;

typedef struct {
    BITv06_DStream_t DStream;
    FSE1v06_DState_t stateLL;
    FSE1v06_DState_t stateOffb;
    FSE1v06_DState_t stateML;
    size_t prevOffset[ZSTD1v06_REP_INIT];
} seqState_t;



static void ZSTD1v06_decodeSequence(seq_t* seq, seqState_t* seqState)
{
    /* Literal length */
    U32 const llCode = FSE1v06_peekSymbol(&(seqState->stateLL));
    U32 const mlCode = FSE1v06_peekSymbol(&(seqState->stateML));
    U32 const ofCode = FSE1v06_peekSymbol(&(seqState->stateOffb));   /* <= maxOff, by table construction */

    U32 const llBits = LL_bits[llCode];
    U32 const mlBits = ML_bits[mlCode];
    U32 const ofBits = ofCode;
    U32 const totalBits = llBits+mlBits+ofBits;

    static const U32 LL_base[MaxLL+1] = {
                             0,  1,  2,  3,  4,  5,  6,  7,  8,  9,   10,    11,    12,    13,    14,     15,
                            16, 18, 20, 22, 24, 28, 32, 40, 48, 64, 0x80, 0x100, 0x200, 0x400, 0x800, 0x1000,
                            0x2000, 0x4000, 0x8000, 0x10000 };

    static const U32 ML_base[MaxML+1] = {
                             0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10,   11,    12,    13,    14,    15,
                            16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26,   27,    28,    29,    30,    31,
                            32, 34, 36, 38, 40, 44, 48, 56, 64, 80, 96, 0x80, 0x100, 0x200, 0x400, 0x800,
                            0x1000, 0x2000, 0x4000, 0x8000, 0x10000 };

    static const U32 OF_base[MaxOff+1] = {
                 0,        1,       3,       7,     0xF,     0x1F,     0x3F,     0x7F,
                 0xFF,   0x1FF,   0x3FF,   0x7FF,   0xFFF,   0x1FFF,   0x3FFF,   0x7FFF,
                 0xFFFF, 0x1FFFF, 0x3FFFF, 0x7FFFF, 0xFFFFF, 0x1FFFFF, 0x3FFFFF, 0x7FFFFF,
                 0xFFFFFF, 0x1FFFFFF, 0x3FFFFFF, /*fake*/ 1, 1 };

    /* sequence */
    {   size_t offset;
        if (!ofCode)
            offset = 0;
        else {
            offset = OF_base[ofCode] + BITv06_readBits(&(seqState->DStream), ofBits);   /* <=  26 bits */
            if (MEM_32bits()) BITv06_reloadDStream(&(seqState->DStream));
        }

        if (offset < ZSTD1v06_REP_NUM) {
            if (llCode == 0 && offset <= 1) offset = 1-offset;

            if (offset != 0) {
                size_t temp = seqState->prevOffset[offset];
                if (offset != 1) {
                    seqState->prevOffset[2] = seqState->prevOffset[1];
                }
                seqState->prevOffset[1] = seqState->prevOffset[0];
                seqState->prevOffset[0] = offset = temp;

            } else {
                offset = seqState->prevOffset[0];
            }
        } else {
            offset -= ZSTD1v06_REP_MOVE;
            seqState->prevOffset[2] = seqState->prevOffset[1];
            seqState->prevOffset[1] = seqState->prevOffset[0];
            seqState->prevOffset[0] = offset;
        }
        seq->offset = offset;
    }

    seq->matchLength = ML_base[mlCode] + MINMATCH + ((mlCode>31) ? BITv06_readBits(&(seqState->DStream), mlBits) : 0);   /* <=  16 bits */
    if (MEM_32bits() && (mlBits+llBits>24)) BITv06_reloadDStream(&(seqState->DStream));

    seq->litLength = LL_base[llCode] + ((llCode>15) ? BITv06_readBits(&(seqState->DStream), llBits) : 0);   /* <=  16 bits */
    if (MEM_32bits() ||
       (totalBits > 64 - 7 - (LLFSE1Log+MLFSE1Log+OffFSE1Log)) ) BITv06_reloadDStream(&(seqState->DStream));

    /* ANS state update */
    FSE1v06_updateState(&(seqState->stateLL), &(seqState->DStream));   /* <=  9 bits */
    FSE1v06_updateState(&(seqState->stateML), &(seqState->DStream));   /* <=  9 bits */
    if (MEM_32bits()) BITv06_reloadDStream(&(seqState->DStream));     /* <= 18 bits */
    FSE1v06_updateState(&(seqState->stateOffb), &(seqState->DStream)); /* <=  8 bits */
}


size_t ZSTD1v06_execSequence(BYTE* op,
                                BYTE* const oend, seq_t sequence,
                                const BYTE** litPtr, const BYTE* const litLimit,
                                const BYTE* const base, const BYTE* const vBase, const BYTE* const dictEnd)
{
    BYTE* const oLitEnd = op + sequence.litLength;
    size_t const sequenceLength = sequence.litLength + sequence.matchLength;
    BYTE* const oMatchEnd = op + sequenceLength;   /* risk : address space overflow (32-bits) */
    BYTE* const oend_8 = oend-8;
    const BYTE* const iLitEnd = *litPtr + sequence.litLength;
    const BYTE* match = oLitEnd - sequence.offset;

    /* check */
    if (oLitEnd > oend_8) return ERROR(dstSize_tooSmall);   /* last match must start at a minimum distance of 8 from oend */
    if (oMatchEnd > oend) return ERROR(dstSize_tooSmall);   /* overwrite beyond dst buffer */
    if (iLitEnd > litLimit) return ERROR(corruption_detected);   /* over-read beyond lit buffer */

    /* copy Literals */
    ZSTD1v06_wildcopy(op, *litPtr, sequence.litLength);   /* note : oLitEnd <= oend-8 : no risk of overwrite beyond oend */
    op = oLitEnd;
    *litPtr = iLitEnd;   /* update for next sequence */

    /* copy Match */
    if (sequence.offset > (size_t)(oLitEnd - base)) {
        /* offset beyond prefix */
        if (sequence.offset > (size_t)(oLitEnd - vBase)) return ERROR(corruption_detected);
        match = dictEnd - (base-match);
        if (match + sequence.matchLength <= dictEnd) {
            memmove(oLitEnd, match, sequence.matchLength);
            return sequenceLength;
        }
        /* span extDict & currentPrefixSegment */
        {   size_t const length1 = dictEnd - match;
            memmove(oLitEnd, match, length1);
            op = oLitEnd + length1;
            sequence.matchLength -= length1;
            match = base;
            if (op > oend_8 || sequence.matchLength < MINMATCH) {
              while (op < oMatchEnd) *op++ = *match++;
              return sequenceLength;
            }
    }   }
    /* Requirement: op <= oend_8 */

    /* match within prefix */
    if (sequence.offset < 8) {
        /* close range match, overlap */
        static const U32 dec32table[] = { 0, 1, 2, 1, 4, 4, 4, 4 };   /* added */
        static const int dec64table[] = { 8, 8, 8, 7, 8, 9,10,11 };   /* substracted */
        int const sub2 = dec64table[sequence.offset];
        op[0] = match[0];
        op[1] = match[1];
        op[2] = match[2];
        op[3] = match[3];
        match += dec32table[sequence.offset];
        ZSTD1v06_copy4(op+4, match);
        match -= sub2;
    } else {
        ZSTD1v06_copy8(op, match);
    }
    op += 8; match += 8;

    if (oMatchEnd > oend-(16-MINMATCH)) {
        if (op < oend_8) {
            ZSTD1v06_wildcopy(op, match, oend_8 - op);
            match += oend_8 - op;
            op = oend_8;
        }
        while (op < oMatchEnd) *op++ = *match++;
    } else {
        ZSTD1v06_wildcopy(op, match, (ptrdiff_t)sequence.matchLength-8);   /* works even if matchLength < 8 */
    }
    return sequenceLength;
}


static size_t ZSTD1v06_decompressSequences(
                               ZSTD1v06_DCtx* dctx,
                               void* dst, size_t maxDstSize,
                         const void* seqStart, size_t seqSize)
{
    const BYTE* ip = (const BYTE*)seqStart;
    const BYTE* const iend = ip + seqSize;
    BYTE* const ostart = (BYTE* const)dst;
    BYTE* const oend = ostart + maxDstSize;
    BYTE* op = ostart;
    const BYTE* litPtr = dctx->litPtr;
    const BYTE* const litEnd = litPtr + dctx->litSize;
    FSE1v06_DTable* DTableLL = dctx->LLTable;
    FSE1v06_DTable* DTableML = dctx->MLTable;
    FSE1v06_DTable* DTableOffb = dctx->OffTable;
    const BYTE* const base = (const BYTE*) (dctx->base);
    const BYTE* const vBase = (const BYTE*) (dctx->vBase);
    const BYTE* const dictEnd = (const BYTE*) (dctx->dictEnd);
    int nbSeq;

    /* Build Decoding Tables */
    {   size_t const seqHSize = ZSTD1v06_decodeSeqHeaders(&nbSeq, DTableLL, DTableML, DTableOffb, dctx->flagRepeatTable, ip, seqSize);
        if (ZSTD1v06_isError(seqHSize)) return seqHSize;
        ip += seqHSize;
        dctx->flagRepeatTable = 0;
    }

    /* Regen sequences */
    if (nbSeq) {
        seq_t sequence;
        seqState_t seqState;

        memset(&sequence, 0, sizeof(sequence));
        sequence.offset = REPCODE_STARTVALUE;
        { U32 i; for (i=0; i<ZSTD1v06_REP_INIT; i++) seqState.prevOffset[i] = REPCODE_STARTVALUE; }
        { size_t const errorCode = BITv06_initDStream(&(seqState.DStream), ip, iend-ip);
          if (ERR_isError(errorCode)) return ERROR(corruption_detected); }
        FSE1v06_initDState(&(seqState.stateLL), &(seqState.DStream), DTableLL);
        FSE1v06_initDState(&(seqState.stateOffb), &(seqState.DStream), DTableOffb);
        FSE1v06_initDState(&(seqState.stateML), &(seqState.DStream), DTableML);

        for ( ; (BITv06_reloadDStream(&(seqState.DStream)) <= BITv06_DStream_completed) && nbSeq ; ) {
            nbSeq--;
            ZSTD1v06_decodeSequence(&sequence, &seqState);

#if 0  /* debug */
            static BYTE* start = NULL;
            if (start==NULL) start = op;
            size_t pos = (size_t)(op-start);
            if ((pos >= 5810037) && (pos < 5810400))
                printf("Dpos %6u :%5u literals & match %3u bytes at distance %6u \n",
                       pos, (U32)sequence.litLength, (U32)sequence.matchLength, (U32)sequence.offset);
#endif

            {   size_t const oneSeqSize = ZSTD1v06_execSequence(op, oend, sequence, &litPtr, litEnd, base, vBase, dictEnd);
                if (ZSTD1v06_isError(oneSeqSize)) return oneSeqSize;
                op += oneSeqSize;
        }   }

        /* check if reached exact end */
        if (nbSeq) return ERROR(corruption_detected);
    }

    /* last literal segment */
    {   size_t const lastLLSize = litEnd - litPtr;
        if (litPtr > litEnd) return ERROR(corruption_detected);   /* too many literals already used */
        if (op+lastLLSize > oend) return ERROR(dstSize_tooSmall);
        memcpy(op, litPtr, lastLLSize);
        op += lastLLSize;
    }

    return op-ostart;
}


static void ZSTD1v06_checkContinuity(ZSTD1v06_DCtx* dctx, const void* dst)
{
    if (dst != dctx->previousDstEnd) {   /* not contiguous */
        dctx->dictEnd = dctx->previousDstEnd;
        dctx->vBase = (const char*)dst - ((const char*)(dctx->previousDstEnd) - (const char*)(dctx->base));
        dctx->base = dst;
        dctx->previousDstEnd = dst;
    }
}


static size_t ZSTD1v06_decompressBlock_internal(ZSTD1v06_DCtx* dctx,
                            void* dst, size_t dstCapacity,
                      const void* src, size_t srcSize)
{   /* blockType == blockCompressed */
    const BYTE* ip = (const BYTE*)src;

    if (srcSize >= ZSTD1v06_BLOCKSIZE_MAX) return ERROR(srcSize_wrong);

    /* Decode literals sub-block */
    {   size_t const litCSize = ZSTD1v06_decodeLiteralsBlock(dctx, src, srcSize);
        if (ZSTD1v06_isError(litCSize)) return litCSize;
        ip += litCSize;
        srcSize -= litCSize;
    }
    return ZSTD1v06_decompressSequences(dctx, dst, dstCapacity, ip, srcSize);
}


size_t ZSTD1v06_decompressBlock(ZSTD1v06_DCtx* dctx,
                            void* dst, size_t dstCapacity,
                      const void* src, size_t srcSize)
{
    ZSTD1v06_checkContinuity(dctx, dst);
    return ZSTD1v06_decompressBlock_internal(dctx, dst, dstCapacity, src, srcSize);
}


/*! ZSTD1v06_decompressFrame() :
*   `dctx` must be properly initialized */
static size_t ZSTD1v06_decompressFrame(ZSTD1v06_DCtx* dctx,
                                 void* dst, size_t dstCapacity,
                                 const void* src, size_t srcSize)
{
    const BYTE* ip = (const BYTE*)src;
    const BYTE* const iend = ip + srcSize;
    BYTE* const ostart = (BYTE* const)dst;
    BYTE* op = ostart;
    BYTE* const oend = ostart + dstCapacity;
    size_t remainingSize = srcSize;
    blockProperties_t blockProperties = { bt_compressed, 0 };

    /* check */
    if (srcSize < ZSTD1v06_frameHeaderSize_min+ZSTD1v06_blockHeaderSize) return ERROR(srcSize_wrong);

    /* Frame Header */
    {   size_t const frameHeaderSize = ZSTD1v06_frameHeaderSize(src, ZSTD1v06_frameHeaderSize_min);
        if (ZSTD1v06_isError(frameHeaderSize)) return frameHeaderSize;
        if (srcSize < frameHeaderSize+ZSTD1v06_blockHeaderSize) return ERROR(srcSize_wrong);
        if (ZSTD1v06_decodeFrameHeader(dctx, src, frameHeaderSize)) return ERROR(corruption_detected);
        ip += frameHeaderSize; remainingSize -= frameHeaderSize;
    }

    /* Loop on each block */
    while (1) {
        size_t decodedSize=0;
        size_t const cBlockSize = ZSTD1v06_getcBlockSize(ip, iend-ip, &blockProperties);
        if (ZSTD1v06_isError(cBlockSize)) return cBlockSize;

        ip += ZSTD1v06_blockHeaderSize;
        remainingSize -= ZSTD1v06_blockHeaderSize;
        if (cBlockSize > remainingSize) return ERROR(srcSize_wrong);

        switch(blockProperties.blockType)
        {
        case bt_compressed:
            decodedSize = ZSTD1v06_decompressBlock_internal(dctx, op, oend-op, ip, cBlockSize);
            break;
        case bt_raw :
            decodedSize = ZSTD1v06_copyRawBlock(op, oend-op, ip, cBlockSize);
            break;
        case bt_rle :
            return ERROR(GENERIC);   /* not yet supported */
            break;
        case bt_end :
            /* end of frame */
            if (remainingSize) return ERROR(srcSize_wrong);
            break;
        default:
            return ERROR(GENERIC);   /* impossible */
        }
        if (cBlockSize == 0) break;   /* bt_end */

        if (ZSTD1v06_isError(decodedSize)) return decodedSize;
        op += decodedSize;
        ip += cBlockSize;
        remainingSize -= cBlockSize;
    }

    return op-ostart;
}


size_t ZSTD1v06_decompress_usingPreparedDCtx(ZSTD1v06_DCtx* dctx, const ZSTD1v06_DCtx* refDCtx,
                                         void* dst, size_t dstCapacity,
                                   const void* src, size_t srcSize)
{
    ZSTD1v06_copyDCtx(dctx, refDCtx);
    ZSTD1v06_checkContinuity(dctx, dst);
    return ZSTD1v06_decompressFrame(dctx, dst, dstCapacity, src, srcSize);
}


size_t ZSTD1v06_decompress_usingDict(ZSTD1v06_DCtx* dctx,
                                 void* dst, size_t dstCapacity,
                                 const void* src, size_t srcSize,
                                 const void* dict, size_t dictSize)
{
    ZSTD1v06_decompressBegin_usingDict(dctx, dict, dictSize);
    ZSTD1v06_checkContinuity(dctx, dst);
    return ZSTD1v06_decompressFrame(dctx, dst, dstCapacity, src, srcSize);
}


size_t ZSTD1v06_decompressDCtx(ZSTD1v06_DCtx* dctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
    return ZSTD1v06_decompress_usingDict(dctx, dst, dstCapacity, src, srcSize, NULL, 0);
}


size_t ZSTD1v06_decompress(void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
#if defined(ZSTD1v06_HEAPMODE) && (ZSTD1v06_HEAPMODE==1)
    size_t regenSize;
    ZSTD1v06_DCtx* dctx = ZSTD1v06_createDCtx();
    if (dctx==NULL) return ERROR(memory_allocation);
    regenSize = ZSTD1v06_decompressDCtx(dctx, dst, dstCapacity, src, srcSize);
    ZSTD1v06_freeDCtx(dctx);
    return regenSize;
#else   /* stack mode */
    ZSTD1v06_DCtx dctx;
    return ZSTD1v06_decompressDCtx(&dctx, dst, dstCapacity, src, srcSize);
#endif
}

size_t ZSTD1v06_findFrameCompressedSize(const void* src, size_t srcSize)
{
    const BYTE* ip = (const BYTE*)src;
    size_t remainingSize = srcSize;
    blockProperties_t blockProperties = { bt_compressed, 0 };

    /* Frame Header */
    {   size_t const frameHeaderSize = ZSTD1v06_frameHeaderSize(src, ZSTD1v06_frameHeaderSize_min);
        if (ZSTD1v06_isError(frameHeaderSize)) return frameHeaderSize;
        if (MEM_readLE32(src) != ZSTD1v06_MAGICNUMBER) return ERROR(prefix_unknown);
        if (srcSize < frameHeaderSize+ZSTD1v06_blockHeaderSize) return ERROR(srcSize_wrong);
        ip += frameHeaderSize; remainingSize -= frameHeaderSize;
    }

    /* Loop on each block */
    while (1) {
        size_t const cBlockSize = ZSTD1v06_getcBlockSize(ip, remainingSize, &blockProperties);
        if (ZSTD1v06_isError(cBlockSize)) return cBlockSize;

        ip += ZSTD1v06_blockHeaderSize;
        remainingSize -= ZSTD1v06_blockHeaderSize;
        if (cBlockSize > remainingSize) return ERROR(srcSize_wrong);

        if (cBlockSize == 0) break;   /* bt_end */

        ip += cBlockSize;
        remainingSize -= cBlockSize;
    }

    return ip - (const BYTE*)src;
}

/*_******************************
*  Streaming Decompression API
********************************/
size_t ZSTD1v06_nextSrcSizeToDecompress(ZSTD1v06_DCtx* dctx)
{
    return dctx->expected;
}

size_t ZSTD1v06_decompressContinue(ZSTD1v06_DCtx* dctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
    /* Sanity check */
    if (srcSize != dctx->expected) return ERROR(srcSize_wrong);
    if (dstCapacity) ZSTD1v06_checkContinuity(dctx, dst);

    /* Decompress : frame header; part 1 */
    switch (dctx->stage)
    {
    case ZSTD1ds_getFrameHeaderSize :
        if (srcSize != ZSTD1v06_frameHeaderSize_min) return ERROR(srcSize_wrong);   /* impossible */
        dctx->headerSize = ZSTD1v06_frameHeaderSize(src, ZSTD1v06_frameHeaderSize_min);
        if (ZSTD1v06_isError(dctx->headerSize)) return dctx->headerSize;
        memcpy(dctx->headerBuffer, src, ZSTD1v06_frameHeaderSize_min);
        if (dctx->headerSize > ZSTD1v06_frameHeaderSize_min) {
            dctx->expected = dctx->headerSize - ZSTD1v06_frameHeaderSize_min;
            dctx->stage = ZSTD1ds_decodeFrameHeader;
            return 0;
        }
        dctx->expected = 0;   /* not necessary to copy more */
	/* fall-through */
    case ZSTD1ds_decodeFrameHeader:
        {   size_t result;
            memcpy(dctx->headerBuffer + ZSTD1v06_frameHeaderSize_min, src, dctx->expected);
            result = ZSTD1v06_decodeFrameHeader(dctx, dctx->headerBuffer, dctx->headerSize);
            if (ZSTD1v06_isError(result)) return result;
            dctx->expected = ZSTD1v06_blockHeaderSize;
            dctx->stage = ZSTD1ds_decodeBlockHeader;
            return 0;
        }
    case ZSTD1ds_decodeBlockHeader:
        {   blockProperties_t bp;
            size_t const cBlockSize = ZSTD1v06_getcBlockSize(src, ZSTD1v06_blockHeaderSize, &bp);
            if (ZSTD1v06_isError(cBlockSize)) return cBlockSize;
            if (bp.blockType == bt_end) {
                dctx->expected = 0;
                dctx->stage = ZSTD1ds_getFrameHeaderSize;
            } else {
                dctx->expected = cBlockSize;
                dctx->bType = bp.blockType;
                dctx->stage = ZSTD1ds_decompressBlock;
            }
            return 0;
        }
    case ZSTD1ds_decompressBlock:
        {   size_t rSize;
            switch(dctx->bType)
            {
            case bt_compressed:
                rSize = ZSTD1v06_decompressBlock_internal(dctx, dst, dstCapacity, src, srcSize);
                break;
            case bt_raw :
                rSize = ZSTD1v06_copyRawBlock(dst, dstCapacity, src, srcSize);
                break;
            case bt_rle :
                return ERROR(GENERIC);   /* not yet handled */
                break;
            case bt_end :   /* should never happen (filtered at phase 1) */
                rSize = 0;
                break;
            default:
                return ERROR(GENERIC);   /* impossible */
            }
            dctx->stage = ZSTD1ds_decodeBlockHeader;
            dctx->expected = ZSTD1v06_blockHeaderSize;
            dctx->previousDstEnd = (char*)dst + rSize;
            return rSize;
        }
    default:
        return ERROR(GENERIC);   /* impossible */
    }
}


static void ZSTD1v06_refDictContent(ZSTD1v06_DCtx* dctx, const void* dict, size_t dictSize)
{
    dctx->dictEnd = dctx->previousDstEnd;
    dctx->vBase = (const char*)dict - ((const char*)(dctx->previousDstEnd) - (const char*)(dctx->base));
    dctx->base = dict;
    dctx->previousDstEnd = (const char*)dict + dictSize;
}

static size_t ZSTD1v06_loadEntropy(ZSTD1v06_DCtx* dctx, const void* dict, size_t dictSize)
{
    size_t hSize, offcodeHeaderSize, matchlengthHeaderSize, litlengthHeaderSize;

    hSize = HUF1v06_readDTableX4(dctx->hufTableX4, dict, dictSize);
    if (HUF1v06_isError(hSize)) return ERROR(dictionary_corrupted);
    dict = (const char*)dict + hSize;
    dictSize -= hSize;

    {   short offcodeNCount[MaxOff+1];
        U32 offcodeMaxValue=MaxOff, offcodeLog;
        offcodeHeaderSize = FSE1v06_readNCount(offcodeNCount, &offcodeMaxValue, &offcodeLog, dict, dictSize);
        if (FSE1v06_isError(offcodeHeaderSize)) return ERROR(dictionary_corrupted);
        if (offcodeLog > OffFSE1Log) return ERROR(dictionary_corrupted);
        { size_t const errorCode = FSE1v06_buildDTable(dctx->OffTable, offcodeNCount, offcodeMaxValue, offcodeLog);
          if (FSE1v06_isError(errorCode)) return ERROR(dictionary_corrupted); }
        dict = (const char*)dict + offcodeHeaderSize;
        dictSize -= offcodeHeaderSize;
    }

    {   short matchlengthNCount[MaxML+1];
        unsigned matchlengthMaxValue = MaxML, matchlengthLog;
        matchlengthHeaderSize = FSE1v06_readNCount(matchlengthNCount, &matchlengthMaxValue, &matchlengthLog, dict, dictSize);
        if (FSE1v06_isError(matchlengthHeaderSize)) return ERROR(dictionary_corrupted);
        if (matchlengthLog > MLFSE1Log) return ERROR(dictionary_corrupted);
        { size_t const errorCode = FSE1v06_buildDTable(dctx->MLTable, matchlengthNCount, matchlengthMaxValue, matchlengthLog);
          if (FSE1v06_isError(errorCode)) return ERROR(dictionary_corrupted); }
        dict = (const char*)dict + matchlengthHeaderSize;
        dictSize -= matchlengthHeaderSize;
    }

    {   short litlengthNCount[MaxLL+1];
        unsigned litlengthMaxValue = MaxLL, litlengthLog;
        litlengthHeaderSize = FSE1v06_readNCount(litlengthNCount, &litlengthMaxValue, &litlengthLog, dict, dictSize);
        if (FSE1v06_isError(litlengthHeaderSize)) return ERROR(dictionary_corrupted);
        if (litlengthLog > LLFSE1Log) return ERROR(dictionary_corrupted);
        { size_t const errorCode = FSE1v06_buildDTable(dctx->LLTable, litlengthNCount, litlengthMaxValue, litlengthLog);
          if (FSE1v06_isError(errorCode)) return ERROR(dictionary_corrupted); }
    }

    dctx->flagRepeatTable = 1;
    return hSize + offcodeHeaderSize + matchlengthHeaderSize + litlengthHeaderSize;
}

static size_t ZSTD1v06_decompress_insertDictionary(ZSTD1v06_DCtx* dctx, const void* dict, size_t dictSize)
{
    size_t eSize;
    U32 const magic = MEM_readLE32(dict);
    if (magic != ZSTD1v06_DICT_MAGIC) {
        /* pure content mode */
        ZSTD1v06_refDictContent(dctx, dict, dictSize);
        return 0;
    }
    /* load entropy tables */
    dict = (const char*)dict + 4;
    dictSize -= 4;
    eSize = ZSTD1v06_loadEntropy(dctx, dict, dictSize);
    if (ZSTD1v06_isError(eSize)) return ERROR(dictionary_corrupted);

    /* reference dictionary content */
    dict = (const char*)dict + eSize;
    dictSize -= eSize;
    ZSTD1v06_refDictContent(dctx, dict, dictSize);

    return 0;
}


size_t ZSTD1v06_decompressBegin_usingDict(ZSTD1v06_DCtx* dctx, const void* dict, size_t dictSize)
{
    { size_t const errorCode = ZSTD1v06_decompressBegin(dctx);
      if (ZSTD1v06_isError(errorCode)) return errorCode; }

    if (dict && dictSize) {
        size_t const errorCode = ZSTD1v06_decompress_insertDictionary(dctx, dict, dictSize);
        if (ZSTD1v06_isError(errorCode)) return ERROR(dictionary_corrupted);
    }

    return 0;
}

/*
    Buffered version of Zstd compression library
    Copyright (C) 2015-2016, Yann Collet.

    BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions are
    met:
    * Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above
    copyright notice, this list of conditions and the following disclaimer
    in the documentation and/or other materials provided with the
    distribution.
    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    You can contact the author at :
    - zstd homepage : http://www.zstd.net/
*/


/*-***************************************************************************
*  Streaming decompression howto
*
*  A ZBUFF1v06_DCtx object is required to track streaming operations.
*  Use ZBUFF1v06_createDCtx() and ZBUFF1v06_freeDCtx() to create/release resources.
*  Use ZBUFF1v06_decompressInit() to start a new decompression operation,
*   or ZBUFF1v06_decompressInitDictionary() if decompression requires a dictionary.
*  Note that ZBUFF1v06_DCtx objects can be re-init multiple times.
*
*  Use ZBUFF1v06_decompressContinue() repetitively to consume your input.
*  *srcSizePtr and *dstCapacityPtr can be any size.
*  The function will report how many bytes were read or written by modifying *srcSizePtr and *dstCapacityPtr.
*  Note that it may not consume the entire input, in which case it's up to the caller to present remaining input again.
*  The content of @dst will be overwritten (up to *dstCapacityPtr) at each function call, so save its content if it matters, or change @dst.
*  @return : a hint to preferred nb of bytes to use as input for next function call (it's only a hint, to help latency),
*            or 0 when a frame is completely decoded,
*            or an error code, which can be tested using ZBUFF1v06_isError().
*
*  Hint : recommended buffer sizes (not compulsory) : ZBUFF1v06_recommendedDInSize() and ZBUFF1v06_recommendedDOutSize()
*  output : ZBUFF1v06_recommendedDOutSize==128 KB block size is the internal unit, it ensures it's always possible to write a full block when decoded.
*  input  : ZBUFF1v06_recommendedDInSize == 128KB + 3;
*           just follow indications from ZBUFF1v06_decompressContinue() to minimize latency. It should always be <= 128 KB + 3 .
* *******************************************************************************/

typedef enum { ZBUFF1ds_init, ZBUFF1ds_loadHeader,
               ZBUFF1ds_read, ZBUFF1ds_load, ZBUFF1ds_flush } ZBUFF1v06_dStage;

/* *** Resource management *** */
struct ZBUFF1v06_DCtx_s {
    ZSTD1v06_DCtx* zd;
    ZSTD1v06_frameParams fParams;
    ZBUFF1v06_dStage stage;
    char*  inBuff;
    size_t inBuffSize;
    size_t inPos;
    char*  outBuff;
    size_t outBuffSize;
    size_t outStart;
    size_t outEnd;
    size_t blockSize;
    BYTE headerBuffer[ZSTD1v06_FRAMEHEADERSIZE_MAX];
    size_t lhSize;
};   /* typedef'd to ZBUFF1v06_DCtx within "zstd_buffered.h" */


ZBUFF1v06_DCtx* ZBUFF1v06_createDCtx(void)
{
    ZBUFF1v06_DCtx* zbd = (ZBUFF1v06_DCtx*)malloc(sizeof(ZBUFF1v06_DCtx));
    if (zbd==NULL) return NULL;
    memset(zbd, 0, sizeof(*zbd));
    zbd->zd = ZSTD1v06_createDCtx();
    zbd->stage = ZBUFF1ds_init;
    return zbd;
}

size_t ZBUFF1v06_freeDCtx(ZBUFF1v06_DCtx* zbd)
{
    if (zbd==NULL) return 0;   /* support free on null */
    ZSTD1v06_freeDCtx(zbd->zd);
    free(zbd->inBuff);
    free(zbd->outBuff);
    free(zbd);
    return 0;
}


/* *** Initialization *** */

size_t ZBUFF1v06_decompressInitDictionary(ZBUFF1v06_DCtx* zbd, const void* dict, size_t dictSize)
{
    zbd->stage = ZBUFF1ds_loadHeader;
    zbd->lhSize = zbd->inPos = zbd->outStart = zbd->outEnd = 0;
    return ZSTD1v06_decompressBegin_usingDict(zbd->zd, dict, dictSize);
}

size_t ZBUFF1v06_decompressInit(ZBUFF1v06_DCtx* zbd)
{
    return ZBUFF1v06_decompressInitDictionary(zbd, NULL, 0);
}



MEM_STATIC size_t ZBUFF1v06_limitCopy(void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
    size_t length = MIN(dstCapacity, srcSize);
    memcpy(dst, src, length);
    return length;
}


/* *** Decompression *** */

size_t ZBUFF1v06_decompressContinue(ZBUFF1v06_DCtx* zbd,
                                void* dst, size_t* dstCapacityPtr,
                          const void* src, size_t* srcSizePtr)
{
    const char* const istart = (const char*)src;
    const char* const iend = istart + *srcSizePtr;
    const char* ip = istart;
    char* const ostart = (char*)dst;
    char* const oend = ostart + *dstCapacityPtr;
    char* op = ostart;
    U32 notDone = 1;

    while (notDone) {
        switch(zbd->stage)
        {
        case ZBUFF1ds_init :
            return ERROR(init_missing);

        case ZBUFF1ds_loadHeader :
            {   size_t const hSize = ZSTD1v06_getFrameParams(&(zbd->fParams), zbd->headerBuffer, zbd->lhSize);
                if (hSize != 0) {
                    size_t const toLoad = hSize - zbd->lhSize;   /* if hSize!=0, hSize > zbd->lhSize */
                    if (ZSTD1v06_isError(hSize)) return hSize;
                    if (toLoad > (size_t)(iend-ip)) {   /* not enough input to load full header */
                        memcpy(zbd->headerBuffer + zbd->lhSize, ip, iend-ip);
                        zbd->lhSize += iend-ip; ip = iend; notDone = 0;
                        *dstCapacityPtr = 0;
                        return (hSize - zbd->lhSize) + ZSTD1v06_blockHeaderSize;   /* remaining header bytes + next block header */
                    }
                    memcpy(zbd->headerBuffer + zbd->lhSize, ip, toLoad); zbd->lhSize = hSize; ip += toLoad;
                    break;
            }   }

            /* Consume header */
            {   size_t const h1Size = ZSTD1v06_nextSrcSizeToDecompress(zbd->zd);  /* == ZSTD1v06_frameHeaderSize_min */
                size_t const h1Result = ZSTD1v06_decompressContinue(zbd->zd, NULL, 0, zbd->headerBuffer, h1Size);
                if (ZSTD1v06_isError(h1Result)) return h1Result;
                if (h1Size < zbd->lhSize) {   /* long header */
                    size_t const h2Size = ZSTD1v06_nextSrcSizeToDecompress(zbd->zd);
                    size_t const h2Result = ZSTD1v06_decompressContinue(zbd->zd, NULL, 0, zbd->headerBuffer+h1Size, h2Size);
                    if (ZSTD1v06_isError(h2Result)) return h2Result;
            }   }

            /* Frame header instruct buffer sizes */
            {   size_t const blockSize = MIN(1 << zbd->fParams.windowLog, ZSTD1v06_BLOCKSIZE_MAX);
                zbd->blockSize = blockSize;
                if (zbd->inBuffSize < blockSize) {
                    free(zbd->inBuff);
                    zbd->inBuffSize = blockSize;
                    zbd->inBuff = (char*)malloc(blockSize);
                    if (zbd->inBuff == NULL) return ERROR(memory_allocation);
                }
                {   size_t const neededOutSize = ((size_t)1 << zbd->fParams.windowLog) + blockSize + WILDCOPY_OVERLENGTH * 2;
                    if (zbd->outBuffSize < neededOutSize) {
                        free(zbd->outBuff);
                        zbd->outBuffSize = neededOutSize;
                        zbd->outBuff = (char*)malloc(neededOutSize);
                        if (zbd->outBuff == NULL) return ERROR(memory_allocation);
            }   }   }
            zbd->stage = ZBUFF1ds_read;
	    /* fall-through */
        case ZBUFF1ds_read:
            {   size_t const neededInSize = ZSTD1v06_nextSrcSizeToDecompress(zbd->zd);
                if (neededInSize==0) {  /* end of frame */
                    zbd->stage = ZBUFF1ds_init;
                    notDone = 0;
                    break;
                }
                if ((size_t)(iend-ip) >= neededInSize) {  /* decode directly from src */
                    size_t const decodedSize = ZSTD1v06_decompressContinue(zbd->zd,
                        zbd->outBuff + zbd->outStart, zbd->outBuffSize - zbd->outStart,
                        ip, neededInSize);
                    if (ZSTD1v06_isError(decodedSize)) return decodedSize;
                    ip += neededInSize;
                    if (!decodedSize) break;   /* this was just a header */
                    zbd->outEnd = zbd->outStart +  decodedSize;
                    zbd->stage = ZBUFF1ds_flush;
                    break;
                }
                if (ip==iend) { notDone = 0; break; }   /* no more input */
                zbd->stage = ZBUFF1ds_load;
            }
	    /* fall-through */
        case ZBUFF1ds_load:
            {   size_t const neededInSize = ZSTD1v06_nextSrcSizeToDecompress(zbd->zd);
                size_t const toLoad = neededInSize - zbd->inPos;   /* should always be <= remaining space within inBuff */
                size_t loadedSize;
                if (toLoad > zbd->inBuffSize - zbd->inPos) return ERROR(corruption_detected);   /* should never happen */
                loadedSize = ZBUFF1v06_limitCopy(zbd->inBuff + zbd->inPos, toLoad, ip, iend-ip);
                ip += loadedSize;
                zbd->inPos += loadedSize;
                if (loadedSize < toLoad) { notDone = 0; break; }   /* not enough input, wait for more */

                /* decode loaded input */
                {   size_t const decodedSize = ZSTD1v06_decompressContinue(zbd->zd,
                        zbd->outBuff + zbd->outStart, zbd->outBuffSize - zbd->outStart,
                        zbd->inBuff, neededInSize);
                    if (ZSTD1v06_isError(decodedSize)) return decodedSize;
                    zbd->inPos = 0;   /* input is consumed */
                    if (!decodedSize) { zbd->stage = ZBUFF1ds_read; break; }   /* this was just a header */
                    zbd->outEnd = zbd->outStart +  decodedSize;
                    zbd->stage = ZBUFF1ds_flush;
                    // break; /* ZBUFF1ds_flush follows */
                }
	    }
	    /* fall-through */
        case ZBUFF1ds_flush:
            {   size_t const toFlushSize = zbd->outEnd - zbd->outStart;
                size_t const flushedSize = ZBUFF1v06_limitCopy(op, oend-op, zbd->outBuff + zbd->outStart, toFlushSize);
                op += flushedSize;
                zbd->outStart += flushedSize;
                if (flushedSize == toFlushSize) {
                    zbd->stage = ZBUFF1ds_read;
                    if (zbd->outStart + zbd->blockSize > zbd->outBuffSize)
                        zbd->outStart = zbd->outEnd = 0;
                    break;
                }
                /* cannot flush everything */
                notDone = 0;
                break;
            }
        default: return ERROR(GENERIC);   /* impossible */
    }   }

    /* result */
    *srcSizePtr = ip-istart;
    *dstCapacityPtr = op-ostart;
    {   size_t nextSrcSizeHint = ZSTD1v06_nextSrcSizeToDecompress(zbd->zd);
        if (nextSrcSizeHint > ZSTD1v06_blockHeaderSize) nextSrcSizeHint+= ZSTD1v06_blockHeaderSize;   /* get following block header too */
        nextSrcSizeHint -= zbd->inPos;   /* already loaded*/
        return nextSrcSizeHint;
    }
}



/* *************************************
*  Tool functions
***************************************/
size_t ZBUFF1v06_recommendedDInSize(void)  { return ZSTD1v06_BLOCKSIZE_MAX + ZSTD1v06_blockHeaderSize /* block header size*/ ; }
size_t ZBUFF1v06_recommendedDOutSize(void) { return ZSTD1v06_BLOCKSIZE_MAX; }
