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
#ifndef HUF1_H_298734234
#define HUF1_H_298734234

#if defined (__cplusplus)
extern "C" {
#endif


/* *** Dependencies *** */
#include <stddef.h>    /* size_t */


/* *** library symbols visibility *** */
/* Note : when linking with -fvisibility=hidden on gcc, or by default on Visual,
 *        HUF1 symbols remain "private" (internal symbols for library only).
 *        Set macro FSE1_DLL_EXPORT to 1 if you want HUF1 symbols visible on DLL interface */
#if defined(FSE1_DLL_EXPORT) && (FSE1_DLL_EXPORT==1) && defined(__GNUC__) && (__GNUC__ >= 4)
#  define HUF1_PUBLIC_API __attribute__ ((visibility ("default")))
#elif defined(FSE1_DLL_EXPORT) && (FSE1_DLL_EXPORT==1)   /* Visual expected */
#  define HUF1_PUBLIC_API __declspec(dllexport)
#elif defined(FSE1_DLL_IMPORT) && (FSE1_DLL_IMPORT==1)
#  define HUF1_PUBLIC_API __declspec(dllimport)  /* not required, just to generate faster code (saves a function pointer load from IAT and an indirect jump) */
#else
#  define HUF1_PUBLIC_API
#endif


/* *** simple functions *** */
/**
HUF1_compress() :
    Compress content from buffer 'src', of size 'srcSize', into buffer 'dst'.
    'dst' buffer must be already allocated.
    Compression runs faster if `dstCapacity` >= HUF1_compressBound(srcSize).
    `srcSize` must be <= `HUF1_BLOCKSIZE_MAX` == 128 KB.
    @return : size of compressed data (<= `dstCapacity`).
    Special values : if return == 0, srcData is not compressible => Nothing is stored within dst !!!
                     if return == 1, srcData is a single repeated byte symbol (RLE compression).
                     if HUF1_isError(return), compression failed (more details using HUF1_getErrorName())
*/
HUF1_PUBLIC_API size_t HUF1_compress(void* dst, size_t dstCapacity,
                             const void* src, size_t srcSize);

/**
HUF1_decompress() :
    Decompress HUF1 data from buffer 'cSrc', of size 'cSrcSize',
    into already allocated buffer 'dst', of minimum size 'dstSize'.
    `originalSize` : **must** be the ***exact*** size of original (uncompressed) data.
    Note : in contrast with FSE1, HUF1_decompress can regenerate
           RLE (cSrcSize==1) and uncompressed (cSrcSize==dstSize) data,
           because it knows size to regenerate.
    @return : size of regenerated data (== originalSize),
              or an error code, which can be tested using HUF1_isError()
*/
HUF1_PUBLIC_API size_t HUF1_decompress(void* dst,  size_t originalSize,
                               const void* cSrc, size_t cSrcSize);


/* ***   Tool functions *** */
#define HUF1_BLOCKSIZE_MAX (128 * 1024)                  /**< maximum input size for a single block compressed with HUF1_compress */
HUF1_PUBLIC_API size_t HUF1_compressBound(size_t size);   /**< maximum compressed size (worst case) */

/* Error Management */
HUF1_PUBLIC_API unsigned    HUF1_isError(size_t code);       /**< tells if a return value is an error code */
HUF1_PUBLIC_API const char* HUF1_getErrorName(size_t code);  /**< provides error code string (useful for debugging) */


/* ***   Advanced function   *** */

/** HUF1_compress2() :
 *  Same as HUF1_compress(), but offers direct control over `maxSymbolValue` and `tableLog`.
 *  `tableLog` must be `<= HUF1_TABLELOG_MAX` . */
HUF1_PUBLIC_API size_t HUF1_compress2 (void* dst, size_t dstCapacity, const void* src, size_t srcSize, unsigned maxSymbolValue, unsigned tableLog);

/** HUF1_compress4X_wksp() :
 *  Same as HUF1_compress2(), but uses externally allocated `workSpace`.
 *  `workspace` must have minimum alignment of 4, and be at least as large as following macro */
#define HUF1_WORKSPACE_SIZE (6 << 10)
#define HUF1_WORKSPACE_SIZE_U32 (HUF1_WORKSPACE_SIZE / sizeof(U32))
HUF1_PUBLIC_API size_t HUF1_compress4X_wksp (void* dst, size_t dstCapacity, const void* src, size_t srcSize, unsigned maxSymbolValue, unsigned tableLog, void* workSpace, size_t wkspSize);

/**
 *  The minimum workspace size for the `workSpace` used in
 *  HUF1_readDTableX2_wksp() and HUF1_readDTableX4_wksp().
 *
 *  The space used depends on HUF1_TABLELOG_MAX, ranging from ~1500 bytes when
 *  HUF1_TABLE_LOG_MAX=12 to ~1850 bytes when HUF1_TABLE_LOG_MAX=15.
 *  Buffer overflow errors may potentially occur if code modifications result in
 *  a required workspace size greater than that specified in the following
 *  macro.
 */
#define HUF1_DECOMPRESS_WORKSPACE_SIZE (2 << 10)
#define HUF1_DECOMPRESS_WORKSPACE_SIZE_U32 (HUF1_DECOMPRESS_WORKSPACE_SIZE / sizeof(U32))


/* ******************************************************************
 *  WARNING !!
 *  The following section contains advanced and experimental definitions
 *  which shall never be used in the context of dll
 *  because they are not guaranteed to remain stable in the future.
 *  Only consider them in association with static linking.
 *******************************************************************/
#ifdef HUF1_STATIC_LINKING_ONLY

/* *** Dependencies *** */
#include "mem.h"   /* U32 */


/* *** Constants *** */
#define HUF1_TABLELOG_MAX      12       /* max configured tableLog (for static allocation); can be modified up to HUF1_ABSOLUTEMAX_TABLELOG */
#define HUF1_TABLELOG_DEFAULT  11       /* tableLog by default, when not specified */
#define HUF1_SYMBOLVALUE_MAX  255

#define HUF1_TABLELOG_ABSOLUTEMAX  15   /* absolute limit of HUF1_MAX_TABLELOG. Beyond that value, code does not work */
#if (HUF1_TABLELOG_MAX > HUF1_TABLELOG_ABSOLUTEMAX)
#  error "HUF1_TABLELOG_MAX is too large !"
#endif


/* ****************************************
*  Static allocation
******************************************/
/* HUF1 buffer bounds */
#define HUF1_CTABLEBOUND 129
#define HUF1_BLOCKBOUND(size) (size + (size>>8) + 8)   /* only true when incompressible is pre-filtered with fast heuristic */
#define HUF1_COMPRESSBOUND(size) (HUF1_CTABLEBOUND + HUF1_BLOCKBOUND(size))   /* Macro version, useful for static allocation */

/* static allocation of HUF1's Compression Table */
#define HUF1_CTABLE_SIZE_U32(maxSymbolValue)   ((maxSymbolValue)+1)   /* Use tables of U32, for proper alignment */
#define HUF1_CTABLE_SIZE(maxSymbolValue)       (HUF1_CTABLE_SIZE_U32(maxSymbolValue) * sizeof(U32))
#define HUF1_CREATE_STATIC_CTABLE(name, maxSymbolValue) \
    U32 name##hb[HUF1_CTABLE_SIZE_U32(maxSymbolValue)]; \
    void* name##hv = &(name##hb); \
    HUF1_CElt* name = (HUF1_CElt*)(name##hv)   /* no final ; */

/* static allocation of HUF1's DTable */
typedef U32 HUF1_DTable;
#define HUF1_DTABLE_SIZE(maxTableLog)   (1 + (1<<(maxTableLog)))
#define HUF1_CREATE_STATIC_DTABLEX2(DTable, maxTableLog) \
        HUF1_DTable DTable[HUF1_DTABLE_SIZE((maxTableLog)-1)] = { ((U32)((maxTableLog)-1) * 0x01000001) }
#define HUF1_CREATE_STATIC_DTABLEX4(DTable, maxTableLog) \
        HUF1_DTable DTable[HUF1_DTABLE_SIZE(maxTableLog)] = { ((U32)(maxTableLog) * 0x01000001) }


/* ****************************************
*  Advanced decompression functions
******************************************/
size_t HUF1_decompress4X2 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /**< single-symbol decoder */
size_t HUF1_decompress4X4 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /**< double-symbols decoder */

size_t HUF1_decompress4X_DCtx (HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /**< decodes RLE and uncompressed */
size_t HUF1_decompress4X_hufOnly(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize); /**< considers RLE and uncompressed as errors */
size_t HUF1_decompress4X_hufOnly_wksp(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize, void* workSpace, size_t wkspSize); /**< considers RLE and uncompressed as errors */
size_t HUF1_decompress4X2_DCtx(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /**< single-symbol decoder */
size_t HUF1_decompress4X2_DCtx_wksp(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize, void* workSpace, size_t wkspSize);   /**< single-symbol decoder */
size_t HUF1_decompress4X4_DCtx(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /**< double-symbols decoder */
size_t HUF1_decompress4X4_DCtx_wksp(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize, void* workSpace, size_t wkspSize);   /**< double-symbols decoder */


/* ****************************************
*  HUF1 detailed API
******************************************/
/*!
HUF1_compress() does the following:
1. count symbol occurrence from source[] into table count[] using FSE1_count()
2. (optional) refine tableLog using HUF1_optimalTableLog()
3. build Huffman table from count using HUF1_buildCTable()
4. save Huffman table to memory buffer using HUF1_writeCTable()
5. encode the data stream using HUF1_compress4X_usingCTable()

The following API allows targeting specific sub-functions for advanced tasks.
For example, it's possible to compress several blocks using the same 'CTable',
or to save and regenerate 'CTable' using external methods.
*/
/* FSE1_count() : find it within "fse.h" */
unsigned HUF1_optimalTableLog(unsigned maxTableLog, size_t srcSize, unsigned maxSymbolValue);
typedef struct HUF1_CElt_s HUF1_CElt;   /* incomplete type */
size_t HUF1_buildCTable (HUF1_CElt* CTable, const unsigned* count, unsigned maxSymbolValue, unsigned maxNbBits);
size_t HUF1_writeCTable (void* dst, size_t maxDstSize, const HUF1_CElt* CTable, unsigned maxSymbolValue, unsigned huffLog);
size_t HUF1_compress4X_usingCTable(void* dst, size_t dstSize, const void* src, size_t srcSize, const HUF1_CElt* CTable);

typedef enum {
   HUF1_repeat_none,  /**< Cannot use the previous table */
   HUF1_repeat_check, /**< Can use the previous table but it must be checked. Note : The previous table must have been constructed by HUF1_compress{1, 4}X_repeat */
   HUF1_repeat_valid  /**< Can use the previous table and it is asumed to be valid */
 } HUF1_repeat;
/** HUF1_compress4X_repeat() :
*   Same as HUF1_compress4X_wksp(), but considers using hufTable if *repeat != HUF1_repeat_none.
*   If it uses hufTable it does not modify hufTable or repeat.
*   If it doesn't, it sets *repeat = HUF1_repeat_none, and it sets hufTable to the table used.
*   If preferRepeat then the old table will always be used if valid. */
size_t HUF1_compress4X_repeat(void* dst, size_t dstSize, const void* src, size_t srcSize, unsigned maxSymbolValue, unsigned tableLog, void* workSpace, size_t wkspSize, HUF1_CElt* hufTable, HUF1_repeat* repeat, int preferRepeat);  /**< `workSpace` must be a table of at least HUF1_WORKSPACE_SIZE_U32 unsigned */

/** HUF1_buildCTable_wksp() :
 *  Same as HUF1_buildCTable(), but using externally allocated scratch buffer.
 *  `workSpace` must be aligned on 4-bytes boundaries, and be at least as large as a table of 1024 unsigned.
 */
size_t HUF1_buildCTable_wksp (HUF1_CElt* tree, const U32* count, U32 maxSymbolValue, U32 maxNbBits, void* workSpace, size_t wkspSize);

/*! HUF1_readStats() :
    Read compact Huffman tree, saved by HUF1_writeCTable().
    `huffWeight` is destination buffer.
    @return : size read from `src` , or an error Code .
    Note : Needed by HUF1_readCTable() and HUF1_readDTableXn() . */
size_t HUF1_readStats(BYTE* huffWeight, size_t hwSize, U32* rankStats,
                     U32* nbSymbolsPtr, U32* tableLogPtr,
                     const void* src, size_t srcSize);

/** HUF1_readCTable() :
*   Loading a CTable saved with HUF1_writeCTable() */
size_t HUF1_readCTable (HUF1_CElt* CTable, unsigned maxSymbolValue, const void* src, size_t srcSize);


/*
HUF1_decompress() does the following:
1. select the decompression algorithm (X2, X4) based on pre-computed heuristics
2. build Huffman table from save, using HUF1_readDTableXn()
3. decode 1 or 4 segments in parallel using HUF1_decompressSXn_usingDTable
*/

/** HUF1_selectDecoder() :
*   Tells which decoder is likely to decode faster,
*   based on a set of pre-determined metrics.
*   @return : 0==HUF1_decompress4X2, 1==HUF1_decompress4X4 .
*   Assumption : 0 < cSrcSize < dstSize <= 128 KB */
U32 HUF1_selectDecoder (size_t dstSize, size_t cSrcSize);

size_t HUF1_readDTableX2 (HUF1_DTable* DTable, const void* src, size_t srcSize);
size_t HUF1_readDTableX2_wksp (HUF1_DTable* DTable, const void* src, size_t srcSize, void* workSpace, size_t wkspSize);
size_t HUF1_readDTableX4 (HUF1_DTable* DTable, const void* src, size_t srcSize);
size_t HUF1_readDTableX4_wksp (HUF1_DTable* DTable, const void* src, size_t srcSize, void* workSpace, size_t wkspSize);

size_t HUF1_decompress4X_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const HUF1_DTable* DTable);
size_t HUF1_decompress4X2_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const HUF1_DTable* DTable);
size_t HUF1_decompress4X4_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const HUF1_DTable* DTable);


/* single stream variants */

size_t HUF1_compress1X (void* dst, size_t dstSize, const void* src, size_t srcSize, unsigned maxSymbolValue, unsigned tableLog);
size_t HUF1_compress1X_wksp (void* dst, size_t dstSize, const void* src, size_t srcSize, unsigned maxSymbolValue, unsigned tableLog, void* workSpace, size_t wkspSize);  /**< `workSpace` must be a table of at least HUF1_WORKSPACE_SIZE_U32 unsigned */
size_t HUF1_compress1X_usingCTable(void* dst, size_t dstSize, const void* src, size_t srcSize, const HUF1_CElt* CTable);
/** HUF1_compress1X_repeat() :
*   Same as HUF1_compress1X_wksp(), but considers using hufTable if *repeat != HUF1_repeat_none.
*   If it uses hufTable it does not modify hufTable or repeat.
*   If it doesn't, it sets *repeat = HUF1_repeat_none, and it sets hufTable to the table used.
*   If preferRepeat then the old table will always be used if valid. */
size_t HUF1_compress1X_repeat(void* dst, size_t dstSize, const void* src, size_t srcSize, unsigned maxSymbolValue, unsigned tableLog, void* workSpace, size_t wkspSize, HUF1_CElt* hufTable, HUF1_repeat* repeat, int preferRepeat);  /**< `workSpace` must be a table of at least HUF1_WORKSPACE_SIZE_U32 unsigned */

size_t HUF1_decompress1X2 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /* single-symbol decoder */
size_t HUF1_decompress1X4 (void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /* double-symbol decoder */

size_t HUF1_decompress1X_DCtx (HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);
size_t HUF1_decompress1X_DCtx_wksp (HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize, void* workSpace, size_t wkspSize);
size_t HUF1_decompress1X2_DCtx(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /**< single-symbol decoder */
size_t HUF1_decompress1X2_DCtx_wksp(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize, void* workSpace, size_t wkspSize);   /**< single-symbol decoder */
size_t HUF1_decompress1X4_DCtx(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize);   /**< double-symbols decoder */
size_t HUF1_decompress1X4_DCtx_wksp(HUF1_DTable* dctx, void* dst, size_t dstSize, const void* cSrc, size_t cSrcSize, void* workSpace, size_t wkspSize);   /**< double-symbols decoder */

size_t HUF1_decompress1X_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const HUF1_DTable* DTable);   /**< automatic selection of sing or double symbol decoder, based on DTable */
size_t HUF1_decompress1X2_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const HUF1_DTable* DTable);
size_t HUF1_decompress1X4_usingDTable(void* dst, size_t maxDstSize, const void* cSrc, size_t cSrcSize, const HUF1_DTable* DTable);

#endif /* HUF1_STATIC_LINKING_ONLY */


#if defined (__cplusplus)
}
#endif

#endif   /* HUF1_H_298734234 */
