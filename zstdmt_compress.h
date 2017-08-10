/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

 #ifndef ZSTD1MT_COMPRESS_H
 #define ZSTD1MT_COMPRESS_H

 #if defined (__cplusplus)
 extern "C" {
 #endif


/* Note : All prototypes defined in this file are labelled experimental.
 *        No guarantee of API continuity is provided on any of them.
 *        In fact, the expectation is that these prototypes will be replaced
 *        by ZSTD1_compress_generic() API in the near future */

/* ===   Dependencies   === */
#include <stddef.h>                /* size_t */
#define ZSTD1_STATIC_LINKING_ONLY   /* ZSTD1_parameters */
#include "zstd.h"            /* ZSTD1_inBuffer, ZSTD1_outBuffer, ZSTD1LIB_API */


/* ===   Memory management   === */
typedef struct ZSTD1MT_CCtx_s ZSTD1MT_CCtx;
ZSTD1LIB_API ZSTD1MT_CCtx* ZSTD1MT_createCCtx(unsigned nbThreads);
ZSTD1LIB_API ZSTD1MT_CCtx* ZSTD1MT_createCCtx_advanced(unsigned nbThreads,
                                                    ZSTD1_customMem cMem);
ZSTD1LIB_API size_t ZSTD1MT_freeCCtx(ZSTD1MT_CCtx* mtctx);

ZSTD1LIB_API size_t ZSTD1MT_sizeof_CCtx(ZSTD1MT_CCtx* mtctx);


/* ===   Simple buffer-to-butter one-pass function   === */

ZSTD1LIB_API size_t ZSTD1MT_compressCCtx(ZSTD1MT_CCtx* mtctx,
                                       void* dst, size_t dstCapacity,
                                 const void* src, size_t srcSize,
                                       int compressionLevel);



/* ===   Streaming functions   === */

ZSTD1LIB_API size_t ZSTD1MT_initCStream(ZSTD1MT_CCtx* mtctx, int compressionLevel);
ZSTD1LIB_API size_t ZSTD1MT_resetCStream(ZSTD1MT_CCtx* mtctx, unsigned long long pledgedSrcSize);    /**< pledgedSrcSize is optional and can be zero == unknown */

ZSTD1LIB_API size_t ZSTD1MT_compressStream(ZSTD1MT_CCtx* mtctx, ZSTD1_outBuffer* output, ZSTD1_inBuffer* input);

ZSTD1LIB_API size_t ZSTD1MT_flushStream(ZSTD1MT_CCtx* mtctx, ZSTD1_outBuffer* output);   /**< @return : 0 == all flushed; >0 : still some data to be flushed; or an error code (ZSTD1_isError()) */
ZSTD1LIB_API size_t ZSTD1MT_endStream(ZSTD1MT_CCtx* mtctx, ZSTD1_outBuffer* output);     /**< @return : 0 == all flushed; >0 : still some data to be flushed; or an error code (ZSTD1_isError()) */


/* ===   Advanced functions and parameters  === */

#ifndef ZSTD1MT_SECTION_SIZE_MIN
#  define ZSTD1MT_SECTION_SIZE_MIN (1U << 20)   /* 1 MB - Minimum size of each compression job */
#endif

ZSTD1LIB_API size_t ZSTD1MT_compress_advanced(ZSTD1MT_CCtx* mtctx,
                                           void* dst, size_t dstCapacity,
                                     const void* src, size_t srcSize,
                                     const ZSTD1_CDict* cdict,
                                           ZSTD1_parameters const params,
                                           unsigned overlapRLog);

ZSTD1LIB_API size_t ZSTD1MT_initCStream_advanced(ZSTD1MT_CCtx* mtctx,
                                        const void* dict, size_t dictSize,   /* dict can be released after init, a local copy is preserved within zcs */
                                        ZSTD1_parameters params,
                                        unsigned long long pledgedSrcSize);  /* pledgedSrcSize is optional and can be zero == unknown */

ZSTD1LIB_API size_t ZSTD1MT_initCStream_usingCDict(ZSTD1MT_CCtx* mtctx,
                                        const ZSTD1_CDict* cdict,
                                        ZSTD1_frameParameters fparams,
                                        unsigned long long pledgedSrcSize);  /* note : zero means empty */

/* ZSDTMT_parameter :
 * List of parameters that can be set using ZSTD1MT_setMTCtxParameter() */
typedef enum {
    ZSTD1MT_p_sectionSize,        /* size of input "section". Each section is compressed in parallel. 0 means default, which is dynamically determined within compression functions */
    ZSTD1MT_p_overlapSectionLog   /* Log of overlapped section; 0 == no overlap, 6(default) == use 1/8th of window, >=9 == use full window */
} ZSDTMT_parameter;

/* ZSTD1MT_setMTCtxParameter() :
 * allow setting indiv1idual parameters, one at a time, among a list of enums defined in ZSTD1MT_parameter.
 * The function must be called typically after ZSTD1_createCCtx().
 * Parameters not explicitly reset by ZSTD1MT_init*() remain the same in consecutive compression sessions.
 * @return : 0, or an error code (which can be tested using ZSTD1_isError()) */
ZSTD1LIB_API size_t ZSTD1MT_setMTCtxParameter(ZSTD1MT_CCtx* mtctx, ZSDTMT_parameter parameter, unsigned value);


/*! ZSTD1MT_compressStream_generic() :
 *  Combines ZSTD1MT_compressStream() with ZSTD1MT_flushStream() or ZSTD1MT_endStream()
 *  depending on flush directive.
 * @return : minimum amount of data still to be flushed
 *           0 if fully flushed
 *           or an error code */
ZSTD1LIB_API size_t ZSTD1MT_compressStream_generic(ZSTD1MT_CCtx* mtctx,
                                                ZSTD1_outBuffer* output,
                                                ZSTD1_inBuffer* input,
                                                ZSTD1_EndDirective endOp);



#if defined (__cplusplus)
}
#endif

#endif   /* ZSTD1MT_COMPRESS_H */
