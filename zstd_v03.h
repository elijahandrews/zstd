/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#ifndef ZSTD1_V03_H_298734209782
#define ZSTD1_V03_H_298734209782

#if defined (__cplusplus)
extern "C" {
#endif

/* *************************************
*  Includes
***************************************/
#include <stddef.h>   /* size_t */


/* *************************************
*  Simple one-step function
***************************************/
/**
ZSTD1v03_decompress() : decompress ZSTD1 frames compliant with v0.3.x format
    compressedSize : is the exact source size
    maxOriginalSize : is the size of the 'dst' buffer, which must be already allocated.
                      It must be equal or larger than originalSize, otherwise decompression will fail.
    return : the number of bytes decompressed into destination buffer (originalSize)
             or an errorCode if it fails (which can be tested using ZSTD1v01_isError())
*/
size_t ZSTD1v03_decompress( void* dst, size_t maxOriginalSize,
                     const void* src, size_t compressedSize);

/**
ZSTD1v03_getFrameSrcSize() : get the source length of a ZSTD1 frame compliant with v0.3.x format
    compressedSize : The size of the 'src' buffer, at least as large as the frame pointed to by 'src'
    return : the number of bytes that would be read to decompress this frame
             or an errorCode if it fails (which can be tested using ZSTD1v03_isError())
*/
size_t ZSTD1v03_findFrameCompressedSize(const void* src, size_t compressedSize);

    /**
ZSTD1v03_isError() : tells if the result of ZSTD1v03_decompress() is an error
*/
unsigned ZSTD1v03_isError(size_t code);


/* *************************************
*  Advanced functions
***************************************/
typedef struct ZSTD1v03_Dctx_s ZSTD1v03_Dctx;
ZSTD1v03_Dctx* ZSTD1v03_createDCtx(void);
size_t ZSTD1v03_freeDCtx(ZSTD1v03_Dctx* dctx);

size_t ZSTD1v03_decompressDCtx(void* ctx,
                              void* dst, size_t maxOriginalSize,
                        const void* src, size_t compressedSize);

/* *************************************
*  Streaming functions
***************************************/
size_t ZSTD1v03_resetDCtx(ZSTD1v03_Dctx* dctx);

size_t ZSTD1v03_nextSrcSizeToDecompress(ZSTD1v03_Dctx* dctx);
size_t ZSTD1v03_decompressContinue(ZSTD1v03_Dctx* dctx, void* dst, size_t maxDstSize, const void* src, size_t srcSize);
/**
  Use above functions alternatively.
  ZSTD1_nextSrcSizeToDecompress() tells how much bytes to provide as 'srcSize' to ZSTD1_decompressContinue().
  ZSTD1_decompressContinue() will use previous data blocks to improve compression if they are located prior to current block.
  Result is the number of bytes regenerated within 'dst'.
  It can be zero, which is not an error; it just means ZSTD1_decompressContinue() has decoded some header.
*/

/* *************************************
*  Prefix - version detection
***************************************/
#define ZSTD1v03_magicNumber 0xFD2FB523   /* v0.3 */


#if defined (__cplusplus)
}
#endif

#endif /* ZSTD1_V03_H_298734209782 */
