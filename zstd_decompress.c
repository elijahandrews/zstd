/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */


/* ***************************************************************
*  Tuning parameters
*****************************************************************/
/*!
 * HEAPMODE :
 * Select how default decompression function ZSTD1_decompress() will allocate memory,
 * in memory stack (0), or in memory heap (1, requires malloc())
 */
#ifndef ZSTD1_HEAPMODE
#  define ZSTD1_HEAPMODE 1
#endif

/*!
*  LEGACY_SUPPORT :
*  if set to 1, ZSTD1_decompress() can decode older formats (v0.1+)
*/
#ifndef ZSTD1_LEGACY_SUPPORT
#  define ZSTD1_LEGACY_SUPPORT 0
#endif

/*!
*  MAXWINDOWSIZE_DEFAULT :
*  maximum window size accepted by DStream, by default.
*  Frames requiring more memory will be rejected.
*/
#ifndef ZSTD1_MAXWINDOWSIZE_DEFAULT
#  define ZSTD1_MAXWINDOWSIZE_DEFAULT ((1 << ZSTD1_WINDOWLOG_MAX) + 1)   /* defined within zstd.h */
#endif


/*-*******************************************************
*  Dependencies
*********************************************************/
#include <string.h>      /* memcpy, memmove, memset */
#include "mem.h"         /* low level memory routines */
#define FSE1_STATIC_LINKING_ONLY
#include "fse.h"
#define HUF1_STATIC_LINKING_ONLY
#include "huf.h"
#include "zstd_internal.h"

#if defined(ZSTD1_LEGACY_SUPPORT) && (ZSTD1_LEGACY_SUPPORT>=1)
#  include "zstd_legacy.h"
#endif

#if defined(_MSC_VER) && !defined(_M_IA64)  /* _mm_prefetch() is not defined for ia64 */
#  include <mmintrin.h>   /* https://msdn.microsoft.com/fr-fr/library/84szxsww(v=vs.90).aspx */
#  define ZSTD1_PREFETCH(ptr)   _mm_prefetch((const char*)ptr, _MM_HINT_T0)
#elif defined(__GNUC__)
#  define ZSTD1_PREFETCH(ptr)   __builtin_prefetch(ptr, 0, 0)
#else
#  define ZSTD1_PREFETCH(ptr)   /* disabled */
#endif


/*-*************************************
*  Errors
***************************************/
#define ZSTD1_isError ERR_isError   /* for inlining */
#define FSE1_isError  ERR_isError
#define HUF1_isError  ERR_isError


/*_*******************************************************
*  Memory operations
**********************************************************/
static void ZSTD1_copy4(void* dst, const void* src) { memcpy(dst, src, 4); }


/*-*************************************************************
*   Context management
***************************************************************/
typedef enum { ZSTD1ds_getFrameHeaderSize, ZSTD1ds_decodeFrameHeader,
               ZSTD1ds_decodeBlockHeader, ZSTD1ds_decompressBlock,
               ZSTD1ds_decompressLastBlock, ZSTD1ds_checkChecksum,
               ZSTD1ds_decodeSkippableHeader, ZSTD1ds_skipFrame } ZSTD1_dStage;

typedef enum { zdss_init=0, zdss_loadHeader,
               zdss_read, zdss_load, zdss_flush } ZSTD1_dStreamStage;

typedef struct {
    FSE1_DTable LLTable[FSE1_DTABLE_SIZE_U32(LLFSE1Log)];
    FSE1_DTable OFTable[FSE1_DTABLE_SIZE_U32(OffFSE1Log)];
    FSE1_DTable MLTable[FSE1_DTABLE_SIZE_U32(MLFSE1Log)];
    HUF1_DTable hufTable[HUF1_DTABLE_SIZE(HufLog)];  /* can accommodate HUF1_decompress4X */
    U32 workspace[HUF1_DECOMPRESS_WORKSPACE_SIZE_U32];
    U32 rep[ZSTD1_REP_NUM];
} ZSTD1_entropyTables_t;

struct ZSTD1_DCtx_s
{
    const FSE1_DTable* LLTptr;
    const FSE1_DTable* MLTptr;
    const FSE1_DTable* OFTptr;
    const HUF1_DTable* HUF1ptr;
    ZSTD1_entropyTables_t entropy;
    const void* previousDstEnd;   /* detect continuity */
    const void* base;             /* start of current segment */
    const void* vBase;            /* virtual start of previous segment if it was just before current one */
    const void* dictEnd;          /* end of previous segment */
    size_t expected;
    ZSTD1_frameHeader fParams;
    blockType_e bType;   /* used in ZSTD1_decompressContinue(), to transfer blockType between header decoding and block decoding stages */
    ZSTD1_dStage stage;
    U32 litEntropy;
    U32 fseEntropy;
    XXH64_state_t xxhState;
    size_t headerSize;
    U32 dictID;
    const BYTE* litPtr;
    ZSTD1_customMem customMem;
    size_t litSize;
    size_t rleSize;
    size_t staticSize;

    /* streaming */
    ZSTD1_DDict* ddictLocal;
    const ZSTD1_DDict* ddict;
    ZSTD1_dStreamStage streamStage;
    char*  inBuff;
    size_t inBuffSize;
    size_t inPos;
    size_t maxWindowSize;
    char*  outBuff;
    size_t outBuffSize;
    size_t outStart;
    size_t outEnd;
    size_t blockSize;
    size_t lhSize;
    void* legacyContext;
    U32 previousLegacyVersion;
    U32 legacyVersion;
    U32 hostageByte;

    /* workspace */
    BYTE litBuffer[ZSTD1_BLOCKSIZE_MAX + WILDCOPY_OVERLENGTH];
    BYTE headerBuffer[ZSTD1_FRAMEHEADERSIZE_MAX];
};  /* typedef'd to ZSTD1_DCtx within "zstd.h" */

size_t ZSTD1_sizeof_DCtx (const ZSTD1_DCtx* dctx)
{
    if (dctx==NULL) return 0;   /* support sizeof NULL */
    return sizeof(*dctx)
           + ZSTD1_sizeof_DDict(dctx->ddictLocal)
           + dctx->inBuffSize + dctx->outBuffSize;
}

size_t ZSTD1_estimateDCtxSize(void) { return sizeof(ZSTD1_DCtx); }

size_t ZSTD1_decompressBegin(ZSTD1_DCtx* dctx)
{
    dctx->expected = ZSTD1_frameHeaderSize_prefix;
    dctx->stage = ZSTD1ds_getFrameHeaderSize;
    dctx->previousDstEnd = NULL;
    dctx->base = NULL;
    dctx->vBase = NULL;
    dctx->dictEnd = NULL;
    dctx->entropy.hufTable[0] = (HUF1_DTable)((HufLog)*0x1000001);  /* cover both little and big endian */
    dctx->litEntropy = dctx->fseEntropy = 0;
    dctx->dictID = 0;
    MEM_STATIC_ASSERT(sizeof(dctx->entropy.rep) == sizeof(repStartValue));
    memcpy(dctx->entropy.rep, repStartValue, sizeof(repStartValue));  /* initial repcodes */
    dctx->LLTptr = dctx->entropy.LLTable;
    dctx->MLTptr = dctx->entropy.MLTable;
    dctx->OFTptr = dctx->entropy.OFTable;
    dctx->HUF1ptr = dctx->entropy.hufTable;
    return 0;
}

static void ZSTD1_initDCtx_internal(ZSTD1_DCtx* dctx)
{
    ZSTD1_decompressBegin(dctx);   /* cannot fail */
    dctx->staticSize = 0;
    dctx->maxWindowSize = ZSTD1_MAXWINDOWSIZE_DEFAULT;
    dctx->ddict   = NULL;
    dctx->ddictLocal = NULL;
    dctx->inBuff  = NULL;
    dctx->inBuffSize = 0;
    dctx->outBuffSize= 0;
    dctx->streamStage = zdss_init;
}

ZSTD1_DCtx* ZSTD1_createDCtx_advanced(ZSTD1_customMem customMem)
{
    if (!customMem.customAlloc ^ !customMem.customFree) return NULL;

    {   ZSTD1_DCtx* const dctx = (ZSTD1_DCtx*)ZSTD1_malloc(sizeof(*dctx), customMem);
        if (!dctx) return NULL;
        dctx->customMem = customMem;
        dctx->legacyContext = NULL;
        dctx->previousLegacyVersion = 0;
        ZSTD1_initDCtx_internal(dctx);
        return dctx;
    }
}

ZSTD1_DCtx* ZSTD1_initStaticDCtx(void *workspace, size_t workspaceSize)
{
    ZSTD1_DCtx* dctx = (ZSTD1_DCtx*) workspace;

    if ((size_t)workspace & 7) return NULL;  /* 8-aligned */
    if (workspaceSize < sizeof(ZSTD1_DCtx)) return NULL;  /* minimum size */

    ZSTD1_initDCtx_internal(dctx);
    dctx->staticSize = workspaceSize;
    dctx->inBuff = (char*)(dctx+1);
    return dctx;
}

ZSTD1_DCtx* ZSTD1_createDCtx(void)
{
    return ZSTD1_createDCtx_advanced(ZSTD1_defaultCMem);
}

size_t ZSTD1_freeDCtx(ZSTD1_DCtx* dctx)
{
    if (dctx==NULL) return 0;   /* support free on NULL */
    if (dctx->staticSize) return ERROR(memory_allocation);   /* not compatible with static DCtx */
    {   ZSTD1_customMem const cMem = dctx->customMem;
        ZSTD1_freeDDict(dctx->ddictLocal);
        dctx->ddictLocal = NULL;
        ZSTD1_free(dctx->inBuff, cMem);
        dctx->inBuff = NULL;
#if defined(ZSTD1_LEGACY_SUPPORT) && (ZSTD1_LEGACY_SUPPORT >= 1)
        if (dctx->legacyContext)
            ZSTD1_freeLegacyStreamContext(dctx->legacyContext, dctx->previousLegacyVersion);
#endif
        ZSTD1_free(dctx, cMem);
        return 0;
    }
}

/* no longer useful */
void ZSTD1_copyDCtx(ZSTD1_DCtx* dstDCtx, const ZSTD1_DCtx* srcDCtx)
{
    size_t const toCopy = (size_t)((char*)(&dstDCtx->inBuff) - (char*)dstDCtx);
    memcpy(dstDCtx, srcDCtx, toCopy);  /* no need to copy workspace */
}


/*-*************************************************************
*   Decompression section
***************************************************************/

/*! ZSTD1_isFrame() :
 *  Tells if the content of `buffer` starts with a valid Frame Identifier.
 *  Note : Frame Identifier is 4 bytes. If `size < 4`, @return will always be 0.
 *  Note 2 : Legacy Frame Identifiers are considered valid only if Legacy Support is enabled.
 *  Note 3 : Skippable Frame Identifiers are considered valid. */
unsigned ZSTD1_isFrame(const void* buffer, size_t size)
{
    if (size < 4) return 0;
    {   U32 const magic = MEM_readLE32(buffer);
        if (magic == ZSTD1_MAGICNUMBER) return 1;
        if ((magic & 0xFFFFFFF0U) == ZSTD1_MAGIC_SKIPPABLE_START) return 1;
    }
#if defined(ZSTD1_LEGACY_SUPPORT) && (ZSTD1_LEGACY_SUPPORT >= 1)
    if (ZSTD1_isLegacy(buffer, size)) return 1;
#endif
    return 0;
}


/** ZSTD1_frameHeaderSize() :
*   srcSize must be >= ZSTD1_frameHeaderSize_prefix.
*   @return : size of the Frame Header */
size_t ZSTD1_frameHeaderSize(const void* src, size_t srcSize)
{
    if (srcSize < ZSTD1_frameHeaderSize_prefix) return ERROR(srcSize_wrong);
    {   BYTE const fhd = ((const BYTE*)src)[4];
        U32 const dictID= fhd & 3;
        U32 const singleSegment = (fhd >> 5) & 1;
        U32 const fcsId = fhd >> 6;
        return ZSTD1_frameHeaderSize_prefix + !singleSegment + ZSTD1_did_fieldSize[dictID] + ZSTD1_fcs_fieldSize[fcsId]
                + (singleSegment && !fcsId);
    }
}


/** ZSTD1_getFrameHeader() :
*   decode Frame Header, or require larger `srcSize`.
*   @return : 0, `zfhPtr` is correctly filled,
*            >0, `srcSize` is too small, result is expected `srcSize`,
*             or an error code, which can be tested using ZSTD1_isError() */
size_t ZSTD1_getFrameHeader(ZSTD1_frameHeader* zfhPtr, const void* src, size_t srcSize)
{
    const BYTE* ip = (const BYTE*)src;
    if (srcSize < ZSTD1_frameHeaderSize_prefix) return ZSTD1_frameHeaderSize_prefix;

    if (MEM_readLE32(src) != ZSTD1_MAGICNUMBER) {
        if ((MEM_readLE32(src) & 0xFFFFFFF0U) == ZSTD1_MAGIC_SKIPPABLE_START) {
            /* skippable frame */
            if (srcSize < ZSTD1_skippableHeaderSize)
                return ZSTD1_skippableHeaderSize; /* magic number + frame length */
            memset(zfhPtr, 0, sizeof(*zfhPtr));
            zfhPtr->frameContentSize = MEM_readLE32((const char *)src + 4);
            zfhPtr->windowSize = 0; /* windowSize==0 means a frame is skippable */
            return 0;
        }
        return ERROR(prefix_unknown);
    }

    /* ensure there is enough `srcSize` to fully read/decode frame header */
    { size_t const fhsize = ZSTD1_frameHeaderSize(src, srcSize);
      if (srcSize < fhsize) return fhsize; }

    {   BYTE const fhdByte = ip[4];
        size_t pos = 5;
        U32 const dictIDSizeCode = fhdByte&3;
        U32 const checksumFlag = (fhdByte>>2)&1;
        U32 const singleSegment = (fhdByte>>5)&1;
        U32 const fcsID = fhdByte>>6;
        U32 const windowSizeMax = 1U << ZSTD1_WINDOWLOG_MAX;
        U32 windowSize = 0;
        U32 dictID = 0;
        U64 frameContentSize = 0;
        if ((fhdByte & 0x08) != 0)
            return ERROR(frameParameter_unsupported);   /* reserved bits, must be zero */
        if (!singleSegment) {
            BYTE const wlByte = ip[pos++];
            U32 const windowLog = (wlByte >> 3) + ZSTD1_WINDOWLOG_ABSOLUTEMIN;
            if (windowLog > ZSTD1_WINDOWLOG_MAX)
                return ERROR(frameParameter_windowTooLarge);
            windowSize = (1U << windowLog);
            windowSize += (windowSize >> 3) * (wlByte&7);
        }

        switch(dictIDSizeCode)
        {
            default:   /* impossible */
            case 0 : break;
            case 1 : dictID = ip[pos]; pos++; break;
            case 2 : dictID = MEM_readLE16(ip+pos); pos+=2; break;
            case 3 : dictID = MEM_readLE32(ip+pos); pos+=4; break;
        }
        switch(fcsID)
        {
            default:   /* impossible */
            case 0 : if (singleSegment) frameContentSize = ip[pos]; break;
            case 1 : frameContentSize = MEM_readLE16(ip+pos)+256; break;
            case 2 : frameContentSize = MEM_readLE32(ip+pos); break;
            case 3 : frameContentSize = MEM_readLE64(ip+pos); break;
        }
        if (!windowSize) windowSize = (U32)frameContentSize;
        if (windowSize > windowSizeMax) return ERROR(frameParameter_windowTooLarge);
        zfhPtr->frameContentSize = frameContentSize;
        zfhPtr->windowSize = windowSize;
        zfhPtr->dictID = dictID;
        zfhPtr->checksumFlag = checksumFlag;
    }
    return 0;
}

/** ZSTD1_getFrameContentSize() :
*   compatible with legacy mode
*   @return : decompressed size of the single frame pointed to be `src` if known, otherwise
*             - ZSTD1_CONTENTSIZE_UNKNOWN if the size cannot be determined
*             - ZSTD1_CONTENTSIZE_ERROR if an error occurred (e.g. invalid magic number, srcSize too small) */
unsigned long long ZSTD1_getFrameContentSize(const void *src, size_t srcSize)
{
#if defined(ZSTD1_LEGACY_SUPPORT) && (ZSTD1_LEGACY_SUPPORT >= 1)
    if (ZSTD1_isLegacy(src, srcSize)) {
        unsigned long long const ret = ZSTD1_getDecompressedSize_legacy(src, srcSize);
        return ret == 0 ? ZSTD1_CONTENTSIZE_UNKNOWN : ret;
    }
#endif
    {   ZSTD1_frameHeader fParams;
        if (ZSTD1_getFrameHeader(&fParams, src, srcSize) != 0) return ZSTD1_CONTENTSIZE_ERROR;
        if (fParams.windowSize == 0) {
            /* Either skippable or empty frame, size == 0 either way */
            return 0;
        } else if (fParams.frameContentSize != 0) {
            return fParams.frameContentSize;
        } else {
            return ZSTD1_CONTENTSIZE_UNKNOWN;
        }
    }
}

/** ZSTD1_findDecompressedSize() :
 *  compatible with legacy mode
 *  `srcSize` must be the exact length of some number of ZSTD1 compressed and/or
 *      skippable frames
 *  @return : decompressed size of the frames contained */
unsigned long long ZSTD1_findDecompressedSize(const void* src, size_t srcSize)
{
    unsigned long long totalDstSize = 0;

    while (srcSize >= ZSTD1_frameHeaderSize_prefix) {
        const U32 magicNumber = MEM_readLE32(src);

        if ((magicNumber & 0xFFFFFFF0U) == ZSTD1_MAGIC_SKIPPABLE_START) {
            size_t skippableSize;
            if (srcSize < ZSTD1_skippableHeaderSize)
                return ERROR(srcSize_wrong);
            skippableSize = MEM_readLE32((const BYTE *)src + 4) +
                            ZSTD1_skippableHeaderSize;
            if (srcSize < skippableSize) {
                return ZSTD1_CONTENTSIZE_ERROR;
            }

            src = (const BYTE *)src + skippableSize;
            srcSize -= skippableSize;
            continue;
        }

        {   unsigned long long const ret = ZSTD1_getFrameContentSize(src, srcSize);
            if (ret >= ZSTD1_CONTENTSIZE_ERROR) return ret;

            /* check for overflow */
            if (totalDstSize + ret < totalDstSize) return ZSTD1_CONTENTSIZE_ERROR;
            totalDstSize += ret;
        }
        {   size_t const frameSrcSize = ZSTD1_findFrameCompressedSize(src, srcSize);
            if (ZSTD1_isError(frameSrcSize)) {
                return ZSTD1_CONTENTSIZE_ERROR;
            }

            src = (const BYTE *)src + frameSrcSize;
            srcSize -= frameSrcSize;
        }
    }

    if (srcSize) {
        return ZSTD1_CONTENTSIZE_ERROR;
    }

    return totalDstSize;
}

/** ZSTD1_getDecompressedSize() :
*   compatible with legacy mode
*   @return : decompressed size if known, 0 otherwise
              note : 0 can mean any of the following :
                   - decompressed size is not present within frame header
                   - frame header unknown / not supported
                   - frame header not complete (`srcSize` too small) */
unsigned long long ZSTD1_getDecompressedSize(const void* src, size_t srcSize)
{
    unsigned long long const ret = ZSTD1_getFrameContentSize(src, srcSize);
    return ret >= ZSTD1_CONTENTSIZE_ERROR ? 0 : ret;
}


/** ZSTD1_decodeFrameHeader() :
*   `headerSize` must be the size provided by ZSTD1_frameHeaderSize().
*   @return : 0 if success, or an error code, which can be tested using ZSTD1_isError() */
static size_t ZSTD1_decodeFrameHeader(ZSTD1_DCtx* dctx, const void* src, size_t headerSize)
{
    size_t const result = ZSTD1_getFrameHeader(&(dctx->fParams), src, headerSize);
    if (ZSTD1_isError(result)) return result;  /* invalid header */
    if (result>0) return ERROR(srcSize_wrong);   /* headerSize too small */
    if (dctx->fParams.dictID && (dctx->dictID != dctx->fParams.dictID)) return ERROR(dictionary_wrong);
    if (dctx->fParams.checksumFlag) XXH64_reset(&dctx->xxhState, 0);
    return 0;
}


typedef struct
{
    blockType_e blockType;
    U32 lastBlock;
    U32 origSize;
} blockProperties_t;

/*! ZSTD1_getcBlockSize() :
*   Provides the size of compressed block from block header `src` */
size_t ZSTD1_getcBlockSize(const void* src, size_t srcSize,
                          blockProperties_t* bpPtr)
{
    if (srcSize < ZSTD1_blockHeaderSize) return ERROR(srcSize_wrong);
    {   U32 const cBlockHeader = MEM_readLE24(src);
        U32 const cSize = cBlockHeader >> 3;
        bpPtr->lastBlock = cBlockHeader & 1;
        bpPtr->blockType = (blockType_e)((cBlockHeader >> 1) & 3);
        bpPtr->origSize = cSize;   /* only useful for RLE */
        if (bpPtr->blockType == bt_rle) return 1;
        if (bpPtr->blockType == bt_reserved) return ERROR(corruption_detected);
        return cSize;
    }
}


static size_t ZSTD1_copyRawBlock(void* dst, size_t dstCapacity,
                          const void* src, size_t srcSize)
{
    if (srcSize > dstCapacity) return ERROR(dstSize_tooSmall);
    memcpy(dst, src, srcSize);
    return srcSize;
}


static size_t ZSTD1_setRleBlock(void* dst, size_t dstCapacity,
                         const void* src, size_t srcSize,
                               size_t regenSize)
{
    if (srcSize != 1) return ERROR(srcSize_wrong);
    if (regenSize > dstCapacity) return ERROR(dstSize_tooSmall);
    memset(dst, *(const BYTE*)src, regenSize);
    return regenSize;
}

/*! ZSTD1_decodeLiteralsBlock() :
    @return : nb of bytes read from src (< srcSize ) */
size_t ZSTD1_decodeLiteralsBlock(ZSTD1_DCtx* dctx,
                          const void* src, size_t srcSize)   /* note : srcSize < BLOCKSIZE */
{
    if (srcSize < MIN_CBLOCK_SIZE) return ERROR(corruption_detected);

    {   const BYTE* const istart = (const BYTE*) src;
        symbolEncodingType_e const litEncType = (symbolEncodingType_e)(istart[0] & 3);

        switch(litEncType)
        {
        case set_repeat:
            if (dctx->litEntropy==0) return ERROR(dictionary_corrupted);
            /* fall-through */
        case set_compressed:
            if (srcSize < 5) return ERROR(corruption_detected);   /* srcSize >= MIN_CBLOCK_SIZE == 3; here we need up to 5 for case 3 */
            {   size_t lhSize, litSize, litCSize;
                U32 singleStream=0;
                U32 const lhlCode = (istart[0] >> 2) & 3;
                U32 const lhc = MEM_readLE32(istart);
                switch(lhlCode)
                {
                case 0: case 1: default:   /* note : default is impossible, since lhlCode into [0..3] */
                    /* 2 - 2 - 10 - 10 */
                    singleStream = !lhlCode;
                    lhSize = 3;
                    litSize  = (lhc >> 4) & 0x3FF;
                    litCSize = (lhc >> 14) & 0x3FF;
                    break;
                case 2:
                    /* 2 - 2 - 14 - 14 */
                    lhSize = 4;
                    litSize  = (lhc >> 4) & 0x3FFF;
                    litCSize = lhc >> 18;
                    break;
                case 3:
                    /* 2 - 2 - 18 - 18 */
                    lhSize = 5;
                    litSize  = (lhc >> 4) & 0x3FFFF;
                    litCSize = (lhc >> 22) + (istart[4] << 10);
                    break;
                }
                if (litSize > ZSTD1_BLOCKSIZE_MAX) return ERROR(corruption_detected);
                if (litCSize + lhSize > srcSize) return ERROR(corruption_detected);

                if (HUF1_isError((litEncType==set_repeat) ?
                                    ( singleStream ?
                                        HUF1_decompress1X_usingDTable(dctx->litBuffer, litSize, istart+lhSize, litCSize, dctx->HUF1ptr) :
                                        HUF1_decompress4X_usingDTable(dctx->litBuffer, litSize, istart+lhSize, litCSize, dctx->HUF1ptr) ) :
                                    ( singleStream ?
                                        HUF1_decompress1X2_DCtx_wksp(dctx->entropy.hufTable, dctx->litBuffer, litSize, istart+lhSize, litCSize,
                                                                    dctx->entropy.workspace, sizeof(dctx->entropy.workspace)) :
                                        HUF1_decompress4X_hufOnly_wksp(dctx->entropy.hufTable, dctx->litBuffer, litSize, istart+lhSize, litCSize,
                                                                      dctx->entropy.workspace, sizeof(dctx->entropy.workspace)))))
                    return ERROR(corruption_detected);

                dctx->litPtr = dctx->litBuffer;
                dctx->litSize = litSize;
                dctx->litEntropy = 1;
                if (litEncType==set_compressed) dctx->HUF1ptr = dctx->entropy.hufTable;
                memset(dctx->litBuffer + dctx->litSize, 0, WILDCOPY_OVERLENGTH);
                return litCSize + lhSize;
            }

        case set_basic:
            {   size_t litSize, lhSize;
                U32 const lhlCode = ((istart[0]) >> 2) & 3;
                switch(lhlCode)
                {
                case 0: case 2: default:   /* note : default is impossible, since lhlCode into [0..3] */
                    lhSize = 1;
                    litSize = istart[0] >> 3;
                    break;
                case 1:
                    lhSize = 2;
                    litSize = MEM_readLE16(istart) >> 4;
                    break;
                case 3:
                    lhSize = 3;
                    litSize = MEM_readLE24(istart) >> 4;
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

        case set_rle:
            {   U32 const lhlCode = ((istart[0]) >> 2) & 3;
                size_t litSize, lhSize;
                switch(lhlCode)
                {
                case 0: case 2: default:   /* note : default is impossible, since lhlCode into [0..3] */
                    lhSize = 1;
                    litSize = istart[0] >> 3;
                    break;
                case 1:
                    lhSize = 2;
                    litSize = MEM_readLE16(istart) >> 4;
                    break;
                case 3:
                    lhSize = 3;
                    litSize = MEM_readLE24(istart) >> 4;
                    if (srcSize<4) return ERROR(corruption_detected);   /* srcSize >= MIN_CBLOCK_SIZE == 3; here we need lhSize+1 = 4 */
                    break;
                }
                if (litSize > ZSTD1_BLOCKSIZE_MAX) return ERROR(corruption_detected);
                memset(dctx->litBuffer, istart[lhSize], litSize + WILDCOPY_OVERLENGTH);
                dctx->litPtr = dctx->litBuffer;
                dctx->litSize = litSize;
                return lhSize+1;
            }
        default:
            return ERROR(corruption_detected);   /* impossible */
        }
    }
}


typedef union {
    FSE1_decode_t realData;
    U32 alignedBy4;
} FSE1_decode_t4;

/* Default FSE1 distribution table for Literal Lengths */
static const FSE1_decode_t4 LL_defaultDTable[(1<<LL_DEFAULTNORMLOG)+1] = {
    { { LL_DEFAULTNORMLOG, 1, 1 } }, /* header : tableLog, fastMode, fastMode */
     /* base, symbol, bits */
    { {  0,  0,  4 } }, { { 16,  0,  4 } }, { { 32,  1,  5 } }, { {  0,  3,  5 } },
    { {  0,  4,  5 } }, { {  0,  6,  5 } }, { {  0,  7,  5 } }, { {  0,  9,  5 } },
    { {  0, 10,  5 } }, { {  0, 12,  5 } }, { {  0, 14,  6 } }, { {  0, 16,  5 } },
    { {  0, 18,  5 } }, { {  0, 19,  5 } }, { {  0, 21,  5 } }, { {  0, 22,  5 } },
    { {  0, 24,  5 } }, { { 32, 25,  5 } }, { {  0, 26,  5 } }, { {  0, 27,  6 } },
    { {  0, 29,  6 } }, { {  0, 31,  6 } }, { { 32,  0,  4 } }, { {  0,  1,  4 } },
    { {  0,  2,  5 } }, { { 32,  4,  5 } }, { {  0,  5,  5 } }, { { 32,  7,  5 } },
    { {  0,  8,  5 } }, { { 32, 10,  5 } }, { {  0, 11,  5 } }, { {  0, 13,  6 } },
    { { 32, 16,  5 } }, { {  0, 17,  5 } }, { { 32, 19,  5 } }, { {  0, 20,  5 } },
    { { 32, 22,  5 } }, { {  0, 23,  5 } }, { {  0, 25,  4 } }, { { 16, 25,  4 } },
    { { 32, 26,  5 } }, { {  0, 28,  6 } }, { {  0, 30,  6 } }, { { 48,  0,  4 } },
    { { 16,  1,  4 } }, { { 32,  2,  5 } }, { { 32,  3,  5 } }, { { 32,  5,  5 } },
    { { 32,  6,  5 } }, { { 32,  8,  5 } }, { { 32,  9,  5 } }, { { 32, 11,  5 } },
    { { 32, 12,  5 } }, { {  0, 15,  6 } }, { { 32, 17,  5 } }, { { 32, 18,  5 } },
    { { 32, 20,  5 } }, { { 32, 21,  5 } }, { { 32, 23,  5 } }, { { 32, 24,  5 } },
    { {  0, 35,  6 } }, { {  0, 34,  6 } }, { {  0, 33,  6 } }, { {  0, 32,  6 } },
};   /* LL_defaultDTable */

/* Default FSE1 distribution table for Match Lengths */
static const FSE1_decode_t4 ML_defaultDTable[(1<<ML_DEFAULTNORMLOG)+1] = {
    { { ML_DEFAULTNORMLOG, 1, 1 } }, /* header : tableLog, fastMode, fastMode */
    /* base, symbol, bits */
    { {  0,  0,  6 } }, { {  0,  1,  4 } }, { { 32,  2,  5 } }, { {  0,  3,  5 } },
    { {  0,  5,  5 } }, { {  0,  6,  5 } }, { {  0,  8,  5 } }, { {  0, 10,  6 } },
    { {  0, 13,  6 } }, { {  0, 16,  6 } }, { {  0, 19,  6 } }, { {  0, 22,  6 } },
    { {  0, 25,  6 } }, { {  0, 28,  6 } }, { {  0, 31,  6 } }, { {  0, 33,  6 } },
    { {  0, 35,  6 } }, { {  0, 37,  6 } }, { {  0, 39,  6 } }, { {  0, 41,  6 } },
    { {  0, 43,  6 } }, { {  0, 45,  6 } }, { { 16,  1,  4 } }, { {  0,  2,  4 } },
    { { 32,  3,  5 } }, { {  0,  4,  5 } }, { { 32,  6,  5 } }, { {  0,  7,  5 } },
    { {  0,  9,  6 } }, { {  0, 12,  6 } }, { {  0, 15,  6 } }, { {  0, 18,  6 } },
    { {  0, 21,  6 } }, { {  0, 24,  6 } }, { {  0, 27,  6 } }, { {  0, 30,  6 } },
    { {  0, 32,  6 } }, { {  0, 34,  6 } }, { {  0, 36,  6 } }, { {  0, 38,  6 } },
    { {  0, 40,  6 } }, { {  0, 42,  6 } }, { {  0, 44,  6 } }, { { 32,  1,  4 } },
    { { 48,  1,  4 } }, { { 16,  2,  4 } }, { { 32,  4,  5 } }, { { 32,  5,  5 } },
    { { 32,  7,  5 } }, { { 32,  8,  5 } }, { {  0, 11,  6 } }, { {  0, 14,  6 } },
    { {  0, 17,  6 } }, { {  0, 20,  6 } }, { {  0, 23,  6 } }, { {  0, 26,  6 } },
    { {  0, 29,  6 } }, { {  0, 52,  6 } }, { {  0, 51,  6 } }, { {  0, 50,  6 } },
    { {  0, 49,  6 } }, { {  0, 48,  6 } }, { {  0, 47,  6 } }, { {  0, 46,  6 } },
};   /* ML_defaultDTable */

/* Default FSE1 distribution table for Offset Codes */
static const FSE1_decode_t4 OF_defaultDTable[(1<<OF_DEFAULTNORMLOG)+1] = {
    { { OF_DEFAULTNORMLOG, 1, 1 } }, /* header : tableLog, fastMode, fastMode */
    /* base, symbol, bits */
    { {  0,  0,  5 } }, { {  0,  6,  4 } },
    { {  0,  9,  5 } }, { {  0, 15,  5 } },
    { {  0, 21,  5 } }, { {  0,  3,  5 } },
    { {  0,  7,  4 } }, { {  0, 12,  5 } },
    { {  0, 18,  5 } }, { {  0, 23,  5 } },
    { {  0,  5,  5 } }, { {  0,  8,  4 } },
    { {  0, 14,  5 } }, { {  0, 20,  5 } },
    { {  0,  2,  5 } }, { { 16,  7,  4 } },
    { {  0, 11,  5 } }, { {  0, 17,  5 } },
    { {  0, 22,  5 } }, { {  0,  4,  5 } },
    { { 16,  8,  4 } }, { {  0, 13,  5 } },
    { {  0, 19,  5 } }, { {  0,  1,  5 } },
    { { 16,  6,  4 } }, { {  0, 10,  5 } },
    { {  0, 16,  5 } }, { {  0, 28,  5 } },
    { {  0, 27,  5 } }, { {  0, 26,  5 } },
    { {  0, 25,  5 } }, { {  0, 24,  5 } },
};   /* OF_defaultDTable */

/*! ZSTD1_buildSeqTable() :
    @return : nb bytes read from src,
              or an error code if it fails, testable with ZSTD1_isError()
*/
static size_t ZSTD1_buildSeqTable(FSE1_DTable* DTableSpace, const FSE1_DTable** DTablePtr,
                                 symbolEncodingType_e type, U32 max, U32 maxLog,
                                 const void* src, size_t srcSize,
                                 const FSE1_decode_t4* defaultTable, U32 flagRepeatTable)
{
    const void* const tmpPtr = defaultTable;   /* bypass strict aliasing */
    switch(type)
    {
    case set_rle :
        if (!srcSize) return ERROR(srcSize_wrong);
        if ( (*(const BYTE*)src) > max) return ERROR(corruption_detected);
        FSE1_buildDTable_rle(DTableSpace, *(const BYTE*)src);
        *DTablePtr = DTableSpace;
        return 1;
    case set_basic :
        *DTablePtr = (const FSE1_DTable*)tmpPtr;
        return 0;
    case set_repeat:
        if (!flagRepeatTable) return ERROR(corruption_detected);
        return 0;
    default :   /* impossible */
    case set_compressed :
        {   U32 tableLog;
            S16 norm[MaxSeq+1];
            size_t const headerSize = FSE1_readNCount(norm, &max, &tableLog, src, srcSize);
            if (FSE1_isError(headerSize)) return ERROR(corruption_detected);
            if (tableLog > maxLog) return ERROR(corruption_detected);
            FSE1_buildDTable(DTableSpace, norm, max, tableLog);
            *DTablePtr = DTableSpace;
            return headerSize;
    }   }
}

size_t ZSTD1_decodeSeqHeaders(ZSTD1_DCtx* dctx, int* nbSeqPtr,
                             const void* src, size_t srcSize)
{
    const BYTE* const istart = (const BYTE* const)src;
    const BYTE* const iend = istart + srcSize;
    const BYTE* ip = istart;
    DEBUGLOG(5, "ZSTD1_decodeSeqHeaders");

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
    if (ip+4 > iend) return ERROR(srcSize_wrong); /* minimum possible size */
    {   symbolEncodingType_e const LLtype = (symbolEncodingType_e)(*ip >> 6);
        symbolEncodingType_e const OFtype = (symbolEncodingType_e)((*ip >> 4) & 3);
        symbolEncodingType_e const MLtype = (symbolEncodingType_e)((*ip >> 2) & 3);
        ip++;

        /* Build DTables */
        {   size_t const llhSize = ZSTD1_buildSeqTable(dctx->entropy.LLTable, &dctx->LLTptr,
                                                      LLtype, MaxLL, LLFSE1Log,
                                                      ip, iend-ip, LL_defaultDTable, dctx->fseEntropy);
            if (ZSTD1_isError(llhSize)) return ERROR(corruption_detected);
            ip += llhSize;
        }
        {   size_t const ofhSize = ZSTD1_buildSeqTable(dctx->entropy.OFTable, &dctx->OFTptr,
                                                      OFtype, MaxOff, OffFSE1Log,
                                                      ip, iend-ip, OF_defaultDTable, dctx->fseEntropy);
            if (ZSTD1_isError(ofhSize)) return ERROR(corruption_detected);
            ip += ofhSize;
        }
        {   size_t const mlhSize = ZSTD1_buildSeqTable(dctx->entropy.MLTable, &dctx->MLTptr,
                                                      MLtype, MaxML, MLFSE1Log,
                                                      ip, iend-ip, ML_defaultDTable, dctx->fseEntropy);
            if (ZSTD1_isError(mlhSize)) return ERROR(corruption_detected);
            ip += mlhSize;
        }
    }

    return ip-istart;
}


typedef struct {
    size_t litLength;
    size_t matchLength;
    size_t offset;
    const BYTE* match;
} seq_t;

typedef struct {
    BIT_DStream_t DStream;
    FSE1_DState_t stateLL;
    FSE1_DState_t stateOffb;
    FSE1_DState_t stateML;
    size_t prevOffset[ZSTD1_REP_NUM];
    const BYTE* base;
    size_t pos;
    uPtrDiff gotoDict;
} seqState_t;


FORCE_NOINLINE
size_t ZSTD1_execSequenceLast7(BYTE* op,
                              BYTE* const oend, seq_t sequence,
                              const BYTE** litPtr, const BYTE* const litLimit,
                              const BYTE* const base, const BYTE* const vBase, const BYTE* const dictEnd)
{
    BYTE* const oLitEnd = op + sequence.litLength;
    size_t const sequenceLength = sequence.litLength + sequence.matchLength;
    BYTE* const oMatchEnd = op + sequenceLength;   /* risk : address space overflow (32-bits) */
    BYTE* const oend_w = oend - WILDCOPY_OVERLENGTH;
    const BYTE* const iLitEnd = *litPtr + sequence.litLength;
    const BYTE* match = oLitEnd - sequence.offset;

    /* check */
    if (oMatchEnd>oend) return ERROR(dstSize_tooSmall); /* last match must start at a minimum distance of WILDCOPY_OVERLENGTH from oend */
    if (iLitEnd > litLimit) return ERROR(corruption_detected);   /* over-read beyond lit buffer */
    if (oLitEnd <= oend_w) return ERROR(GENERIC);   /* Precondition */

    /* copy literals */
    if (op < oend_w) {
        ZSTD1_wildcopy(op, *litPtr, oend_w - op);
        *litPtr += oend_w - op;
        op = oend_w;
    }
    while (op < oLitEnd) *op++ = *(*litPtr)++;

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
    }   }
    while (op < oMatchEnd) *op++ = *match++;
    return sequenceLength;
}


static seq_t ZSTD1_decodeSequence(seqState_t* seqState)
{
    seq_t seq;

    U32 const llCode = FSE1_peekSymbol(&seqState->stateLL);
    U32 const mlCode = FSE1_peekSymbol(&seqState->stateML);
    U32 const ofCode = FSE1_peekSymbol(&seqState->stateOffb);   /* <= maxOff, by table construction */

    U32 const llBits = LL_bits[llCode];
    U32 const mlBits = ML_bits[mlCode];
    U32 const ofBits = ofCode;
    U32 const totalBits = llBits+mlBits+ofBits;

    static const U32 LL_base[MaxLL+1] = {
                             0,    1,    2,     3,     4,     5,     6,      7,
                             8,    9,   10,    11,    12,    13,    14,     15,
                            16,   18,   20,    22,    24,    28,    32,     40,
                            48,   64, 0x80, 0x100, 0x200, 0x400, 0x800, 0x1000,
                            0x2000, 0x4000, 0x8000, 0x10000 };

    static const U32 ML_base[MaxML+1] = {
                             3,  4,  5,    6,     7,     8,     9,    10,
                            11, 12, 13,   14,    15,    16,    17,    18,
                            19, 20, 21,   22,    23,    24,    25,    26,
                            27, 28, 29,   30,    31,    32,    33,    34,
                            35, 37, 39,   41,    43,    47,    51,    59,
                            67, 83, 99, 0x83, 0x103, 0x203, 0x403, 0x803,
                            0x1003, 0x2003, 0x4003, 0x8003, 0x10003 };

    static const U32 OF_base[MaxOff+1] = {
                     0,        1,       1,       5,     0xD,     0x1D,     0x3D,     0x7D,
                     0xFD,   0x1FD,   0x3FD,   0x7FD,   0xFFD,   0x1FFD,   0x3FFD,   0x7FFD,
                     0xFFFD, 0x1FFFD, 0x3FFFD, 0x7FFFD, 0xFFFFD, 0x1FFFFD, 0x3FFFFD, 0x7FFFFD,
                     0xFFFFFD, 0x1FFFFFD, 0x3FFFFFD, 0x7FFFFFD, 0xFFFFFFD };

    /* sequence */
    {   size_t offset;
        if (!ofCode)
            offset = 0;
        else {
            offset = OF_base[ofCode] + BIT_readBitsFast(&seqState->DStream, ofBits);   /* <=  (ZSTD1_WINDOWLOG_MAX-1) bits */
            if (MEM_32bits()) BIT_reloadDStream(&seqState->DStream);
        }

        if (ofCode <= 1) {
            offset += (llCode==0);
            if (offset) {
                size_t temp = (offset==3) ? seqState->prevOffset[0] - 1 : seqState->prevOffset[offset];
                temp += !temp;   /* 0 is not valid; input is corrupted; force offset to 1 */
                if (offset != 1) seqState->prevOffset[2] = seqState->prevOffset[1];
                seqState->prevOffset[1] = seqState->prevOffset[0];
                seqState->prevOffset[0] = offset = temp;
            } else {
                offset = seqState->prevOffset[0];
            }
        } else {
            seqState->prevOffset[2] = seqState->prevOffset[1];
            seqState->prevOffset[1] = seqState->prevOffset[0];
            seqState->prevOffset[0] = offset;
        }
        seq.offset = offset;
    }

    seq.matchLength = ML_base[mlCode]
                    + ((mlCode>31) ? BIT_readBitsFast(&seqState->DStream, mlBits) : 0);  /* <=  16 bits */
    if (MEM_32bits() && (mlBits+llBits>24)) BIT_reloadDStream(&seqState->DStream);

    seq.litLength = LL_base[llCode]
                  + ((llCode>15) ? BIT_readBitsFast(&seqState->DStream, llBits) : 0);    /* <=  16 bits */
    if (  MEM_32bits()
      || (totalBits > 64 - 7 - (LLFSE1Log+MLFSE1Log+OffFSE1Log)) )
       BIT_reloadDStream(&seqState->DStream);

    DEBUGLOG(6, "seq: litL=%u, matchL=%u, offset=%u",
                (U32)seq.litLength, (U32)seq.matchLength, (U32)seq.offset);

    /* ANS state update */
    FSE1_updateState(&seqState->stateLL, &seqState->DStream);    /* <=  9 bits */
    FSE1_updateState(&seqState->stateML, &seqState->DStream);    /* <=  9 bits */
    if (MEM_32bits()) BIT_reloadDStream(&seqState->DStream);    /* <= 18 bits */
    FSE1_updateState(&seqState->stateOffb, &seqState->DStream);  /* <=  8 bits */

    return seq;
}


FORCE_INLINE
size_t ZSTD1_execSequence(BYTE* op,
                         BYTE* const oend, seq_t sequence,
                         const BYTE** litPtr, const BYTE* const litLimit,
                         const BYTE* const base, const BYTE* const vBase, const BYTE* const dictEnd)
{
    BYTE* const oLitEnd = op + sequence.litLength;
    size_t const sequenceLength = sequence.litLength + sequence.matchLength;
    BYTE* const oMatchEnd = op + sequenceLength;   /* risk : address space overflow (32-bits) */
    BYTE* const oend_w = oend - WILDCOPY_OVERLENGTH;
    const BYTE* const iLitEnd = *litPtr + sequence.litLength;
    const BYTE* match = oLitEnd - sequence.offset;

    /* check */
    if (oMatchEnd>oend) return ERROR(dstSize_tooSmall); /* last match must start at a minimum distance of WILDCOPY_OVERLENGTH from oend */
    if (iLitEnd > litLimit) return ERROR(corruption_detected);   /* over-read beyond lit buffer */
    if (oLitEnd>oend_w) return ZSTD1_execSequenceLast7(op, oend, sequence, litPtr, litLimit, base, vBase, dictEnd);

    /* copy Literals */
    ZSTD1_copy8(op, *litPtr);
    if (sequence.litLength > 8)
        ZSTD1_wildcopy(op+8, (*litPtr)+8, sequence.litLength - 8);   /* note : since oLitEnd <= oend-WILDCOPY_OVERLENGTH, no risk of overwrite beyond oend */
    op = oLitEnd;
    *litPtr = iLitEnd;   /* update for next sequence */

    /* copy Match */
    if (sequence.offset > (size_t)(oLitEnd - base)) {
        /* offset beyond prefix -> go into extDict */
        if (sequence.offset > (size_t)(oLitEnd - vBase))
            return ERROR(corruption_detected);
        match = dictEnd + (match - base);
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
            if (op > oend_w || sequence.matchLength < MINMATCH) {
              U32 i;
              for (i = 0; i < sequence.matchLength; ++i) op[i] = match[i];
              return sequenceLength;
            }
    }   }
    /* Requirement: op <= oend_w && sequence.matchLength >= MINMATCH */

    /* match within prefix */
    if (sequence.offset < 8) {
        /* close range match, overlap */
        static const U32 dec32table[] = { 0, 1, 2, 1, 4, 4, 4, 4 };   /* added */
        static const int dec64table[] = { 8, 8, 8, 7, 8, 9,10,11 };   /* subtracted */
        int const sub2 = dec64table[sequence.offset];
        op[0] = match[0];
        op[1] = match[1];
        op[2] = match[2];
        op[3] = match[3];
        match += dec32table[sequence.offset];
        ZSTD1_copy4(op+4, match);
        match -= sub2;
    } else {
        ZSTD1_copy8(op, match);
    }
    op += 8; match += 8;

    if (oMatchEnd > oend-(16-MINMATCH)) {
        if (op < oend_w) {
            ZSTD1_wildcopy(op, match, oend_w - op);
            match += oend_w - op;
            op = oend_w;
        }
        while (op < oMatchEnd) *op++ = *match++;
    } else {
        ZSTD1_wildcopy(op, match, (ptrdiff_t)sequence.matchLength-8);   /* works even if matchLength < 8 */
    }
    return sequenceLength;
}


static size_t ZSTD1_decompressSequences(
                               ZSTD1_DCtx* dctx,
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
    const BYTE* const base = (const BYTE*) (dctx->base);
    const BYTE* const vBase = (const BYTE*) (dctx->vBase);
    const BYTE* const dictEnd = (const BYTE*) (dctx->dictEnd);
    int nbSeq;
    DEBUGLOG(5, "ZSTD1_decompressSequences");

    /* Build Decoding Tables */
    {   size_t const seqHSize = ZSTD1_decodeSeqHeaders(dctx, &nbSeq, ip, seqSize);
        DEBUGLOG(5, "ZSTD1_decodeSeqHeaders: size=%u, nbSeq=%i",
                    (U32)seqHSize, nbSeq);
        if (ZSTD1_isError(seqHSize)) return seqHSize;
        ip += seqHSize;
    }

    /* Regen sequences */
    if (nbSeq) {
        seqState_t seqState;
        dctx->fseEntropy = 1;
        { U32 i; for (i=0; i<ZSTD1_REP_NUM; i++) seqState.prevOffset[i] = dctx->entropy.rep[i]; }
        CHECK_E(BIT_initDStream(&seqState.DStream, ip, iend-ip), corruption_detected);
        FSE1_initDState(&seqState.stateLL, &seqState.DStream, dctx->LLTptr);
        FSE1_initDState(&seqState.stateOffb, &seqState.DStream, dctx->OFTptr);
        FSE1_initDState(&seqState.stateML, &seqState.DStream, dctx->MLTptr);

        for ( ; (BIT_reloadDStream(&(seqState.DStream)) <= BIT_DStream_completed) && nbSeq ; ) {
            nbSeq--;
            {   seq_t const sequence = ZSTD1_decodeSequence(&seqState);
                size_t const oneSeqSize = ZSTD1_execSequence(op, oend, sequence, &litPtr, litEnd, base, vBase, dictEnd);
                DEBUGLOG(6, "regenerated sequence size : %u", (U32)oneSeqSize);
                if (ZSTD1_isError(oneSeqSize)) return oneSeqSize;
                op += oneSeqSize;
        }   }

        /* check if reached exact end */
        DEBUGLOG(5, "after decode loop, remaining nbSeq : %i", nbSeq);
        if (nbSeq) return ERROR(corruption_detected);
        /* save reps for next block */
        { U32 i; for (i=0; i<ZSTD1_REP_NUM; i++) dctx->entropy.rep[i] = (U32)(seqState.prevOffset[i]); }
    }

    /* last literal segment */
    {   size_t const lastLLSize = litEnd - litPtr;
        if (lastLLSize > (size_t)(oend-op)) return ERROR(dstSize_tooSmall);
        memcpy(op, litPtr, lastLLSize);
        op += lastLLSize;
    }

    return op-ostart;
}


FORCE_INLINE seq_t ZSTD1_decodeSequenceLong_generic(seqState_t* seqState, int const longOffsets)
{
    seq_t seq;

    U32 const llCode = FSE1_peekSymbol(&seqState->stateLL);
    U32 const mlCode = FSE1_peekSymbol(&seqState->stateML);
    U32 const ofCode = FSE1_peekSymbol(&seqState->stateOffb);   /* <= maxOff, by table construction */

    U32 const llBits = LL_bits[llCode];
    U32 const mlBits = ML_bits[mlCode];
    U32 const ofBits = ofCode;
    U32 const totalBits = llBits+mlBits+ofBits;

    static const U32 LL_base[MaxLL+1] = {
                             0,  1,    2,     3,     4,     5,     6,      7,
                             8,  9,   10,    11,    12,    13,    14,     15,
                            16, 18,   20,    22,    24,    28,    32,     40,
                            48, 64, 0x80, 0x100, 0x200, 0x400, 0x800, 0x1000,
                            0x2000, 0x4000, 0x8000, 0x10000 };

    static const U32 ML_base[MaxML+1] = {
                             3,  4,  5,    6,     7,     8,     9,    10,
                            11, 12, 13,   14,    15,    16,    17,    18,
                            19, 20, 21,   22,    23,    24,    25,    26,
                            27, 28, 29,   30,    31,    32,    33,    34,
                            35, 37, 39,   41,    43,    47,    51,    59,
                            67, 83, 99, 0x83, 0x103, 0x203, 0x403, 0x803,
                            0x1003, 0x2003, 0x4003, 0x8003, 0x10003 };

    static const U32 OF_base[MaxOff+1] = {
                     0,        1,       1,       5,     0xD,     0x1D,     0x3D,     0x7D,
                     0xFD,   0x1FD,   0x3FD,   0x7FD,   0xFFD,   0x1FFD,   0x3FFD,   0x7FFD,
                     0xFFFD, 0x1FFFD, 0x3FFFD, 0x7FFFD, 0xFFFFD, 0x1FFFFD, 0x3FFFFD, 0x7FFFFD,
                     0xFFFFFD, 0x1FFFFFD, 0x3FFFFFD, 0x7FFFFFD, 0xFFFFFFD };

    /* sequence */
    {   size_t offset;
        if (!ofCode)
            offset = 0;
        else {
            if (longOffsets) {
                int const extraBits = ofBits - MIN(ofBits, STREAM_ACCUMULATOR_MIN);
                offset = OF_base[ofCode] + (BIT_readBitsFast(&seqState->DStream, ofBits - extraBits) << extraBits);
                if (MEM_32bits() || extraBits) BIT_reloadDStream(&seqState->DStream);
                if (extraBits) offset += BIT_readBitsFast(&seqState->DStream, extraBits);
            } else {
                offset = OF_base[ofCode] + BIT_readBitsFast(&seqState->DStream, ofBits);   /* <=  (ZSTD1_WINDOWLOG_MAX-1) bits */
                if (MEM_32bits()) BIT_reloadDStream(&seqState->DStream);
            }
        }

        if (ofCode <= 1) {
            offset += (llCode==0);
            if (offset) {
                size_t temp = (offset==3) ? seqState->prevOffset[0] - 1 : seqState->prevOffset[offset];
                temp += !temp;   /* 0 is not valid; input is corrupted; force offset to 1 */
                if (offset != 1) seqState->prevOffset[2] = seqState->prevOffset[1];
                seqState->prevOffset[1] = seqState->prevOffset[0];
                seqState->prevOffset[0] = offset = temp;
            } else {
                offset = seqState->prevOffset[0];
            }
        } else {
            seqState->prevOffset[2] = seqState->prevOffset[1];
            seqState->prevOffset[1] = seqState->prevOffset[0];
            seqState->prevOffset[0] = offset;
        }
        seq.offset = offset;
    }

    seq.matchLength = ML_base[mlCode] + ((mlCode>31) ? BIT_readBitsFast(&seqState->DStream, mlBits) : 0);  /* <=  16 bits */
    if (MEM_32bits() && (mlBits+llBits>24)) BIT_reloadDStream(&seqState->DStream);

    seq.litLength = LL_base[llCode] + ((llCode>15) ? BIT_readBitsFast(&seqState->DStream, llBits) : 0);    /* <=  16 bits */
    if (MEM_32bits() ||
       (totalBits > 64 - 7 - (LLFSE1Log+MLFSE1Log+OffFSE1Log)) ) BIT_reloadDStream(&seqState->DStream);

    {   size_t const pos = seqState->pos + seq.litLength;
        seq.match = seqState->base + pos - seq.offset;    /* single memory segment */
        if (seq.offset > pos) seq.match += seqState->gotoDict;   /* separate memory segment */
        seqState->pos = pos + seq.matchLength;
    }

    /* ANS state update */
    FSE1_updateState(&seqState->stateLL, &seqState->DStream);    /* <=  9 bits */
    FSE1_updateState(&seqState->stateML, &seqState->DStream);    /* <=  9 bits */
    if (MEM_32bits()) BIT_reloadDStream(&seqState->DStream);    /* <= 18 bits */
    FSE1_updateState(&seqState->stateOffb, &seqState->DStream);  /* <=  8 bits */

    return seq;
}

static seq_t ZSTD1_decodeSequenceLong(seqState_t* seqState, unsigned const windowSize) {
    if (ZSTD1_highbit32(windowSize) > STREAM_ACCUMULATOR_MIN) {
        return ZSTD1_decodeSequenceLong_generic(seqState, 1);
    } else {
        return ZSTD1_decodeSequenceLong_generic(seqState, 0);
    }
}

FORCE_INLINE
size_t ZSTD1_execSequenceLong(BYTE* op,
                                BYTE* const oend, seq_t sequence,
                                const BYTE** litPtr, const BYTE* const litLimit,
                                const BYTE* const base, const BYTE* const vBase, const BYTE* const dictEnd)
{
    BYTE* const oLitEnd = op + sequence.litLength;
    size_t const sequenceLength = sequence.litLength + sequence.matchLength;
    BYTE* const oMatchEnd = op + sequenceLength;   /* risk : address space overflow (32-bits) */
    BYTE* const oend_w = oend - WILDCOPY_OVERLENGTH;
    const BYTE* const iLitEnd = *litPtr + sequence.litLength;
    const BYTE* match = sequence.match;

    /* check */
#if 1
    if (oMatchEnd>oend) return ERROR(dstSize_tooSmall); /* last match must start at a minimum distance of WILDCOPY_OVERLENGTH from oend */
    if (iLitEnd > litLimit) return ERROR(corruption_detected);   /* over-read beyond lit buffer */
    if (oLitEnd>oend_w) return ZSTD1_execSequenceLast7(op, oend, sequence, litPtr, litLimit, base, vBase, dictEnd);
#endif

    /* copy Literals */
    ZSTD1_copy8(op, *litPtr);
    if (sequence.litLength > 8)
        ZSTD1_wildcopy(op+8, (*litPtr)+8, sequence.litLength - 8);   /* note : since oLitEnd <= oend-WILDCOPY_OVERLENGTH, no risk of overwrite beyond oend */
    op = oLitEnd;
    *litPtr = iLitEnd;   /* update for next sequence */

    /* copy Match */
#if 1
    if (sequence.offset > (size_t)(oLitEnd - base)) {
        /* offset beyond prefix */
        if (sequence.offset > (size_t)(oLitEnd - vBase)) return ERROR(corruption_detected);
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
            if (op > oend_w || sequence.matchLength < MINMATCH) {
              U32 i;
              for (i = 0; i < sequence.matchLength; ++i) op[i] = match[i];
              return sequenceLength;
            }
    }   }
    /* Requirement: op <= oend_w && sequence.matchLength >= MINMATCH */
#endif

    /* match within prefix */
    if (sequence.offset < 8) {
        /* close range match, overlap */
        static const U32 dec32table[] = { 0, 1, 2, 1, 4, 4, 4, 4 };   /* added */
        static const int dec64table[] = { 8, 8, 8, 7, 8, 9,10,11 };   /* subtracted */
        int const sub2 = dec64table[sequence.offset];
        op[0] = match[0];
        op[1] = match[1];
        op[2] = match[2];
        op[3] = match[3];
        match += dec32table[sequence.offset];
        ZSTD1_copy4(op+4, match);
        match -= sub2;
    } else {
        ZSTD1_copy8(op, match);
    }
    op += 8; match += 8;

    if (oMatchEnd > oend-(16-MINMATCH)) {
        if (op < oend_w) {
            ZSTD1_wildcopy(op, match, oend_w - op);
            match += oend_w - op;
            op = oend_w;
        }
        while (op < oMatchEnd) *op++ = *match++;
    } else {
        ZSTD1_wildcopy(op, match, (ptrdiff_t)sequence.matchLength-8);   /* works even if matchLength < 8 */
    }
    return sequenceLength;
}

static size_t ZSTD1_decompressSequencesLong(
                               ZSTD1_DCtx* dctx,
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
    const BYTE* const base = (const BYTE*) (dctx->base);
    const BYTE* const vBase = (const BYTE*) (dctx->vBase);
    const BYTE* const dictEnd = (const BYTE*) (dctx->dictEnd);
    unsigned const windowSize32 = (unsigned)dctx->fParams.windowSize;
    int nbSeq;

    /* Build Decoding Tables */
    {   size_t const seqHSize = ZSTD1_decodeSeqHeaders(dctx, &nbSeq, ip, seqSize);
        if (ZSTD1_isError(seqHSize)) return seqHSize;
        ip += seqHSize;
    }

    /* Regen sequences */
    if (nbSeq) {
#define STORED_SEQS 4
#define STOSEQ_MASK (STORED_SEQS-1)
#define ADVANCED_SEQS 4
        seq_t sequences[STORED_SEQS];
        int const seqAdvance = MIN(nbSeq, ADVANCED_SEQS);
        seqState_t seqState;
        int seqNb;
        dctx->fseEntropy = 1;
        { U32 i; for (i=0; i<ZSTD1_REP_NUM; i++) seqState.prevOffset[i] = dctx->entropy.rep[i]; }
        seqState.base = base;
        seqState.pos = (size_t)(op-base);
        seqState.gotoDict = (uPtrDiff)dictEnd - (uPtrDiff)base; /* cast to avoid undefined behaviour */
        CHECK_E(BIT_initDStream(&seqState.DStream, ip, iend-ip), corruption_detected);
        FSE1_initDState(&seqState.stateLL, &seqState.DStream, dctx->LLTptr);
        FSE1_initDState(&seqState.stateOffb, &seqState.DStream, dctx->OFTptr);
        FSE1_initDState(&seqState.stateML, &seqState.DStream, dctx->MLTptr);

        /* prepare in advance */
        for (seqNb=0; (BIT_reloadDStream(&seqState.DStream) <= BIT_DStream_completed) && seqNb<seqAdvance; seqNb++) {
            sequences[seqNb] = ZSTD1_decodeSequenceLong(&seqState, windowSize32);
        }
        if (seqNb<seqAdvance) return ERROR(corruption_detected);

        /* decode and decompress */
        for ( ; (BIT_reloadDStream(&(seqState.DStream)) <= BIT_DStream_completed) && seqNb<nbSeq ; seqNb++) {
            seq_t const sequence = ZSTD1_decodeSequenceLong(&seqState, windowSize32);
            size_t const oneSeqSize = ZSTD1_execSequenceLong(op, oend, sequences[(seqNb-ADVANCED_SEQS) & STOSEQ_MASK], &litPtr, litEnd, base, vBase, dictEnd);
            if (ZSTD1_isError(oneSeqSize)) return oneSeqSize;
            ZSTD1_PREFETCH(sequence.match);
            sequences[seqNb&STOSEQ_MASK] = sequence;
            op += oneSeqSize;
        }
        if (seqNb<nbSeq) return ERROR(corruption_detected);

        /* finish queue */
        seqNb -= seqAdvance;
        for ( ; seqNb<nbSeq ; seqNb++) {
            size_t const oneSeqSize = ZSTD1_execSequenceLong(op, oend, sequences[seqNb&STOSEQ_MASK], &litPtr, litEnd, base, vBase, dictEnd);
            if (ZSTD1_isError(oneSeqSize)) return oneSeqSize;
            op += oneSeqSize;
        }

        /* save reps for next block */
        { U32 i; for (i=0; i<ZSTD1_REP_NUM; i++) dctx->entropy.rep[i] = (U32)(seqState.prevOffset[i]); }
    }

    /* last literal segment */
    {   size_t const lastLLSize = litEnd - litPtr;
        if (lastLLSize > (size_t)(oend-op)) return ERROR(dstSize_tooSmall);
        memcpy(op, litPtr, lastLLSize);
        op += lastLLSize;
    }

    return op-ostart;
}


static size_t ZSTD1_decompressBlock_internal(ZSTD1_DCtx* dctx,
                            void* dst, size_t dstCapacity,
                      const void* src, size_t srcSize)
{   /* blockType == blockCompressed */
    const BYTE* ip = (const BYTE*)src;
    DEBUGLOG(5, "ZSTD1_decompressBlock_internal");

    if (srcSize >= ZSTD1_BLOCKSIZE_MAX) return ERROR(srcSize_wrong);

    /* Decode literals section */
    {   size_t const litCSize = ZSTD1_decodeLiteralsBlock(dctx, src, srcSize);
        DEBUGLOG(5, "ZSTD1_decodeLiteralsBlock : %u", (U32)litCSize);
        if (ZSTD1_isError(litCSize)) return litCSize;
        ip += litCSize;
        srcSize -= litCSize;
    }
    if (sizeof(size_t) > 4)  /* do not enable prefetching on 32-bits x86, as it's performance detrimental */
                             /* likely because of register pressure */
                             /* if that's the correct cause, then 32-bits ARM should be affected differently */
                             /* it would be good to test this on ARM real hardware, to see if prefetch version improves speed */
        if (dctx->fParams.windowSize > (1<<23))
            return ZSTD1_decompressSequencesLong(dctx, dst, dstCapacity, ip, srcSize);
    return ZSTD1_decompressSequences(dctx, dst, dstCapacity, ip, srcSize);
}


static void ZSTD1_checkContinuity(ZSTD1_DCtx* dctx, const void* dst)
{
    if (dst != dctx->previousDstEnd) {   /* not contiguous */
        dctx->dictEnd = dctx->previousDstEnd;
        dctx->vBase = (const char*)dst - ((const char*)(dctx->previousDstEnd) - (const char*)(dctx->base));
        dctx->base = dst;
        dctx->previousDstEnd = dst;
    }
}

size_t ZSTD1_decompressBlock(ZSTD1_DCtx* dctx,
                            void* dst, size_t dstCapacity,
                      const void* src, size_t srcSize)
{
    size_t dSize;
    ZSTD1_checkContinuity(dctx, dst);
    dSize = ZSTD1_decompressBlock_internal(dctx, dst, dstCapacity, src, srcSize);
    dctx->previousDstEnd = (char*)dst + dSize;
    return dSize;
}


/** ZSTD1_insertBlock() :
    insert `src` block into `dctx` history. Useful to track uncompressed blocks. */
ZSTD1LIB_API size_t ZSTD1_insertBlock(ZSTD1_DCtx* dctx, const void* blockStart, size_t blockSize)
{
    ZSTD1_checkContinuity(dctx, blockStart);
    dctx->previousDstEnd = (const char*)blockStart + blockSize;
    return blockSize;
}


size_t ZSTD1_generateNxBytes(void* dst, size_t dstCapacity, BYTE byte, size_t length)
{
    if (length > dstCapacity) return ERROR(dstSize_tooSmall);
    memset(dst, byte, length);
    return length;
}

/** ZSTD1_findFrameCompressedSize() :
 *  compatible with legacy mode
 *  `src` must point to the start of a ZSTD1 frame, ZSTD1 legacy frame, or skippable frame
 *  `srcSize` must be at least as large as the frame contained
 *  @return : the compressed size of the frame starting at `src` */
size_t ZSTD1_findFrameCompressedSize(const void *src, size_t srcSize)
{
#if defined(ZSTD1_LEGACY_SUPPORT) && (ZSTD1_LEGACY_SUPPORT >= 1)
    if (ZSTD1_isLegacy(src, srcSize)) return ZSTD1_findFrameCompressedSizeLegacy(src, srcSize);
#endif
    if (srcSize >= ZSTD1_skippableHeaderSize &&
            (MEM_readLE32(src) & 0xFFFFFFF0U) == ZSTD1_MAGIC_SKIPPABLE_START) {
        return ZSTD1_skippableHeaderSize + MEM_readLE32((const BYTE*)src + 4);
    } else {
        const BYTE* ip = (const BYTE*)src;
        const BYTE* const ipstart = ip;
        size_t remainingSize = srcSize;
        ZSTD1_frameHeader fParams;

        size_t const headerSize = ZSTD1_frameHeaderSize(ip, remainingSize);
        if (ZSTD1_isError(headerSize)) return headerSize;

        /* Frame Header */
        {   size_t const ret = ZSTD1_getFrameHeader(&fParams, ip, remainingSize);
            if (ZSTD1_isError(ret)) return ret;
            if (ret > 0) return ERROR(srcSize_wrong);
        }

        ip += headerSize;
        remainingSize -= headerSize;

        /* Loop on each block */
        while (1) {
            blockProperties_t blockProperties;
            size_t const cBlockSize = ZSTD1_getcBlockSize(ip, remainingSize, &blockProperties);
            if (ZSTD1_isError(cBlockSize)) return cBlockSize;

            if (ZSTD1_blockHeaderSize + cBlockSize > remainingSize) return ERROR(srcSize_wrong);

            ip += ZSTD1_blockHeaderSize + cBlockSize;
            remainingSize -= ZSTD1_blockHeaderSize + cBlockSize;

            if (blockProperties.lastBlock) break;
        }

        if (fParams.checksumFlag) {   /* Frame content checksum */
            if (remainingSize < 4) return ERROR(srcSize_wrong);
            ip += 4;
            remainingSize -= 4;
        }

        return ip - ipstart;
    }
}

/*! ZSTD1_decompressFrame() :
*   @dctx must be properly initialized */
static size_t ZSTD1_decompressFrame(ZSTD1_DCtx* dctx,
                                 void* dst, size_t dstCapacity,
                                 const void** srcPtr, size_t *srcSizePtr)
{
    const BYTE* ip = (const BYTE*)(*srcPtr);
    BYTE* const ostart = (BYTE* const)dst;
    BYTE* const oend = ostart + dstCapacity;
    BYTE* op = ostart;
    size_t remainingSize = *srcSizePtr;

    /* check */
    if (remainingSize < ZSTD1_frameHeaderSize_min+ZSTD1_blockHeaderSize) return ERROR(srcSize_wrong);

    /* Frame Header */
    {   size_t const frameHeaderSize = ZSTD1_frameHeaderSize(ip, ZSTD1_frameHeaderSize_prefix);
        if (ZSTD1_isError(frameHeaderSize)) return frameHeaderSize;
        if (remainingSize < frameHeaderSize+ZSTD1_blockHeaderSize) return ERROR(srcSize_wrong);
        CHECK_F(ZSTD1_decodeFrameHeader(dctx, ip, frameHeaderSize));
        ip += frameHeaderSize; remainingSize -= frameHeaderSize;
    }

    /* Loop on each block */
    while (1) {
        size_t decodedSize;
        blockProperties_t blockProperties;
        size_t const cBlockSize = ZSTD1_getcBlockSize(ip, remainingSize, &blockProperties);
        if (ZSTD1_isError(cBlockSize)) return cBlockSize;

        ip += ZSTD1_blockHeaderSize;
        remainingSize -= ZSTD1_blockHeaderSize;
        if (cBlockSize > remainingSize) return ERROR(srcSize_wrong);

        switch(blockProperties.blockType)
        {
        case bt_compressed:
            decodedSize = ZSTD1_decompressBlock_internal(dctx, op, oend-op, ip, cBlockSize);
            break;
        case bt_raw :
            decodedSize = ZSTD1_copyRawBlock(op, oend-op, ip, cBlockSize);
            break;
        case bt_rle :
            decodedSize = ZSTD1_generateNxBytes(op, oend-op, *ip, blockProperties.origSize);
            break;
        case bt_reserved :
        default:
            return ERROR(corruption_detected);
        }

        if (ZSTD1_isError(decodedSize)) return decodedSize;
        if (dctx->fParams.checksumFlag) XXH64_update(&dctx->xxhState, op, decodedSize);
        op += decodedSize;
        ip += cBlockSize;
        remainingSize -= cBlockSize;
        if (blockProperties.lastBlock) break;
    }

    if (dctx->fParams.checksumFlag) {   /* Frame content checksum verification */
        U32 const checkCalc = (U32)XXH64_digest(&dctx->xxhState);
        U32 checkRead;
        if (remainingSize<4) return ERROR(checksum_wrong);
        checkRead = MEM_readLE32(ip);
        if (checkRead != checkCalc) return ERROR(checksum_wrong);
        ip += 4;
        remainingSize -= 4;
    }

    /* Allow caller to get size read */
    *srcPtr = ip;
    *srcSizePtr = remainingSize;
    return op-ostart;
}

static const void* ZSTD1_DDictDictContent(const ZSTD1_DDict* ddict);
static size_t ZSTD1_DDictDictSize(const ZSTD1_DDict* ddict);

static size_t ZSTD1_decompressMultiFrame(ZSTD1_DCtx* dctx,
                                        void* dst, size_t dstCapacity,
                                  const void* src, size_t srcSize,
                                  const void *dict, size_t dictSize,
                                  const ZSTD1_DDict* ddict)
{
    void* const dststart = dst;

    if (ddict) {
        if (dict) {
            /* programmer error, these two cases should be mutually exclusive */
            return ERROR(GENERIC);
        }

        dict = ZSTD1_DDictDictContent(ddict);
        dictSize = ZSTD1_DDictDictSize(ddict);
    }

    while (srcSize >= ZSTD1_frameHeaderSize_prefix) {
        U32 magicNumber;

#if defined(ZSTD1_LEGACY_SUPPORT) && (ZSTD1_LEGACY_SUPPORT >= 1)
        if (ZSTD1_isLegacy(src, srcSize)) {
            size_t decodedSize;
            size_t const frameSize = ZSTD1_findFrameCompressedSizeLegacy(src, srcSize);
            if (ZSTD1_isError(frameSize)) return frameSize;
            /* legacy support is incompatible with static dctx */
            if (dctx->staticSize) return ERROR(memory_allocation);

            decodedSize = ZSTD1_decompressLegacy(dst, dstCapacity, src, frameSize, dict, dictSize);

            dst = (BYTE*)dst + decodedSize;
            dstCapacity -= decodedSize;

            src = (const BYTE*)src + frameSize;
            srcSize -= frameSize;

            continue;
        }
#endif

        magicNumber = MEM_readLE32(src);
        if (magicNumber != ZSTD1_MAGICNUMBER) {
            if ((magicNumber & 0xFFFFFFF0U) == ZSTD1_MAGIC_SKIPPABLE_START) {
                size_t skippableSize;
                if (srcSize < ZSTD1_skippableHeaderSize)
                    return ERROR(srcSize_wrong);
                skippableSize = MEM_readLE32((const BYTE *)src + 4) +
                                ZSTD1_skippableHeaderSize;
                if (srcSize < skippableSize) {
                    return ERROR(srcSize_wrong);
                }

                src = (const BYTE *)src + skippableSize;
                srcSize -= skippableSize;
                continue;
            } else {
                return ERROR(prefix_unknown);
            }
        }

        if (ddict) {
            /* we were called from ZSTD1_decompress_usingDDict */
            CHECK_F(ZSTD1_decompressBegin_usingDDict(dctx, ddict));
        } else {
            /* this will initialize correctly with no dict if dict == NULL, so
             * use this in all cases but ddict */
            CHECK_F(ZSTD1_decompressBegin_usingDict(dctx, dict, dictSize));
        }
        ZSTD1_checkContinuity(dctx, dst);

        {   const size_t res = ZSTD1_decompressFrame(dctx, dst, dstCapacity,
                                                    &src, &srcSize);
            if (ZSTD1_isError(res)) return res;
            /* don't need to bounds check this, ZSTD1_decompressFrame will have
             * already */
            dst = (BYTE*)dst + res;
            dstCapacity -= res;
        }
    }

    if (srcSize) return ERROR(srcSize_wrong); /* input not entirely consumed */

    return (BYTE*)dst - (BYTE*)dststart;
}

size_t ZSTD1_decompress_usingDict(ZSTD1_DCtx* dctx,
                                 void* dst, size_t dstCapacity,
                           const void* src, size_t srcSize,
                           const void* dict, size_t dictSize)
{
    return ZSTD1_decompressMultiFrame(dctx, dst, dstCapacity, src, srcSize, dict, dictSize, NULL);
}


size_t ZSTD1_decompressDCtx(ZSTD1_DCtx* dctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
    return ZSTD1_decompress_usingDict(dctx, dst, dstCapacity, src, srcSize, NULL, 0);
}


size_t ZSTD1_decompress(void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
#if defined(ZSTD1_HEAPMODE) && (ZSTD1_HEAPMODE>=1)
    size_t regenSize;
    ZSTD1_DCtx* const dctx = ZSTD1_createDCtx();
    if (dctx==NULL) return ERROR(memory_allocation);
    regenSize = ZSTD1_decompressDCtx(dctx, dst, dstCapacity, src, srcSize);
    ZSTD1_freeDCtx(dctx);
    return regenSize;
#else   /* stack mode */
    ZSTD1_DCtx dctx;
    return ZSTD1_decompressDCtx(&dctx, dst, dstCapacity, src, srcSize);
#endif
}


/*-**************************************
*   Advanced Streaming Decompression API
*   Bufferless and synchronous
****************************************/
size_t ZSTD1_nextSrcSizeToDecompress(ZSTD1_DCtx* dctx) { return dctx->expected; }

ZSTD1_nextInputType_e ZSTD1_nextInputType(ZSTD1_DCtx* dctx) {
    switch(dctx->stage)
    {
    default:   /* should not happen */
        assert(0);
    case ZSTD1ds_getFrameHeaderSize:
    case ZSTD1ds_decodeFrameHeader:
        return ZSTD1nit_frameHeader;
    case ZSTD1ds_decodeBlockHeader:
        return ZSTD1nit_blockHeader;
    case ZSTD1ds_decompressBlock:
        return ZSTD1nit_block;
    case ZSTD1ds_decompressLastBlock:
        return ZSTD1nit_lastBlock;
    case ZSTD1ds_checkChecksum:
        return ZSTD1nit_checksum;
    case ZSTD1ds_decodeSkippableHeader:
    case ZSTD1ds_skipFrame:
        return ZSTD1nit_skippableFrame;
    }
}

static int ZSTD1_isSkipFrame(ZSTD1_DCtx* dctx) { return dctx->stage == ZSTD1ds_skipFrame; }

/** ZSTD1_decompressContinue() :
 *  srcSize : must be the exact nb of bytes expected (see ZSTD1_nextSrcSizeToDecompress())
 *  @return : nb of bytes generated into `dst` (necessarily <= `dstCapacity)
 *            or an error code, which can be tested using ZSTD1_isError() */
size_t ZSTD1_decompressContinue(ZSTD1_DCtx* dctx, void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
    DEBUGLOG(5, "ZSTD1_decompressContinue");
    /* Sanity check */
    if (srcSize != dctx->expected) return ERROR(srcSize_wrong);   /* unauthorized */
    if (dstCapacity) ZSTD1_checkContinuity(dctx, dst);

    switch (dctx->stage)
    {
    case ZSTD1ds_getFrameHeaderSize :
        if (srcSize != ZSTD1_frameHeaderSize_prefix) return ERROR(srcSize_wrong);      /* unauthorized */
        assert(src != NULL);
        if ((MEM_readLE32(src) & 0xFFFFFFF0U) == ZSTD1_MAGIC_SKIPPABLE_START) {        /* skippable frame */
            memcpy(dctx->headerBuffer, src, ZSTD1_frameHeaderSize_prefix);
            dctx->expected = ZSTD1_skippableHeaderSize - ZSTD1_frameHeaderSize_prefix;  /* magic number + skippable frame length */
            dctx->stage = ZSTD1ds_decodeSkippableHeader;
            return 0;
        }
        dctx->headerSize = ZSTD1_frameHeaderSize(src, ZSTD1_frameHeaderSize_prefix);
        if (ZSTD1_isError(dctx->headerSize)) return dctx->headerSize;
        memcpy(dctx->headerBuffer, src, ZSTD1_frameHeaderSize_prefix);
        if (dctx->headerSize > ZSTD1_frameHeaderSize_prefix) {
            dctx->expected = dctx->headerSize - ZSTD1_frameHeaderSize_prefix;
            dctx->stage = ZSTD1ds_decodeFrameHeader;
            return 0;
        }
        dctx->expected = 0;   /* not necessary to copy more */

    case ZSTD1ds_decodeFrameHeader:
        assert(src != NULL);
        memcpy(dctx->headerBuffer + ZSTD1_frameHeaderSize_prefix, src, dctx->expected);
        CHECK_F(ZSTD1_decodeFrameHeader(dctx, dctx->headerBuffer, dctx->headerSize));
        dctx->expected = ZSTD1_blockHeaderSize;
        dctx->stage = ZSTD1ds_decodeBlockHeader;
        return 0;

    case ZSTD1ds_decodeBlockHeader:
        {   blockProperties_t bp;
            size_t const cBlockSize = ZSTD1_getcBlockSize(src, ZSTD1_blockHeaderSize, &bp);
            if (ZSTD1_isError(cBlockSize)) return cBlockSize;
            dctx->expected = cBlockSize;
            dctx->bType = bp.blockType;
            dctx->rleSize = bp.origSize;
            if (cBlockSize) {
                dctx->stage = bp.lastBlock ? ZSTD1ds_decompressLastBlock : ZSTD1ds_decompressBlock;
                return 0;
            }
            /* empty block */
            if (bp.lastBlock) {
                if (dctx->fParams.checksumFlag) {
                    dctx->expected = 4;
                    dctx->stage = ZSTD1ds_checkChecksum;
                } else {
                    dctx->expected = 0; /* end of frame */
                    dctx->stage = ZSTD1ds_getFrameHeaderSize;
                }
            } else {
                dctx->expected = ZSTD1_blockHeaderSize;  /* jump to next header */
                dctx->stage = ZSTD1ds_decodeBlockHeader;
            }
            return 0;
        }
    case ZSTD1ds_decompressLastBlock:
    case ZSTD1ds_decompressBlock:
        DEBUGLOG(5, "case ZSTD1ds_decompressBlock");
        {   size_t rSize;
            switch(dctx->bType)
            {
            case bt_compressed:
                DEBUGLOG(5, "case bt_compressed");
                rSize = ZSTD1_decompressBlock_internal(dctx, dst, dstCapacity, src, srcSize);
                break;
            case bt_raw :
                rSize = ZSTD1_copyRawBlock(dst, dstCapacity, src, srcSize);
                break;
            case bt_rle :
                rSize = ZSTD1_setRleBlock(dst, dstCapacity, src, srcSize, dctx->rleSize);
                break;
            case bt_reserved :   /* should never happen */
            default:
                return ERROR(corruption_detected);
            }
            if (ZSTD1_isError(rSize)) return rSize;
            if (dctx->fParams.checksumFlag) XXH64_update(&dctx->xxhState, dst, rSize);

            if (dctx->stage == ZSTD1ds_decompressLastBlock) {   /* end of frame */
                if (dctx->fParams.checksumFlag) {  /* another round for frame checksum */
                    dctx->expected = 4;
                    dctx->stage = ZSTD1ds_checkChecksum;
                } else {
                    dctx->expected = 0;   /* ends here */
                    dctx->stage = ZSTD1ds_getFrameHeaderSize;
                }
            } else {
                dctx->stage = ZSTD1ds_decodeBlockHeader;
                dctx->expected = ZSTD1_blockHeaderSize;
                dctx->previousDstEnd = (char*)dst + rSize;
            }
            return rSize;
        }
    case ZSTD1ds_checkChecksum:
        {   U32 const h32 = (U32)XXH64_digest(&dctx->xxhState);
            U32 const check32 = MEM_readLE32(src);   /* srcSize == 4, guaranteed by dctx->expected */
            if (check32 != h32) return ERROR(checksum_wrong);
            dctx->expected = 0;
            dctx->stage = ZSTD1ds_getFrameHeaderSize;
            return 0;
        }
    case ZSTD1ds_decodeSkippableHeader:
        {   assert(src != NULL);
            memcpy(dctx->headerBuffer + ZSTD1_frameHeaderSize_prefix, src, dctx->expected);
            dctx->expected = MEM_readLE32(dctx->headerBuffer + 4);
            dctx->stage = ZSTD1ds_skipFrame;
            return 0;
        }
    case ZSTD1ds_skipFrame:
        {   dctx->expected = 0;
            dctx->stage = ZSTD1ds_getFrameHeaderSize;
            return 0;
        }
    default:
        return ERROR(GENERIC);   /* impossible */
    }
}


static size_t ZSTD1_refDictContent(ZSTD1_DCtx* dctx, const void* dict, size_t dictSize)
{
    dctx->dictEnd = dctx->previousDstEnd;
    dctx->vBase = (const char*)dict - ((const char*)(dctx->previousDstEnd) - (const char*)(dctx->base));
    dctx->base = dict;
    dctx->previousDstEnd = (const char*)dict + dictSize;
    return 0;
}

/* ZSTD1_loadEntropy() :
 * dict : must point at beginning of a valid zstd dictionary
 * @return : size of entropy tables read */
static size_t ZSTD1_loadEntropy(ZSTD1_entropyTables_t* entropy, const void* const dict, size_t const dictSize)
{
    const BYTE* dictPtr = (const BYTE*)dict;
    const BYTE* const dictEnd = dictPtr + dictSize;

    if (dictSize <= 8) return ERROR(dictionary_corrupted);
    dictPtr += 8;   /* skip header = magic + dictID */


    {   size_t const hSize = HUF1_readDTableX4_wksp(
            entropy->hufTable, dictPtr, dictEnd - dictPtr,
            entropy->workspace, sizeof(entropy->workspace));
        if (HUF1_isError(hSize)) return ERROR(dictionary_corrupted);
        dictPtr += hSize;
    }

    {   short offcodeNCount[MaxOff+1];
        U32 offcodeMaxValue = MaxOff, offcodeLog;
        size_t const offcodeHeaderSize = FSE1_readNCount(offcodeNCount, &offcodeMaxValue, &offcodeLog, dictPtr, dictEnd-dictPtr);
        if (FSE1_isError(offcodeHeaderSize)) return ERROR(dictionary_corrupted);
        if (offcodeLog > OffFSE1Log) return ERROR(dictionary_corrupted);
        CHECK_E(FSE1_buildDTable(entropy->OFTable, offcodeNCount, offcodeMaxValue, offcodeLog), dictionary_corrupted);
        dictPtr += offcodeHeaderSize;
    }

    {   short matchlengthNCount[MaxML+1];
        unsigned matchlengthMaxValue = MaxML, matchlengthLog;
        size_t const matchlengthHeaderSize = FSE1_readNCount(matchlengthNCount, &matchlengthMaxValue, &matchlengthLog, dictPtr, dictEnd-dictPtr);
        if (FSE1_isError(matchlengthHeaderSize)) return ERROR(dictionary_corrupted);
        if (matchlengthLog > MLFSE1Log) return ERROR(dictionary_corrupted);
        CHECK_E(FSE1_buildDTable(entropy->MLTable, matchlengthNCount, matchlengthMaxValue, matchlengthLog), dictionary_corrupted);
        dictPtr += matchlengthHeaderSize;
    }

    {   short litlengthNCount[MaxLL+1];
        unsigned litlengthMaxValue = MaxLL, litlengthLog;
        size_t const litlengthHeaderSize = FSE1_readNCount(litlengthNCount, &litlengthMaxValue, &litlengthLog, dictPtr, dictEnd-dictPtr);
        if (FSE1_isError(litlengthHeaderSize)) return ERROR(dictionary_corrupted);
        if (litlengthLog > LLFSE1Log) return ERROR(dictionary_corrupted);
        CHECK_E(FSE1_buildDTable(entropy->LLTable, litlengthNCount, litlengthMaxValue, litlengthLog), dictionary_corrupted);
        dictPtr += litlengthHeaderSize;
    }

    if (dictPtr+12 > dictEnd) return ERROR(dictionary_corrupted);
    {   int i;
        size_t const dictContentSize = (size_t)(dictEnd - (dictPtr+12));
        for (i=0; i<3; i++) {
            U32 const rep = MEM_readLE32(dictPtr); dictPtr += 4;
            if (rep==0 || rep >= dictContentSize) return ERROR(dictionary_corrupted);
            entropy->rep[i] = rep;
    }   }

    return dictPtr - (const BYTE*)dict;
}

static size_t ZSTD1_decompress_insertDictionary(ZSTD1_DCtx* dctx, const void* dict, size_t dictSize)
{
    if (dictSize < 8) return ZSTD1_refDictContent(dctx, dict, dictSize);
    {   U32 const magic = MEM_readLE32(dict);
        if (magic != ZSTD1_MAGIC_DICTIONARY) {
            return ZSTD1_refDictContent(dctx, dict, dictSize);   /* pure content mode */
    }   }
    dctx->dictID = MEM_readLE32((const char*)dict + 4);

    /* load entropy tables */
    {   size_t const eSize = ZSTD1_loadEntropy(&dctx->entropy, dict, dictSize);
        if (ZSTD1_isError(eSize)) return ERROR(dictionary_corrupted);
        dict = (const char*)dict + eSize;
        dictSize -= eSize;
    }
    dctx->litEntropy = dctx->fseEntropy = 1;

    /* reference dictionary content */
    return ZSTD1_refDictContent(dctx, dict, dictSize);
}

size_t ZSTD1_decompressBegin_usingDict(ZSTD1_DCtx* dctx, const void* dict, size_t dictSize)
{
    CHECK_F(ZSTD1_decompressBegin(dctx));
    if (dict && dictSize) CHECK_E(ZSTD1_decompress_insertDictionary(dctx, dict, dictSize), dictionary_corrupted);
    return 0;
}


/* ======   ZSTD1_DDict   ====== */

struct ZSTD1_DDict_s {
    void* dictBuffer;
    const void* dictContent;
    size_t dictSize;
    ZSTD1_entropyTables_t entropy;
    U32 dictID;
    U32 entropyPresent;
    ZSTD1_customMem cMem;
};  /* typedef'd to ZSTD1_DDict within "zstd.h" */

static const void* ZSTD1_DDictDictContent(const ZSTD1_DDict* ddict)
{
    return ddict->dictContent;
}

static size_t ZSTD1_DDictDictSize(const ZSTD1_DDict* ddict)
{
    return ddict->dictSize;
}

size_t ZSTD1_decompressBegin_usingDDict(ZSTD1_DCtx* dstDCtx, const ZSTD1_DDict* ddict)
{
    CHECK_F(ZSTD1_decompressBegin(dstDCtx));
    if (ddict) {   /* support begin on NULL */
        dstDCtx->dictID = ddict->dictID;
        dstDCtx->base = ddict->dictContent;
        dstDCtx->vBase = ddict->dictContent;
        dstDCtx->dictEnd = (const BYTE*)ddict->dictContent + ddict->dictSize;
        dstDCtx->previousDstEnd = dstDCtx->dictEnd;
        if (ddict->entropyPresent) {
            dstDCtx->litEntropy = 1;
            dstDCtx->fseEntropy = 1;
            dstDCtx->LLTptr = ddict->entropy.LLTable;
            dstDCtx->MLTptr = ddict->entropy.MLTable;
            dstDCtx->OFTptr = ddict->entropy.OFTable;
            dstDCtx->HUF1ptr = ddict->entropy.hufTable;
            dstDCtx->entropy.rep[0] = ddict->entropy.rep[0];
            dstDCtx->entropy.rep[1] = ddict->entropy.rep[1];
            dstDCtx->entropy.rep[2] = ddict->entropy.rep[2];
        } else {
            dstDCtx->litEntropy = 0;
            dstDCtx->fseEntropy = 0;
        }
    }
    return 0;
}

static size_t ZSTD1_loadEntropy_inDDict(ZSTD1_DDict* ddict)
{
    ddict->dictID = 0;
    ddict->entropyPresent = 0;
    if (ddict->dictSize < 8) return 0;
    {   U32 const magic = MEM_readLE32(ddict->dictContent);
        if (magic != ZSTD1_MAGIC_DICTIONARY) return 0;   /* pure content mode */
    }
    ddict->dictID = MEM_readLE32((const char*)ddict->dictContent + 4);

    /* load entropy tables */
    CHECK_E( ZSTD1_loadEntropy(&ddict->entropy, ddict->dictContent, ddict->dictSize), dictionary_corrupted );
    ddict->entropyPresent = 1;
    return 0;
}


static size_t ZSTD1_initDDict_internal(ZSTD1_DDict* ddict, const void* dict, size_t dictSize, unsigned byReference)
{
    if ((byReference) || (!dict) || (!dictSize)) {
        ddict->dictBuffer = NULL;
        ddict->dictContent = dict;
    } else {
        void* const internalBuffer = ZSTD1_malloc(dictSize, ddict->cMem);
        ddict->dictBuffer = internalBuffer;
        ddict->dictContent = internalBuffer;
        if (!internalBuffer) return ERROR(memory_allocation);
        memcpy(internalBuffer, dict, dictSize);
    }
    ddict->dictSize = dictSize;
    ddict->entropy.hufTable[0] = (HUF1_DTable)((HufLog)*0x1000001);  /* cover both little and big endian */

    /* parse dictionary content */
    CHECK_F( ZSTD1_loadEntropy_inDDict(ddict) );

    return 0;
}

ZSTD1_DDict* ZSTD1_createDDict_advanced(const void* dict, size_t dictSize, unsigned byReference, ZSTD1_customMem customMem)
{
    if (!customMem.customAlloc ^ !customMem.customFree) return NULL;

    {   ZSTD1_DDict* const ddict = (ZSTD1_DDict*) ZSTD1_malloc(sizeof(ZSTD1_DDict), customMem);
        if (!ddict) return NULL;
        ddict->cMem = customMem;

        if (ZSTD1_isError( ZSTD1_initDDict_internal(ddict, dict, dictSize, byReference) )) {
            ZSTD1_freeDDict(ddict);
            return NULL;
        }

        return ddict;
    }
}

/*! ZSTD1_createDDict() :
*   Create a digested dictionary, to start decompression without startup delay.
*   `dict` content is copied inside DDict.
*   Consequently, `dict` can be released after `ZSTD1_DDict` creation */
ZSTD1_DDict* ZSTD1_createDDict(const void* dict, size_t dictSize)
{
    ZSTD1_customMem const allocator = { NULL, NULL, NULL };
    return ZSTD1_createDDict_advanced(dict, dictSize, 0, allocator);
}

/*! ZSTD1_createDDict_byReference() :
 *  Create a digested dictionary, to start decompression without startup delay.
 *  Dictionary content is simply referenced, it will be accessed during decompression.
 *  Warning : dictBuffer must outlive DDict (DDict must be freed before dictBuffer) */
ZSTD1_DDict* ZSTD1_createDDict_byReference(const void* dictBuffer, size_t dictSize)
{
    ZSTD1_customMem const allocator = { NULL, NULL, NULL };
    return ZSTD1_createDDict_advanced(dictBuffer, dictSize, 1, allocator);
}


ZSTD1_DDict* ZSTD1_initStaticDDict(void* workspace, size_t workspaceSize,
                                 const void* dict, size_t dictSize,
                                 unsigned byReference)
{
    size_t const neededSpace = sizeof(ZSTD1_DDict) + (byReference ? 0 : dictSize);
    ZSTD1_DDict* const ddict = (ZSTD1_DDict*)workspace;
    assert(workspace != NULL);
    assert(dict != NULL);
    if ((size_t)workspace & 7) return NULL;  /* 8-aligned */
    if (workspaceSize < neededSpace) return NULL;
    if (!byReference) {
        memcpy(ddict+1, dict, dictSize);  /* local copy */
        dict = ddict+1;
    }
    if (ZSTD1_isError( ZSTD1_initDDict_internal(ddict, dict, dictSize, 1 /* byRef */) ))
        return NULL;
    return ddict;
}


size_t ZSTD1_freeDDict(ZSTD1_DDict* ddict)
{
    if (ddict==NULL) return 0;   /* support free on NULL */
    {   ZSTD1_customMem const cMem = ddict->cMem;
        ZSTD1_free(ddict->dictBuffer, cMem);
        ZSTD1_free(ddict, cMem);
        return 0;
    }
}

/*! ZSTD1_estimateDDictSize() :
 *  Estimate amount of memory that will be needed to create a dictionary for decompression.
 *  Note : dictionary created "byReference" are smaller */
size_t ZSTD1_estimateDDictSize(size_t dictSize, unsigned byReference)
{
    return sizeof(ZSTD1_DDict) + (byReference ? 0 : dictSize);
}

size_t ZSTD1_sizeof_DDict(const ZSTD1_DDict* ddict)
{
    if (ddict==NULL) return 0;   /* support sizeof on NULL */
    return sizeof(*ddict) + (ddict->dictBuffer ? ddict->dictSize : 0) ;
}

/*! ZSTD1_getDictID_fromDict() :
 *  Provides the dictID stored within dictionary.
 *  if @return == 0, the dictionary is not conformant with Zstandard specification.
 *  It can still be loaded, but as a content-only dictionary. */
unsigned ZSTD1_getDictID_fromDict(const void* dict, size_t dictSize)
{
    if (dictSize < 8) return 0;
    if (MEM_readLE32(dict) != ZSTD1_MAGIC_DICTIONARY) return 0;
    return MEM_readLE32((const char*)dict + 4);
}

/*! ZSTD1_getDictID_fromDDict() :
 *  Provides the dictID of the dictionary loaded into `ddict`.
 *  If @return == 0, the dictionary is not conformant to Zstandard specification, or empty.
 *  Non-conformant dictionaries can still be loaded, but as content-only dictionaries. */
unsigned ZSTD1_getDictID_fromDDict(const ZSTD1_DDict* ddict)
{
    if (ddict==NULL) return 0;
    return ZSTD1_getDictID_fromDict(ddict->dictContent, ddict->dictSize);
}

/*! ZSTD1_getDictID_fromFrame() :
 *  Provides the dictID required to decompresse frame stored within `src`.
 *  If @return == 0, the dictID could not be decoded.
 *  This could for one of the following reasons :
 *  - The frame does not require a dictionary (most common case).
 *  - The frame was built with dictID intentionally removed.
 *    Needed dictionary is a hidden information.
 *    Note : this use case also happens when using a non-conformant dictionary.
 *  - `srcSize` is too small, and as a result, frame header could not be decoded.
 *    Note : possible if `srcSize < ZSTD1_FRAMEHEADERSIZE_MAX`.
 *  - This is not a Zstandard frame.
 *  When identifying the exact failure cause, it's possible to use
 *  ZSTD1_getFrameHeader(), which will provide a more precise error code. */
unsigned ZSTD1_getDictID_fromFrame(const void* src, size_t srcSize)
{
    ZSTD1_frameHeader zfp = { 0 , 0 , 0 , 0 };
    size_t const hError = ZSTD1_getFrameHeader(&zfp, src, srcSize);
    if (ZSTD1_isError(hError)) return 0;
    return zfp.dictID;
}


/*! ZSTD1_decompress_usingDDict() :
*   Decompression using a pre-digested Dictionary
*   Use dictionary without significant overhead. */
size_t ZSTD1_decompress_usingDDict(ZSTD1_DCtx* dctx,
                                  void* dst, size_t dstCapacity,
                            const void* src, size_t srcSize,
                            const ZSTD1_DDict* ddict)
{
    /* pass content and size in case legacy frames are encountered */
    return ZSTD1_decompressMultiFrame(dctx, dst, dstCapacity, src, srcSize,
                                     NULL, 0,
                                     ddict);
}


/*=====================================
*   Streaming decompression
*====================================*/

ZSTD1_DStream* ZSTD1_createDStream(void)
{
    return ZSTD1_createDStream_advanced(ZSTD1_defaultCMem);
}

ZSTD1_DStream* ZSTD1_initStaticDStream(void *workspace, size_t workspaceSize)
{
    return ZSTD1_initStaticDCtx(workspace, workspaceSize);
}

ZSTD1_DStream* ZSTD1_createDStream_advanced(ZSTD1_customMem customMem)
{
    return ZSTD1_createDCtx_advanced(customMem);
}

size_t ZSTD1_freeDStream(ZSTD1_DStream* zds)
{
    return ZSTD1_freeDCtx(zds);
}


/* *** Initialization *** */

size_t ZSTD1_DStreamInSize(void)  { return ZSTD1_BLOCKSIZE_MAX + ZSTD1_blockHeaderSize; }
size_t ZSTD1_DStreamOutSize(void) { return ZSTD1_BLOCKSIZE_MAX; }

size_t ZSTD1_initDStream_usingDict(ZSTD1_DStream* zds, const void* dict, size_t dictSize)
{
    zds->streamStage = zdss_loadHeader;
    zds->lhSize = zds->inPos = zds->outStart = zds->outEnd = 0;
    ZSTD1_freeDDict(zds->ddictLocal);
    if (dict && dictSize >= 8) {
        zds->ddictLocal = ZSTD1_createDDict(dict, dictSize);
        if (zds->ddictLocal == NULL) return ERROR(memory_allocation);
    } else zds->ddictLocal = NULL;
    zds->ddict = zds->ddictLocal;
    zds->legacyVersion = 0;
    zds->hostageByte = 0;
    return ZSTD1_frameHeaderSize_prefix;
}

size_t ZSTD1_initDStream(ZSTD1_DStream* zds)
{
    return ZSTD1_initDStream_usingDict(zds, NULL, 0);
}

/* ZSTD1_initDStream_usingDDict() :
 * ddict will just be referenced, and must outlive decompression session */
size_t ZSTD1_initDStream_usingDDict(ZSTD1_DStream* zds, const ZSTD1_DDict* ddict)
{
    size_t const initResult = ZSTD1_initDStream(zds);
    zds->ddict = ddict;
    return initResult;
}

size_t ZSTD1_resetDStream(ZSTD1_DStream* zds)
{
    zds->streamStage = zdss_loadHeader;
    zds->lhSize = zds->inPos = zds->outStart = zds->outEnd = 0;
    zds->legacyVersion = 0;
    zds->hostageByte = 0;
    return ZSTD1_frameHeaderSize_prefix;
}

size_t ZSTD1_setDStreamParameter(ZSTD1_DStream* zds,
                                ZSTD1_DStreamParameter_e paramType, unsigned paramValue)
{
    switch(paramType)
    {
        default : return ERROR(parameter_unknown);
        case DStream_p_maxWindowSize : zds->maxWindowSize = paramValue ? paramValue : (U32)(-1); break;
    }
    return 0;
}


size_t ZSTD1_sizeof_DStream(const ZSTD1_DStream* zds)
{
    return ZSTD1_sizeof_DCtx(zds);
}

size_t ZSTD1_estimateDStreamSize(size_t windowSize)
{
    size_t const blockSize = MIN(windowSize, ZSTD1_BLOCKSIZE_MAX);
    size_t const inBuffSize = blockSize;  /* no block can be larger */
    size_t const outBuffSize = windowSize + blockSize + (WILDCOPY_OVERLENGTH * 2);
    return sizeof(ZSTD1_DStream) + ZSTD1_estimateDCtxSize() + inBuffSize + outBuffSize;
}

ZSTD1LIB_API size_t ZSTD1_estimateDStreamSize_fromFrame(const void* src, size_t srcSize)
{
    ZSTD1_frameHeader fh;
    size_t const err = ZSTD1_getFrameHeader(&fh, src, srcSize);
    if (ZSTD1_isError(err)) return err;
    if (err>0) return ERROR(srcSize_wrong);
    return ZSTD1_estimateDStreamSize(fh.windowSize);
}


/* *****   Decompression   ***** */

MEM_STATIC size_t ZSTD1_limitCopy(void* dst, size_t dstCapacity, const void* src, size_t srcSize)
{
    size_t const length = MIN(dstCapacity, srcSize);
    memcpy(dst, src, length);
    return length;
}


size_t ZSTD1_decompressStream(ZSTD1_DStream* zds, ZSTD1_outBuffer* output, ZSTD1_inBuffer* input)
{
    const char* const istart = (const char*)(input->src) + input->pos;
    const char* const iend = (const char*)(input->src) + input->size;
    const char* ip = istart;
    char* const ostart = (char*)(output->dst) + output->pos;
    char* const oend = (char*)(output->dst) + output->size;
    char* op = ostart;
    U32 someMoreWork = 1;

    DEBUGLOG(5, "ZSTD1_decompressStream");
    DEBUGLOG(5, "input size : %u", (U32)(input->size - input->pos));
#if defined(ZSTD1_LEGACY_SUPPORT) && (ZSTD1_LEGACY_SUPPORT>=1)
    if (zds->legacyVersion) {
        /* legacy support is incompatible with static dctx */
        if (zds->staticSize) return ERROR(memory_allocation);
        return ZSTD1_decompressLegacyStream(zds->legacyContext, zds->legacyVersion, output, input);
    }
#endif

    while (someMoreWork) {
        switch(zds->streamStage)
        {
        case zdss_init :
            ZSTD1_resetDStream(zds);   /* transparent reset on starting decoding a new frame */
            /* fall-through */

        case zdss_loadHeader :
            {   size_t const hSize = ZSTD1_getFrameHeader(&zds->fParams, zds->headerBuffer, zds->lhSize);
                if (ZSTD1_isError(hSize)) {
#if defined(ZSTD1_LEGACY_SUPPORT) && (ZSTD1_LEGACY_SUPPORT>=1)
                    U32 const legacyVersion = ZSTD1_isLegacy(istart, iend-istart);
                    if (legacyVersion) {
                        const void* const dict = zds->ddict ? zds->ddict->dictContent : NULL;
                        size_t const dictSize = zds->ddict ? zds->ddict->dictSize : 0;
                        /* legacy support is incompatible with static dctx */
                        if (zds->staticSize) return ERROR(memory_allocation);
                        CHECK_F(ZSTD1_initLegacyStream(&zds->legacyContext, zds->previousLegacyVersion, legacyVersion,
                                                       dict, dictSize));
                        zds->legacyVersion = zds->previousLegacyVersion = legacyVersion;
                        return ZSTD1_decompressLegacyStream(zds->legacyContext, zds->legacyVersion, output, input);
                    } else {
                        return hSize; /* error */
                    }
#else
                    return hSize;
#endif
                }
                if (hSize != 0) {   /* need more input */
                    size_t const toLoad = hSize - zds->lhSize;   /* if hSize!=0, hSize > zds->lhSize */
                    if (toLoad > (size_t)(iend-ip)) {   /* not enough input to load full header */
                        if (iend-ip > 0) {
                            memcpy(zds->headerBuffer + zds->lhSize, ip, iend-ip);
                            zds->lhSize += iend-ip;
                        }
                        input->pos = input->size;
                        return (MAX(ZSTD1_frameHeaderSize_min, hSize) - zds->lhSize) + ZSTD1_blockHeaderSize;   /* remaining header bytes + next block header */
                    }
                    assert(ip != NULL);
                    memcpy(zds->headerBuffer + zds->lhSize, ip, toLoad); zds->lhSize = hSize; ip += toLoad;
                    break;
            }   }

            /* check for single-pass mode opportunity */
            if (zds->fParams.frameContentSize && zds->fParams.windowSize /* skippable frame if == 0 */
                && (U64)(size_t)(oend-op) >= zds->fParams.frameContentSize) {
                size_t const cSize = ZSTD1_findFrameCompressedSize(istart, iend-istart);
                if (cSize <= (size_t)(iend-istart)) {
                    size_t const decompressedSize = ZSTD1_decompress_usingDDict(zds, op, oend-op, istart, cSize, zds->ddict);
                    if (ZSTD1_isError(decompressedSize)) return decompressedSize;
                    ip = istart + cSize;
                    op += decompressedSize;
                    zds->expected = 0;
                    zds->streamStage = zdss_init;
                    someMoreWork = 0;
                    break;
            }   }

            /* Consume header (see ZSTD1ds_decodeFrameHeader) */
            DEBUGLOG(4, "Consume header");
            CHECK_F(ZSTD1_decompressBegin_usingDDict(zds, zds->ddict));

            if ((MEM_readLE32(zds->headerBuffer) & 0xFFFFFFF0U) == ZSTD1_MAGIC_SKIPPABLE_START) {  /* skippable frame */
                zds->expected = MEM_readLE32(zds->headerBuffer + 4);
                zds->stage = ZSTD1ds_skipFrame;
            } else {
                CHECK_F(ZSTD1_decodeFrameHeader(zds, zds->headerBuffer, zds->lhSize));
                zds->expected = ZSTD1_blockHeaderSize;
                zds->stage = ZSTD1ds_decodeBlockHeader;
            }

            /* control buffer memory usage */
            DEBUGLOG(4, "Control max buffer memory usage");
            zds->fParams.windowSize = MAX(zds->fParams.windowSize, 1U << ZSTD1_WINDOWLOG_ABSOLUTEMIN);
            if (zds->fParams.windowSize > zds->maxWindowSize) return ERROR(frameParameter_windowTooLarge);

            /* Adapt buffer sizes to frame header instructions */
            {   size_t const blockSize = MIN(zds->fParams.windowSize, ZSTD1_BLOCKSIZE_MAX);
                size_t const neededOutSize = zds->fParams.windowSize + blockSize + WILDCOPY_OVERLENGTH * 2;
                zds->blockSize = blockSize;
                if ((zds->inBuffSize < blockSize) || (zds->outBuffSize < neededOutSize)) {
                    size_t const bufferSize = blockSize + neededOutSize;
                    DEBUGLOG(4, "inBuff  : from %u to %u",
                                (U32)zds->inBuffSize, (U32)blockSize);
                    DEBUGLOG(4, "outBuff : from %u to %u",
                                (U32)zds->outBuffSize, (U32)neededOutSize);
                    if (zds->staticSize) {  /* static DCtx */
                        DEBUGLOG(4, "staticSize : %u", (U32)zds->staticSize);
                        assert(zds->staticSize >= sizeof(ZSTD1_DCtx));  /* controlled at init */
                        if (bufferSize > zds->staticSize - sizeof(ZSTD1_DCtx))
                            return ERROR(memory_allocation);
                    } else {
                        ZSTD1_free(zds->inBuff, zds->customMem);
                        zds->inBuffSize = 0;
                        zds->outBuffSize = 0;
                        zds->inBuff = (char*)ZSTD1_malloc(bufferSize, zds->customMem);
                        if (zds->inBuff == NULL) return ERROR(memory_allocation);
                    }
                    zds->inBuffSize = blockSize;
                    zds->outBuff = zds->inBuff + zds->inBuffSize;
                    zds->outBuffSize = neededOutSize;
            }   }
            zds->streamStage = zdss_read;
            /* pass-through */

        case zdss_read:
            DEBUGLOG(5, "stage zdss_read");
            {   size_t const neededInSize = ZSTD1_nextSrcSizeToDecompress(zds);
                DEBUGLOG(5, "neededInSize = %u", (U32)neededInSize);
                if (neededInSize==0) {  /* end of frame */
                    zds->streamStage = zdss_init;
                    someMoreWork = 0;
                    break;
                }
                if ((size_t)(iend-ip) >= neededInSize) {  /* decode directly from src */
                    int const isSkipFrame = ZSTD1_isSkipFrame(zds);
                    size_t const decodedSize = ZSTD1_decompressContinue(zds,
                        zds->outBuff + zds->outStart, (isSkipFrame ? 0 : zds->outBuffSize - zds->outStart),
                        ip, neededInSize);
                    if (ZSTD1_isError(decodedSize)) return decodedSize;
                    ip += neededInSize;
                    if (!decodedSize && !isSkipFrame) break;   /* this was just a header */
                    zds->outEnd = zds->outStart + decodedSize;
                    zds->streamStage = zdss_flush;
                    break;
            }   }
            if (ip==iend) { someMoreWork = 0; break; }   /* no more input */
            zds->streamStage = zdss_load;
            /* pass-through */

        case zdss_load:
            {   size_t const neededInSize = ZSTD1_nextSrcSizeToDecompress(zds);
                size_t const toLoad = neededInSize - zds->inPos;   /* should always be <= remaining space within inBuff */
                size_t loadedSize;
                if (toLoad > zds->inBuffSize - zds->inPos) return ERROR(corruption_detected);   /* should never happen */
                loadedSize = ZSTD1_limitCopy(zds->inBuff + zds->inPos, toLoad, ip, iend-ip);
                ip += loadedSize;
                zds->inPos += loadedSize;
                if (loadedSize < toLoad) { someMoreWork = 0; break; }   /* not enough input, wait for more */

                /* decode loaded input */
                {  const int isSkipFrame = ZSTD1_isSkipFrame(zds);
                   size_t const decodedSize = ZSTD1_decompressContinue(zds,
                        zds->outBuff + zds->outStart, zds->outBuffSize - zds->outStart,
                        zds->inBuff, neededInSize);
                    if (ZSTD1_isError(decodedSize)) return decodedSize;
                    zds->inPos = 0;   /* input is consumed */
                    if (!decodedSize && !isSkipFrame) { zds->streamStage = zdss_read; break; }   /* this was just a header */
                    zds->outEnd = zds->outStart +  decodedSize;
            }   }
            zds->streamStage = zdss_flush;
            /* pass-through */

        case zdss_flush:
            {   size_t const toFlushSize = zds->outEnd - zds->outStart;
                size_t const flushedSize = ZSTD1_limitCopy(op, oend-op, zds->outBuff + zds->outStart, toFlushSize);
                op += flushedSize;
                zds->outStart += flushedSize;
                if (flushedSize == toFlushSize) {  /* flush completed */
                    zds->streamStage = zdss_read;
                    if (zds->outStart + zds->blockSize > zds->outBuffSize)
                        zds->outStart = zds->outEnd = 0;
                    break;
            }   }
            /* cannot complete flush */
            someMoreWork = 0;
            break;

        default: return ERROR(GENERIC);   /* impossible */
    }   }

    /* result */
    input->pos += (size_t)(ip-istart);
    output->pos += (size_t)(op-ostart);
    {   size_t nextSrcSizeHint = ZSTD1_nextSrcSizeToDecompress(zds);
        if (!nextSrcSizeHint) {   /* frame fully decoded */
            if (zds->outEnd == zds->outStart) {  /* output fully flushed */
                if (zds->hostageByte) {
                    if (input->pos >= input->size) {
                        /* can't release hostage (not present) */
                        zds->streamStage = zdss_read;
                        return 1;
                    }
                    input->pos++;  /* release hostage */
                }   /* zds->hostageByte */
                return 0;
            }  /* zds->outEnd == zds->outStart */
            if (!zds->hostageByte) { /* output not fully flushed; keep last byte as hostage; will be released when all output is flushed */
                input->pos--;   /* note : pos > 0, otherwise, impossible to finish reading last block */
                zds->hostageByte=1;
            }
            return 1;
        }  /* nextSrcSizeHint==0 */
        nextSrcSizeHint += ZSTD1_blockHeaderSize * (ZSTD1_nextInputType(zds) == ZSTD1nit_block);   /* preload header of next block */
        if (zds->inPos > nextSrcSizeHint) return ERROR(GENERIC);   /* should never happen */
        nextSrcSizeHint -= zds->inPos;   /* already loaded*/
        return nextSrcSizeHint;
    }
}
