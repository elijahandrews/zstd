/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */



/*-*************************************
*  Dependencies
***************************************/
#include <stdlib.h>      /* malloc, calloc, free */
#include <string.h>      /* memset */
#include "error_private.h"
#define ZSTD1_STATIC_LINKING_ONLY
#include "zstd.h"


/*-****************************************
*  Version
******************************************/
unsigned ZSTD1_versionNumber(void) { return ZSTD1_VERSION_NUMBER; }

const char* ZSTD1_versionString(void) { return ZSTD1_VERSION_STRING; }


/*-****************************************
*  ZSTD1 Error Management
******************************************/
/*! ZSTD1_isError() :
*   tells if a return value is an error code */
unsigned ZSTD1_isError(size_t code) { return ERR_isError(code); }

/*! ZSTD1_getErrorName() :
*   provides error code string from function result (useful for debugging) */
const char* ZSTD1_getErrorName(size_t code) { return ERR_getErrorName(code); }

/*! ZSTD1_getError() :
*   convert a `size_t` function result into a proper ZSTD1_errorCode enum */
ZSTD1_ErrorCode ZSTD1_getErrorCode(size_t code) { return ERR_getErrorCode(code); }

/*! ZSTD1_getErrorString() :
*   provides error code string from enum */
const char* ZSTD1_getErrorString(ZSTD1_ErrorCode code) { return ERR_getErrorString(code); }


/*=**************************************************************
*  Custom allocator
****************************************************************/
void* ZSTD1_malloc(size_t size, ZSTD1_customMem customMem)
{
    if (customMem.customAlloc)
        return customMem.customAlloc(customMem.opaque, size);
    return malloc(size);
}

void* ZSTD1_calloc(size_t size, ZSTD1_customMem customMem)
{
    if (customMem.customAlloc) {
        /* calloc implemented as malloc+memset;
         * not as efficient as calloc, but next best guess for custom malloc */
        void* const ptr = customMem.customAlloc(customMem.opaque, size);
        memset(ptr, 0, size);
        return ptr;
    }
    return calloc(1, size);
}

void ZSTD1_free(void* ptr, ZSTD1_customMem customMem)
{
    if (ptr!=NULL) {
        if (customMem.customFree)
            customMem.customFree(customMem.opaque, ptr);
        else
            free(ptr);
    }
}
