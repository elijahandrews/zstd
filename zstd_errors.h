/**
 * Copyright (c) 2016-present, Yann Collet, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the root directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 */

#ifndef ZSTD1_ERRORS_H_398273423
#define ZSTD1_ERRORS_H_398273423

#if defined (__cplusplus)
extern "C" {
#endif

/*===== dependency =====*/
#include <stddef.h>   /* size_t */


/* =====   ZSTD1ERRORLIB_API : control library symbols visibility   ===== */
#ifndef ZSTD1ERRORLIB_VISIBILITY
#  if defined(__GNUC__) && (__GNUC__ >= 4)
#    define ZSTD1ERRORLIB_VISIBILITY __attribute__ ((visibility ("default")))
#  else
#    define ZSTD1ERRORLIB_VISIBILITY
#  endif
#endif
#if defined(ZSTD1_DLL_EXPORT) && (ZSTD1_DLL_EXPORT==1)
#  define ZSTD1ERRORLIB_API __declspec(dllexport) ZSTD1ERRORLIB_VISIBILITY
#elif defined(ZSTD1_DLL_IMPORT) && (ZSTD1_DLL_IMPORT==1)
#  define ZSTD1ERRORLIB_API __declspec(dllimport) ZSTD1ERRORLIB_VISIBILITY /* It isn't required but allows to generate better code, saving a function pointer load from the IAT and an indirect jump.*/
#else
#  define ZSTD1ERRORLIB_API ZSTD1ERRORLIB_VISIBILITY
#endif

/*-****************************************
 *  error codes list
 *  note : this API is still considered unstable
 *         it should not be used with a dynamic library
 *         only static linking is allowed
 ******************************************/
typedef enum {
  ZSTD1_error_no_error,
  ZSTD1_error_GENERIC,
  ZSTD1_error_prefix_unknown,
  ZSTD1_error_version_unsupported,
  ZSTD1_error_parameter_unknown,
  ZSTD1_error_frameParameter_unsupported,
  ZSTD1_error_frameParameter_unsupportedBy32bits,
  ZSTD1_error_frameParameter_windowTooLarge,
  ZSTD1_error_compressionParameter_unsupported,
  ZSTD1_error_compressionParameter_outOfBound,
  ZSTD1_error_init_missing,
  ZSTD1_error_memory_allocation,
  ZSTD1_error_stage_wrong,
  ZSTD1_error_dstSize_tooSmall,
  ZSTD1_error_srcSize_wrong,
  ZSTD1_error_corruption_detected,
  ZSTD1_error_checksum_wrong,
  ZSTD1_error_tableLog_tooLarge,
  ZSTD1_error_maxSymbolValue_tooLarge,
  ZSTD1_error_maxSymbolValue_tooSmall,
  ZSTD1_error_dictionary_corrupted,
  ZSTD1_error_dictionary_wrong,
  ZSTD1_error_dictionaryCreation_failed,
  ZSTD1_error_frameIndex_tooLarge,
  ZSTD1_error_seekableIO,
  ZSTD1_error_maxCode
} ZSTD1_ErrorCode;

/*! ZSTD1_getErrorCode() :
    convert a `size_t` function result into a `ZSTD1_ErrorCode` enum type,
    which can be used to compare with enum list published above */
ZSTD1ERRORLIB_API ZSTD1_ErrorCode ZSTD1_getErrorCode(size_t functionResult);
ZSTD1ERRORLIB_API const char* ZSTD1_getErrorString(ZSTD1_ErrorCode code);


#if defined (__cplusplus)
}
#endif

#endif /* ZSTD1_ERRORS_H_398273423 */
