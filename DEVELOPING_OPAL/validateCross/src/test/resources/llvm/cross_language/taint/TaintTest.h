/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class org_opalj_fpcf_fixtures_taint_xlang_TaintTest */

#ifndef _Included_org_opalj_fpcf_fixtures_taint_xlang_TaintTest
#define _Included_org_opalj_fpcf_fixtures_taint_xlang_TaintTest
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    sum
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_sum
  (JNIEnv *, jobject, jint, jint);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    propagate_source
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_propagate_1source
  (JNIEnv *, jobject);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    propagate_sanitize
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_propagate_1sanitize
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    propagate_sink
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_propagate_1sink
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    sanitize_only_a_into_sink
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_sanitize_1only_1a_1into_1sink
  (JNIEnv *, jobject, jint, jint);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    propagate_identity_to_sink
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_propagate_1identity_1to_1sink
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    propagate_zero_to_sink
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_propagate_1zero_1to_1sink
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    native_array_tainted
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_native_1array_1tainted
  (JNIEnv *, jobject);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    native_array_untainted
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_native_1array_1untainted
  (JNIEnv *, jobject);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    propagate_to_java_sink
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_propagate_1to_1java_1sink
  (JNIEnv *, jobject, jint);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    propagate_from_java_source
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_propagate_1from_1java_1source
  (JNIEnv *, jobject);

/*
 * Class:     org_opalj_fpcf_fixtures_taint_xlang_TaintTest
 * Method:    propagate_java_sanitize
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_org_opalj_fpcf_fixtures_taint_xlang_TaintTest_propagate_1java_1sanitize
  (JNIEnv *, jobject, jint);

#ifdef __cplusplus
}
#endif
#endif