/***********************************************************************/
/*                                                                     */
/*                                 GALAX                               */
/*                              XQuery Engine                          */
/*                                                                     */
/*  Copyright 2001-2007.                                               */
/*  Distributed only by permission.                                    */
/*                                                                     */
/***********************************************************************/


/* $Id: galax_jni_stub.c,v 1.18 2007/05/02 19:31:00 mff Exp $ */

#include <string.h>

#include <caml/mlvalues.h>
#include <caml/callback.h>
#include <caml/memory.h>

#include <jni.h>

#include "galax.h"

#include "galapi_Galax.h"
#include "galapi_Item.h"
#include "galapi_ItemList.h"
#include "galapi_Atomic.h"
#include "galapi_xsString.h"
#include "galapi_xsBoolean.h"
#include "galapi_xsInteger.h"
#include "galapi_xsDecimal.h"
#include "galapi_xsFloat.h"
#include "galapi_xsDouble.h"
#include "galapi_xsAnyURI.h"
#include "galapi_xsUntyped.h"
#include "galapi_xsQName.h"
#include "galapi_xsDate.h"
#include "galapi_xsTime.h"
#include "galapi_xsDateTime.h"
#include "galapi_xsDayTimeDuration.h"
#include "galapi_xsYearMonthDuration.h"
#include "galapi_Node.h"
#include "galapi_Document.h"
#include "galapi_Element.h"
#include "galapi_Text.h"
#include "galapi_Attribute.h"
#include "galapi_Comment.h"
#include "galapi_ProcessingInstruction.h"
#include "galapi_ProcessingContext.h"
#include "galapi_ExternalContext.h"
#include "galapi_CompiledModule.h"
#include "galapi_CompiledProgram.h"
#include "galapi_NodeList.h"
#include "galapi_AttributeList.h"
#include "galapi_AtomicList.h"

/* #define DEBUG */

/* Exception handling */

void throw_exception (JNIEnv *env, char *exception, 
		      char *message,
		      int code) {
  jclass newExcCls;

  // (*env)->ExceptionDescribe(env);
  // (*env)->ExceptionClear(env);

  newExcCls = (*env)->FindClass(env, exception);
  if (newExcCls == NULL) {
    fprintf (stderr, "Unable to load the exception class %s\n", exception);
    fflush (stderr);
    return;
  }
#ifdef DEBUG
  fprintf (stderr, "Exception class %s found!\n", exception);
#endif

  (*env)->ThrowNew(env, newExcCls, message);
}

void
throw_galapi_exception (JNIEnv *env, char *message, int err) {
#ifdef DEBUG
  fprintf(stderr,"Throwing Java exception with message: %s\n", message);
#endif

  //  why doesn't this work ??? :
  throw_exception (env, "galapi/GalapiException", message, err);
  // throw_exception (env, "java/lang/Exception", message, err);
}


/**
 * Class:     Galax
 * Method:    initialize
 * @param jne
 * @param jo
 * @param jarr array holding the program arguments
 */



JNIEXPORT void JNICALL
 Java_galapi_Galax_nativeInitialize (JNIEnv * jne, jobject jo)
{
  
#ifdef DEBUG
  fprintf (stderr, "Calling galax_init() from Java\n");
#endif

  galax_init ();

#ifdef DEBUG
  fprintf (stderr, "Successful galax_init() called from Java stub\n");
#endif

}// Java_GALAX_initialize()



/*
 * jarr is an array of java.lang.String
 * @return a C array of (char*)
 */

char ** getStringArray (JNIEnv * jne, jobjectArray jarr) {

  jsize sz = (*jne)->GetArrayLength (jne, jarr);
  char **a = (char **)malloc (sz * sizeof(char *));
  jsize i;
  const char *s;
  jstring js;

  for (i=0; i<sz; i++) {
    js = (jstring) (*jne)->GetObjectArrayElement (jne, jarr, i);
    s = (*jne)->GetStringUTFChars (jne, js, NULL); 
    a[i] = (char *) s;
  }//end for

  return a;
}


void releaseStringArray (JNIEnv * jne, jobjectArray jarr, char **a) {

  jsize sz = (*jne)->GetArrayLength (jne, jarr);
  jsize j;
  jstring js;

  for (j=0; j<sz; j++) {
    js = (jstring) (*jne)->GetObjectArrayElement (jne, jarr, j);
    (*jne)->ReleaseStringUTFChars (jne, js, a[j]);
  }//end for
  
  free (a);
}

/*
 * class Context
 */



JNIEXPORT void JNICALL 
Java_galapi_Context_nativeFree (JNIEnv * jne, jclass jc, jint ji) {
  item_free ((item)ji);

}// Java_Context_nativeFree()



/*
 * class Item
 */

JNIEXPORT void JNICALL 
Java_galapi_Item_nativeFree (JNIEnv * jne, jclass jc, jint ji) {
  item_free ((item)ji);

}// Java_Item_nativeFree()

/*
 JNIEXPORT jint JNICALL
Java_galapi_Item_nativeAtomicValue (JNIEnv *jne, jclass jc, jint ji) {
  atomicValue v;
  galax_err err = galax_get_atomicValue ((item) ji, &v);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  return (jint) v;

}// Java_Item_nativeAtomicValue()

JNIEXPORT jint JNICALL
Java_galapi_Item_nativeNode (JNIEnv *jne, jclass jc, jint ji) {
  element e;
  galax_err err;

#ifdef DEBUG
  fprintf (stderr,"GALAX GET NODE2\n");
#endif
  err = galax_get_node ((item) ji, &e);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
    
#ifdef DEBUG
  fprintf (stderr,"OUT OF GALAX GET NODE2\n");
#endif
  return (jint) e;

}// Java_Item_nativeNode()

JNIEXPORT jint JNICALL
Java_galapi_Item_nativeAttribute (JNIEnv *jne, jclass jc, jint ji) {
  element e;
  galax_err err = galax_get_element ((item) ji, &e);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) e;

}// Java_Item_nativeAttribute()


JNIEXPORT jint JNICALL
Java_galapi_Item_nativeElement (JNIEnv *jne, jclass jc, jint ji) {
  element e;
  galax_err err = galax_get_attribute ((item) ji, &e);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) e;

}// Java_Item_nativeElement()
*/

JNIEXPORT jstring JNICALL
Java_galapi_Item_nativeStringValue (JNIEnv *jne, jclass jc, jint ji) {
  char *s;
  galax_err err = galax_string_value ((item) ji, &s);
  if (0 != err)
    throw_exception (jne, "java/lang/RuntimeException", galax_error_string, err);
  
  return (*jne)->NewStringUTF(jne, s);
}// Java_Item_nativeStringValue()


JNIEXPORT jstring JNICALL
Java_galapi_Item_nativeItemKind (JNIEnv *jne, jclass jc, jint ji) {
  char *s;
  galax_err err = galax_item_kind ((item) ji, &s);
  if (0 != err)
    throw_exception (jne, "java/lang/RuntimeException", galax_error_string, err);
  
  return (*jne)->NewStringUTF(jne, s);
}// Java_Item_nativeString()



/*
 * class ItemList
 */


JNIEXPORT jint JNICALL 
Java_galapi_ItemList_nativeEmptyList (JNIEnv *jne, jclass jc) {
  return (jint) itemlist_empty();  
}// Java_ItemList_nativeEmptyList()


JNIEXPORT void JNICALL 
Java_galapi_ItemList_nativeFreeItemList
(JNIEnv *jne, jclass jc, jint ji) {
  itemlist_free ((itemlist)ji);

}// Java_ItemList_nativeFreeItemList()


JNIEXPORT jint JNICALL
Java_galapi_ItemList_nativeCons (JNIEnv *jne, jclass jc, jint jit, jint jitl) {
  itemlist il = itemlist_cons ((item) jit, (itemlist) jitl);
  return (jint) il;

}// Java_Item_nativeCons()

JNIEXPORT jboolean JNICALL 
Java_galapi_ItemList_nativeIsEmpty (JNIEnv *jne, jclass jc, jint ji) {
  int b = is_empty ((itemlist) ji);
  return (jboolean) b;

}// Java_ItemList_nativeIsEmpty()


JNIEXPORT jstring JNICALL 
Java_galapi_ItemList_nativeSerialize (JNIEnv *jne, jclass jc, jint ji) {
  char *s = NULL;
  processing_context pc;
  galax_err err;

  err = galax_default_processing_context(&pc);
  if (0 != err)
    throw_exception (jne, "java/lang/RuntimeException", galax_error_string, err);

  err = galax_serialize_to_string (pc, (itemlist) ji, &s);
  
  if (0 != err)
    throw_exception (jne, "java/lang/RuntimeException", galax_error_string, err);

  return (*jne)->NewStringUTF(jne, s);
}// Java_ItemList_nativeSerialize()



JNIEXPORT jint JNICALL
Java_galapi_ItemList_nativeItemsFirst (JNIEnv *jne, jclass jc, jint jitl) {
  item i = items_first  ((itemlist) jitl);
  return (jint) i;

}// Java_ItemList_nativeItemsFirst


JNIEXPORT jint JNICALL
Java_galapi_ItemList_nativeItemsNext (JNIEnv *jne, jclass jc, jint jitl) {
  itemlist il = items_next  ((itemlist) jitl);
  return (jint) il;

}// Java_Item_nativeIemsNext()

/*
 *
 * clas Atomic
 *
 */

JNIEXPORT jstring JNICALL
Java_galapi_Atomic_nativeSerialize (JNIEnv *jne, jclass jc, jint ji) {  
  char *s;
  galax_err err = galax_string_of_atomicValue ((atomicValue) ji, &s);

  if (0 != err)
    throw_exception (jne, "java/lang/RuntimeException", galax_error_string, err);

  return (*jne)->NewStringUTF(jne, s);
}// Java_Atomic_nativeSerialize()


/*
 * class xsString
 */

JNIEXPORT jint JNICALL 
Java_galapi_xsString_stringToItem (JNIEnv *jne, jclass jc, jstring js) {

  const char *str = (*jne)->GetStringUTFChars(jne, js, NULL);
  atomicString it;
  galax_err err = galax_atomicString ((char *)str, &it);

  (*jne)->ReleaseStringUTFChars (jne, js, str);

  // Attention: the exception should always be thrown after having
  // released all the resources

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) it;

}// Java_xsString_stringToItem()

/*
 * class xsDate
 */

JNIEXPORT jint JNICALL 
Java_galapi_xsDate_dateToItem (JNIEnv *jne, jclass jc, jstring js) {

  const char *str = (*jne)->GetStringUTFChars(jne, js, NULL);
  atomicDate it;
  galax_err err = galax_atomicDate ((char *)str, &it);

  (*jne)->ReleaseStringUTFChars (jne, js, str);

  // Attention: the exception should always be thrown after having
  // released all the resources

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) it;

}// Java_xsDate_dateToItem()


/*
 * class xsTime
 */

JNIEXPORT jint JNICALL 
Java_galapi_xsTime_timeToItem (JNIEnv *jne, jclass jc, jstring js) {

  const char *str = (*jne)->GetStringUTFChars(jne, js, NULL);
  atomicTime it;
  galax_err err = galax_atomicTime ((char *)str, &it);

  (*jne)->ReleaseStringUTFChars (jne, js, str);

  // Attention: the exception should always be thrown after having
  // released all the resources

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) it;

}// Java_xsTime_timeToItem()

/*
 * class xsDateTime
 */

JNIEXPORT jint JNICALL 
Java_galapi_xsDateTime_datetimeToItem (JNIEnv *jne, jclass jc, jstring js) {

  const char *str = (*jne)->GetStringUTFChars(jne, js, NULL);
  atomicDateTime it;
  galax_err err = galax_atomicDateTime ((char *)str, &it);

  (*jne)->ReleaseStringUTFChars (jne, js, str);

  // Attention: the exception should always be thrown after having
  // released all the resources

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) it;

}// Java_xsDateTime_datetimeToItem()

/*
 * class xsDayTimeDuration
 */

JNIEXPORT jint JNICALL 
Java_galapi_xsDayTimeDuration_daytimedurationToItem (JNIEnv *jne, jclass jc, jstring js) {

  const char *str = (*jne)->GetStringUTFChars(jne, js, NULL);
  atomicDayTimeDuration it;
  galax_err err = galax_atomicDayTimeDuration ((char *)str, &it);

  (*jne)->ReleaseStringUTFChars (jne, js, str);

  // Attention: the exception should always be thrown after having
  // released all the resources

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) it;

}// Java_xsDayTimeTime_daytimedurationToItem()


/*
 * class xsYearMonthDuration
 */

JNIEXPORT jint JNICALL 
Java_galapi_xsYearMonthDuration_yearmonthdurationToItem (JNIEnv *jne, jclass jc, jstring js) {

  const char *str = (*jne)->GetStringUTFChars(jne, js, NULL);
  atomicYearMonthDuration it;
  galax_err err = galax_atomicYearMonthDuration ((char *)str, &it);

  (*jne)->ReleaseStringUTFChars (jne, js, str);

  // Attention: the exception should always be thrown after having
  // released all the resources

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) it;

}// Java_xsDayTimeTime_yearmonthdurationToItem()


/*
 * xsBoolean
 */

JNIEXPORT jint JNICALL 
Java_galapi_xsBoolean_booleanToItem
(JNIEnv *jne, jclass jc, jboolean jb) {
  atomicBoolean xsb;
  galax_err err = galax_atomicBoolean ((int) jb, &xsb);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) xsb;

}// Java_xsBoolean_booleanToItem()


JNIEXPORT jboolean JNICALL Java_galapi_xsBoolean_itemToBoolean
(JNIEnv *jne, jclass jc, jint ji) {
  int b;
  galax_err err = galax_boolean_of_atomicBoolean ((atomicBoolean) ji, &b);
  
  if (0 != err)
      throw_galapi_exception (jne, galax_error_string, err);

  return (jboolean) b;

}// Java_xsBoolean_itemToBoolean()



/*
 * xsInteger
 */

JNIEXPORT jint JNICALL Java_galapi_xsInteger_integerToItem
(JNIEnv *jne, jclass jc, jint ji){
  atomicInteger xi;
  galax_err err = galax_atomicInteger ((int) ji, &xi);

  if (0 != err)
      throw_galapi_exception (jne, galax_error_string, err);
  
  return (jint) xi;  

} // Java_xsInteger_integerToItem()


JNIEXPORT jint JNICALL Java_galapi_xsInteger_itemToInteger
(JNIEnv *jne, jclass jc, jint ji) {
  int d;
  
  galax_err err = galax_integer_of_atomicInteger ((atomicInteger) ji, &d);

  if (0 != err)
      throw_galapi_exception (jne, galax_error_string, err);
  
  return (jint) d;

}// Java_xsInteger_itemToInteger ()


/*
 * xsDecimal
 */

JNIEXPORT jint JNICALL Java_galapi_xsDecimal_decimalToItem
(JNIEnv *jne, jclass jc, jint ji){
  atomicDecimal xi;
  galax_err err = galax_atomicDecimal ((int) ji, &xi);
  
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  
  return (jint) xi;  

} // Java_xsDecimal_decimalToItem()


JNIEXPORT jint JNICALL Java_galapi_xsDecimal_itemToDecimal
(JNIEnv *jne, jclass jc, jint ji) {
  int d;
  galax_err err = galax_decimal_of_atomicDecimal ((atomicDecimal) ji, &d);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) d;

}// Java_xsDecimal_itemToDecimal()


/*
 * xsFloat
 */

JNIEXPORT jint JNICALL Java_galapi_xsFloat_floatToItem
(JNIEnv *jne, jclass jc, jdouble jf){
  atomicFloat xf;
  galax_err err = galax_atomicFloat ((double) jf, &xf);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  
  return (jint) xf;  

} // Java_xsFloat_floatToItem()


JNIEXPORT jdouble JNICALL Java_galapi_xsFloat_itemToFloat
(JNIEnv *jne, jclass jc, jint ji) {
  double f;
  galax_err err = galax_float_of_atomicFloat ((atomicFloat) ji, &f);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jdouble) f;

}// Java_xsFloat_itemToFloat()



/*
 * xsDouble
 */

JNIEXPORT jint JNICALL Java_galapi_xsDouble_doubleToItem
(JNIEnv *jne, jclass jc, jdouble jf){
  atomicDouble xg;
  galax_err err = galax_atomicDouble ((double) jf, &xg);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) xg;  

}// Java_xsDouble_doubleToItem()


JNIEXPORT jdouble JNICALL Java_galapi_xsDouble_itemToDouble
(JNIEnv *jne, jclass jc, jint ji) {
  double g;
  galax_err err = galax_float_of_atomicDouble ((atomicDouble) ji, &g);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jdouble) g;

}// Java_xsDouble_itemToDouble()


/*
 * class xsAnyURI
 */

JNIEXPORT jint JNICALL 
Java_galapi_xsAnyURI_uriToItem (JNIEnv *jne, jclass jc, jstring js) {


  const char *str = (*jne)->GetStringUTFChars(jne, js, NULL);
  atomicAnyURI it;
  galax_err err = galax_atomicAnyURI ((char *)str, &it);

  (*jne)->ReleaseStringUTFChars (jne, js, str);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) it;

}// Java_xsAnyURI_uriToItem()


/*
 * xsQName
 */

JNIEXPORT jint JNICALL Java_galapi_xsQName_qNameToItem
(JNIEnv *jne, jclass jc, jint ji, jstring js) {
  
  const char *name = (*jne)->GetStringUTFChars(jne, js, NULL);  

  atomicQName it;
  galax_err err;

#ifdef DEBUG
  fprintf (stderr, "Calling xsQName\n");
#endif

  err = galax_atomicQName ((namespace_env) ji, (char *)name, &it);

  (*jne)->ReleaseStringUTFChars (jne, js, name);

#ifdef DEBUG
  fprintf (stderr, "xsQName call done\n");
#endif

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  
  return (jint) it; 

}// Java_xsQName_qNameToItem()


/*
 * Document
 */

JNIEXPORT jint JNICALL Java_galapi_Document_documentToItem
(JNIEnv *jne, jclass jc, jstring juri, jint nodes) {
  
  document d;

  const char *str = (*jne)->GetStringUTFChars(jne, juri, NULL);
  galax_err err = galax_documentNode((char *) str, (itemlist) nodes, &d);
  
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) d;

}// Java_Element_documentToItem()

/*
 * Element
 */

JNIEXPORT jint JNICALL Java_galapi_Element_elementToItem
(JNIEnv *jne, jclass jc, jint jq, jint attrs, jint nodes, jint jq1) {
  
  element e;
  galax_err err = galax_elementNode((atomicQName) jq, 
				    (attribute_list) attrs,
				    (itemlist) nodes,
				    (atomicQName) jq1,
				    &e);
  
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) e;

}// Java_Element_elementToItem()


/*
 * Text
 */

JNIEXPORT jint JNICALL Java_galapi_Text_textToItem
(JNIEnv * jne, jclass jc, jint ji) {
  
  text t;
  galax_err err = galax_textNode((atomicString) ji, &t);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) t;
}// Java_Text_textToItem


/*
 * Attribute
 */

JNIEXPORT jint JNICALL Java_galapi_Attribute_attributeToItem
(JNIEnv *jne, jclass jc, jint q, jint s, jint q1) {
  
  attribute a;
  galax_err err = galax_attributeNode((atomicQName)q,
				      (atomicString) s, (atomicQName)q1,
				      &a);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) a;
}// Java_Attribute_attributeToItem()



/*
 * Comment
 */

JNIEXPORT jint JNICALL Java_galapi_Comment_commentToItem
(JNIEnv * jne, jclass jc, jint ji) {
  
  text t;
  galax_err err = galax_commentNode((atomicString) ji, &t);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) t;
}// Java_Comment_commentToItem

/*
 * ProcessingInstruction
 */


JNIEXPORT jint JNICALL 
Java_galapi_ProcessingInstruction_processingInstructionToItem
(JNIEnv *jne, jclass jc, jint ji, jint ji1) {
  processingInstruction pi;
  
  galax_err err =
    galax_processingInstructionNode((atomicString) ji, (atomicString) ji1, &pi);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) pi;
}// Java_ProcessingInstruction_processingInstructionToItem()


/*
 * Node
 */

JNIEXPORT jint JNICALL Java_galapi_Node_nativeParent
(JNIEnv *jne, jclass jc, jint ji) {

  node_list ndl;
  galax_err err = galax_parent ((node) ji, &ndl);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) ndl;

}// Java_Node_nativeParent



JNIEXPORT jint JNICALL Java_galapi_Node_nativeChildren
(JNIEnv *jne, jclass jc, jint ji) {

  node_list ndl;
  galax_err err = galax_children ((node) ji, &ndl);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) ndl;

}// Java_Node_nativeChildren()



JNIEXPORT jint JNICALL Java_galapi_Node_nativeBaseURI
(JNIEnv *jne, jclass jc, jint ji) {
  atomicValue_list al;
  galax_err err = galax_base_uri ((node) ji, &al); 

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  
  return (jint) al;
}// Java_Node_nativeBaseURI


JNIEXPORT jint JNICALL Java_galapi_Node_nativeNodeName
(JNIEnv *jne, jclass jc, jint ji) {
  atomicValue_list al;
  galax_err err = galax_node_name ((node) ji, &al); 

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  
  return (jint) al;
}// Java_Node_nativeBaseURI


JNIEXPORT jstring JNICALL
Java_galapi_Node_nativeNodeKind (JNIEnv *jne, jclass jc, jint ji) {
  char *s;
  galax_err err = galax_node_kind ((item) ji, &s);
  if (0 != err)
    throw_exception (jne, "java/lang/RuntimeException", galax_error_string, err);
  
  return (*jne)->NewStringUTF(jne, s);
}// Java_Item_nativeString()


JNIEXPORT jint JNICALL Java_galapi_Node_nativeTypedValue
(JNIEnv *jne, jclass jc, jint ji) {

  atomicValue_list a;
  galax_err err = galax_typed_value ((node) ji, &a);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  
  return (jint) a;

}// Java_Node_nativeTypedValue


JNIEXPORT jint JNICALL Java_galapi_Node_nativeAttributes
(JNIEnv *jne, jclass jc, jint ji) {

  attribute_list a;
  galax_err err = galax_attributes ((node) ji, &a);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) a;

}// Java_Node_nativeAttributes



/*
 * class ExternalContext
 */

JNIEXPORT jint JNICALL 
Java_galapi_ExternalContext_externalContextToItem
(JNIEnv * jne, jclass jc, jint pc, jint ctxt, jint tz,
 jobjectArray jarr, jintArray values) {

  jint *p = (*jne)->GetIntArrayElements (jne, values, NULL);
  //jsize n = (*jne)->GetArrayLength (jne, values);

  jsize sz = (*jne)->GetArrayLength (jne, jarr);
  char **a = getStringArray (jne, jarr);
  external_context ec;
  itemlist ctil = (itemlist) ctxt;
  itemlist tzil = (itemlist) tz;

  galax_err err =
    galax_build_external_context ((processing_context) pc, ctil, tzil, a, (itemlist*) p, sz, &ec);

    
  releaseStringArray (jne, jarr, a);
  (*jne)->ReleaseIntArrayElements (jne, values, p, 0); 

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) ec;  
  
}// Java_galapi_ExternalContext_externalContextToItem()

JNIEXPORT jint JNICALL 
Java_galapi_ExternalContext_defaultExternalContext
(JNIEnv * jne, jclass jc) {

  external_context ec;

  galax_err err = galax_default_external_context (&ec);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) ec;  
  
}// Java_galapi_ExternalContext_defaultExternalContext()

/*
 * Galax static methods
 */

JNIEXPORT jint JNICALL Java_galapi_Galax_nativeLoadDocument
(JNIEnv *jne, jclass jc, jint pc, jint source_kind, jstring js) {

  node_list ndl;      
  const char *input = (*jne)->GetStringUTFChars (jne, js, NULL);

  galax_err err = galax_load_document ((processing_context)pc, (int)source_kind, (char *) input, &ndl);


  (*jne)->ReleaseStringUTFChars (jne, js, input);    
  
#ifdef DEBUG
  fprintf (stderr, "LoadDocument error code=%d\n", err);
  fflush (stderr);
#endif
  

  if (0 != err) {    
    throw_galapi_exception (jne, galax_error_string, err);
  }
  
  return (jint) ndl;
  
}// Java_Galax_nativeLoadDocument()



JNIEXPORT jstring JNICALL Java_galapi_Galax_nativeSerializeToString
(JNIEnv *jne, jclass jc, jint pc, jint ji) {

  char *s;
  galax_err err = galax_serialize_to_string ((processing_context)pc, (itemlist) ji, &s); 

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (*jne)->NewStringUTF(jne, s);  

}// Java_Galax_nativeSerializeToString()


JNIEXPORT void JNICALL Java_galapi_Galax_nativeSerializeToStdout
(JNIEnv *jne, jclass jc, jint pc, jint ji) {

  galax_err err = 
    galax_serialize_to_stdout ((processing_context)pc, (itemlist) ji);

  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

}// Java_Galax_nativeSerializeToStdout()


JNIEXPORT void JNICALL Java_galapi_Galax_nativeSerializeToFile
(JNIEnv *jne, jclass jc, jint pc, jstring js, jint ji) {

    const char *s = (*jne)->GetStringUTFChars (jne, js, NULL); 
    galax_err err = galax_serialize_to_file ((processing_context)pc, (char *) s, (itemlist)ji);
    (*jne)->ReleaseStringUTFChars (jne, js, s);

    if (0 != err)
      throw_galapi_exception (jne, galax_error_string, err);

}// Java_Galax_nativeSerializeToFile()


/*
 * Galax - evaluation & query
 *
 */


JNIEXPORT jint JNICALL Java_galapi_Galax_nativeDefaultProcessingContext
(JNIEnv * jne, jclass jc) {
  processing_context pc;
  galax_err err = galax_default_processing_context (&pc);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  return (jint) pc;
}

JNIEXPORT jint JNICALL Java_galapi_Galax_nativeLoadStandardLibrary
(JNIEnv *jne, jclass jc, jint ji) {
  compiled_program mc;
  galax_err err = galax_load_standard_library ((processing_context) ji, &mc);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) mc;
}

JNIEXPORT jint JNICALL Java_galapi_Galax_nativeImportLibraryModule
(JNIEnv *jne, jclass jc, jint cp, jint input_source_kind, jstring js) {
  const char *s = (*jne)->GetStringUTFChars (jne, js, NULL); 
  compiled_program mc;

  galax_err err = galax_import_library_module ((compiled_program) cp, 
					       (int) input_source_kind, 
					   (char *)s, 
					   &mc);
  
  (*jne)->ReleaseStringUTFChars (jne, js, s);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) mc;
}

JNIEXPORT jint JNICALL Java_galapi_Galax_nativeImportMainModule
(JNIEnv *jne, jclass jc, jint ji, jboolean jec, jint input_source_kind, jstring js) {

  const char *s = (*jne)->GetStringUTFChars (jne, js, NULL); 
  compiled_module cm;
  galax_err err = galax_import_main_module ((compiled_program) ji, 
					    (int) jec,
					    (int) input_source_kind,
					    (char *)s,
					    &cm);
  (*jne)->ReleaseStringUTFChars (jne, js, s);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  return (jint) cm;  
}

JNIEXPORT jint JNICALL
Java_galapi_CompiledProgram_nativeNsenvFromCompiledProgram(JNIEnv *jne, jclass jc, jint ji) {
  namespace_env nsenv;
  galax_err err = galax_nsenv_from_compiled_program((compiled_program) ji, &nsenv);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) nsenv;
}


JNIEXPORT jint JNICALL
Java_galapi_CompiledModule_nativeGetCompiledProgram(JNIEnv *jne, jclass jc, jint cm) {
  return (jint) ((compiled_module)cm)->compiled_program;
}

JNIEXPORT jint JNICALL
Java_galapi_CompiledModule_nativeGetCompiledStatements(JNIEnv *jne, jclass jc, jint cm) {
  return (jint) ((compiled_module)cm)->compiled_stmts;
}

JNIEXPORT jint JNICALL
Java_galapi_Galax_nativeEvalProgram(JNIEnv *jne, jclass jc, jint cp, jint exc) {
  prepared_program pp;
  galax_err err = galax_eval_program((compiled_program) cp, (external_context) exc, &pp);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) pp;  
}


JNIEXPORT jint JNICALL
Java_galapi_Galax_nativeEvalStatement(JNIEnv *jne, jclass jc, jint pp, jint input_source_kind, jstring js) {

  const char *s = (*jne)->GetStringUTFChars (jne, js, NULL); 
  itemlist il;
  galax_err err = galax_eval_statement ((prepared_program) pp, (int) input_source_kind, (char *)s, &il);
  (*jne)->ReleaseStringUTFChars (jne, js, s);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);
  return (jint) il;  

}

JNIEXPORT jint JNICALL
Java_galapi_Galax_nativeEvalCompiledStatement(JNIEnv *jne, jclass hc, jint pp, jint cs) {

  itemlist il;
  galax_err err = galax_eval_compiled_statement ((prepared_program) pp, (compiled_statement)cs, &il);
  if (0 != err)
    throw_galapi_exception (jne, galax_error_string, err);

  return (jint) il;
}


/*
 * Processing Context Features
 * Added by Philippe
 */

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetMonitorMem
(JNIEnv *jne, jclass jc, jint ji, jboolean jb) {
	galax_err err = galax_set_monitor_mem( (processing_context) ji, (int)jb);
	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
}
 
JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetMonitorTime
(JNIEnv *jne, jclass jc, jint ji, jboolean jb) {
	galax_err err = galax_set_monitor_time( (processing_context) ji, (int)jb);
	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
}
 
JNIEXPORT jint JNICALL Java_galapi_ProcessingContext_nativeMonitorOfLastCall 
(JNIEnv *jne, jclass jc, jint ji) {
	itemlist il;
	galax_err err = galax_monitor_of_last_call( (processing_context) ji, &il);
	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
	
	return (jint) il;
}
 
JNIEXPORT jint JNICALL Java_galapi_ProcessingContext_nativeMonitorOfAllCalls 
(JNIEnv *jne, jclass jc, jint ji) {
	itemlist il;
	galax_err err = galax_monitor_of_all_calls( (processing_context) ji, &il);
	
	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
	
	return (jint) il;
}
 
JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetNormalizationPhase
(JNIEnv *jne, jclass jc, jint ji, jboolean jb) {
	galax_err err = galax_set_normalization_phase( (processing_context) ji, (int) jb);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);	
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetTypingPhase
(JNIEnv *jne, jclass jc, jint ji, jboolean jb) {
	galax_err err = galax_set_typing_phase( (processing_context) ji, (int) jb);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);		
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetRewritingPhase
(JNIEnv *jne, jclass jc, jint ji, jboolean jb) {
	galax_err err = galax_set_rewriting_phase( (processing_context) ji, (int) jb);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);	
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetEvaluationPhase
(JNIEnv *jne, jclass jc, jint ji, jboolean jb) {
	galax_err err = galax_set_evaluation_phase( (processing_context) ji, (int) jb);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);	
}


JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetSbdoKind
(JNIEnv *jne, jclass jc, jint ji, jint jk) {
	galax_err err = galax_set_sbdo_kind( (processing_context) ji, (int) jk);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);	
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetTypingKind
(JNIEnv *jne, jclass jc, jint ji, jint jk) {
	galax_err err = galax_set_typing_kind( (processing_context) ji, (int) jk);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetSerializationKind
(JNIEnv *jne, jclass jc, jint ji, jint jk) {
	galax_err err = galax_set_serialization_kind( (processing_context) ji, (int) jk);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetSerializeNsKind
(JNIEnv *jne, jclass jc, jint ji, jint jk) {
	galax_err err = galax_set_serialize_ns( (processing_context) ji, (int) jk);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetProjectionKind
(JNIEnv *jne, jclass jc, jint ji, jint jk) {
	galax_err err = galax_set_projection_kind( (processing_context) ji, (int) jk);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
}
    
JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetBoundarySpaceKind
(JNIEnv *jne, jclass jc, jint ji, jint jb) {
	galax_err err = galax_set_boundary_space_kind( (processing_context) ji, (int) jb);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetXmlWhiteSpace
(JNIEnv *jne, jclass jc, jint ji, jboolean jb) {
	galax_err err = galax_set_xml_whitespace(  (processing_context) ji, (int) jb);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);	
}

JNIEXPORT void JNICALL Java_galapi_ProcessingContext_nativeSetXmlPisAndComments
(JNIEnv *jne, jclass jc, jint ji, jboolean jb) {
	galax_err err = galax_set_xml_pis_and_comments( (processing_context) ji, (int) jb);

	if (0 != err)
	  throw_galapi_exception (jne, galax_error_string, err);	
}

