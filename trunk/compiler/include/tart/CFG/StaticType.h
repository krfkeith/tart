/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */
 
#ifndef TART_CFG_STATICTYPE_H
#define TART_CFG_STATICTYPE_H

#ifndef TART_CFG_PRIMITIVETYPE_H
#include <tart/CFG/PrimitiveType.h>
#endif

#ifndef TART_CFG_FUNCTIONTYPE_H
#include <tart/CFG/FunctionType.h>
#endif

#ifndef TART_CFG_DEFN_H
#include <tart/CFG/Defn.h>
#endif

namespace tart {

/// -------------------------------------------------------------------
/// StaticType is a template class that is used to reference built-in
/// types as compile-time constants via template parameters. For example,
/// StaticType<TypeId_SInt32>::value is a synonym for
/// IntegerType::instance.

template<int type>
class StaticType {
public:
  static Type & value;
};

/// -------------------------------------------------------------------
/// Template to generate static argument names from indexes.

template<int index> class ParameterName {
public:
  static const char value[3];
};

/// -------------------------------------------------------------------
/// Template that generates a static formal parameter literal.
template<int type, int index, int paramFlags = 0>
class StaticParamDefn {
public:
  static ParameterDefn value;
};

template<int type, int index, int paramFlags>
ParameterDefn StaticParamDefn<type, index, paramFlags>::value(
  NULL,
  ParameterName<index>::value,
  &StaticType<type>::value, paramFlags);

/// -------------------------------------------------------------------
/// Template to generate array of 1 formal argument.

template<int arg0, int paramFlags = 0>
class StaticParamArray1 {
public:
  static ParameterDefn * value[1];
};

template<int arg0, int paramFlags>
ParameterDefn * StaticParamArray1<arg0, paramFlags>::value[1] = {
  &StaticParamDefn<arg0, 0, paramFlags>::value
};

/// -------------------------------------------------------------------
/// Template to generate array of 2 formal arguments.

template<int arg0, int arg1>
class StaticParamArray2 {
public:
  static ParameterDefn * value[2];
};

template<int arg0, int arg1>
ParameterDefn * StaticParamArray2<arg0, arg1>::value[2] = {
  &StaticParamDefn<arg0, 0>::value,
  &StaticParamDefn<arg1, 1>::value,
};

/// -------------------------------------------------------------------
/// Template to generate array of 3 formal arguments.

template<int arg0, int arg1, int arg2>
class StaticParamArray3 {
public:
  static ParameterDefn * value[3];
};

template<int arg0, int arg1, int arg2>
ParameterDefn * StaticParamArray3<arg0, arg1, arg2>::value[3] = {
  &StaticParamDefn<arg0, 0>::value,
  &StaticParamDefn<arg1, 1>::value,
  &StaticParamDefn<arg2, 2>::value,
};

/// -------------------------------------------------------------------
/// Template to function types taking 0 arguments.

template<int ret>
class StaticFnType0 {
public:
  static FunctionType value;
};

template<int ret>
FunctionType StaticFnType0<ret>::value(
  &StaticType<ret>::value, NULL, 0);

/// -------------------------------------------------------------------
/// Template to function types taking 1 argument.

template<int ret, int arg0, int paramFlags = 0>
class StaticFnType1 {
public:
  static FunctionType value;
  //static FunctionType constructor;
};

template<int ret, int arg0, int paramFlags>
FunctionType StaticFnType1<ret, arg0, paramFlags>::value(
  &StaticType<ret>::value,
  StaticParamArray1<arg0, paramFlags>::value, 1);

/// -------------------------------------------------------------------
/// Template to static function types taking 2 arguments.

template<int ret, int arg0, int arg1>
class StaticFnType2 {
public:
  static FunctionType value;
  //static FunctionType constructor;
};

template<int ret, int arg0, int arg1>
FunctionType StaticFnType2<ret, arg0, arg1>::value(
  &StaticType<ret>::value,
  StaticParamArray2<arg0, arg1>::value, 2);

/// -------------------------------------------------------------------
/// Template to static function types taking 3 arguments.

template<int ret, int arg0, int arg1, int arg2>
class StaticFnType3 {
public:
  static FunctionType value;
  //static FunctionType constructor;
};

template<int ret, int arg0, int arg1, int arg2>
FunctionType StaticFnType3<ret, arg0, arg1, arg2>::value(
  &StaticType<ret>::value,
  StaticParamArray3<arg0, arg1, arg2>::value, 3);

}

#endif
