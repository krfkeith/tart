/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#ifndef TART_TYPE_STATICTYPE_H
#define TART_TYPE_STATICTYPE_H

#ifndef TART_TYPE_PRIMITIVETYPE_H
#include "tart/Type/PrimitiveType.h"
#endif

#ifndef TART_TYPE_FUNCTIONTYPE_H
#include "tart/Type/FunctionType.h"
#endif

#ifndef TART_DEFN_FUNCTIONDEFN_H
#include "tart/Defn/FunctionDefn.h"
#endif

namespace tart {

/// -------------------------------------------------------------------
/// Template to generate static argument names from indexes.

template<int index> class ParameterName {
public:
  static const char value[3];
};

/// -------------------------------------------------------------------
/// Template that generates a static formal parameter literal.
template<class Type, int index, int paramFlags = 0>
class StaticParamDefn {
public:
  static ParameterDefn value;
};

template<class Type, int index, int paramFlags>
ParameterDefn StaticParamDefn<Type, index, paramFlags>::value(
  NULL,
  ParameterName<index>::value,
  &Type::instance, paramFlags);

/// -------------------------------------------------------------------
/// Template to generate array of 1 formal argument.

template<class Arg0Type, int paramFlags = 0>
class StaticParamArray1 {
public:
  static ParameterDefn * value[1];
};

template<class Arg0Type, int paramFlags>
ParameterDefn * StaticParamArray1<Arg0Type, paramFlags>::value[1] = {
  &StaticParamDefn<Arg0Type, 0, paramFlags>::value
};

/// -------------------------------------------------------------------
/// Template to generate array of 2 formal arguments.

template<class Arg0Type, class Arg1Type>
class StaticParamArray2 {
public:
  static ParameterDefn * value[2];
};

template<class Arg0Type, class Arg1Type>
ParameterDefn * StaticParamArray2<Arg0Type, Arg1Type>::value[2] = {
  &StaticParamDefn<Arg0Type, 0>::value,
  &StaticParamDefn<Arg1Type, 1>::value,
};

/// -------------------------------------------------------------------
/// Template to generate array of 3 formal arguments.

template<class Arg0Type, class Arg1Type, class Arg2Type>
class StaticParamArray3 {
public:
  static ParameterDefn * value[3];
};

template<class Arg0Type, class Arg1Type, class Arg2Type>
ParameterDefn * StaticParamArray3<Arg0Type, Arg1Type, Arg2Type>::value[3] = {
  &StaticParamDefn<Arg0Type, 0>::value,
  &StaticParamDefn<Arg1Type, 1>::value,
  &StaticParamDefn<Arg2Type, 2>::value,
};

/// -------------------------------------------------------------------
/// Template to function types taking 0 arguments.

template<class ReturnType>
class StaticFnType0 {
public:
  static FunctionType value;
};

template<class ReturnType>
FunctionType StaticFnType0<ReturnType>::value(
  &ReturnType::instance, NULL, 0);

/// -------------------------------------------------------------------
/// Template to function types taking 1 argument.

template<class ReturnType, class Arg0Type, int paramFlags = 0>
class StaticFnType1 {
public:
  static FunctionType value;
};

template<class ReturnType, class Arg0Type, int paramFlags>
FunctionType StaticFnType1<ReturnType, Arg0Type, paramFlags>::value(
  &ReturnType::instance,
  StaticParamArray1<Arg0Type, paramFlags>::value, 1);

/// -------------------------------------------------------------------
/// Template to static function types taking 2 arguments.

template<class ReturnType, class Arg0Type, class Arg1Type>
class StaticFnType2 {
public:
  static FunctionType value;
};

template<class ReturnType, class Arg0Type, class Arg1Type>
FunctionType StaticFnType2<ReturnType, Arg0Type, Arg1Type>::value(
  &ReturnType::instance,
  StaticParamArray2<Arg0Type, Arg1Type>::value, 2);

/// -------------------------------------------------------------------
/// Template to static function types taking 3 arguments.

template<class ReturnType, class Arg0Type, class Arg1Type, class Arg2Type>
class StaticFnType3 {
public:
  static FunctionType value;
};

template<class ReturnType, class Arg0Type, class Arg1Type, class Arg2Type>
FunctionType StaticFnType3<ReturnType, Arg0Type, Arg1Type, Arg2Type>::value(
  &ReturnType::instance,
  StaticParamArray3<Arg0Type, Arg1Type, Arg2Type>::value, 3);

}

#endif
