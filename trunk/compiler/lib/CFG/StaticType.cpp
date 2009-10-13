/* ================================================================ *
    TART - A Sweet Programming Language.
 * ================================================================ */

#include "tart/Common/SourceLocation.h"
#include "tart/CFG/StaticType.h"
#include "tart/Objects/Builtins.h"

namespace tart {

template<> Type & StaticType<TypeId_Bool>::value = BoolType::instance;
template<> Type & StaticType<TypeId_Void>::value = VoidType::instance;
template<> Type & StaticType<TypeId_Char>::value = CharType::instance;
template<> Type & StaticType<TypeId_SInt8>::value = ByteType::instance;
template<> Type & StaticType<TypeId_SInt16>::value = ShortType::instance;
template<> Type & StaticType<TypeId_SInt32>::value = IntType::instance;
template<> Type & StaticType<TypeId_SInt64>::value = LongType::instance;
template<> Type & StaticType<TypeId_UInt8>::value = UByteType::instance;
template<> Type & StaticType<TypeId_UInt16>::value = UShortType::instance;
template<> Type & StaticType<TypeId_UInt32>::value = UIntType::instance;
template<> Type & StaticType<TypeId_UInt64>::value = ULongType::instance;
template<> Type & StaticType<TypeId_Float>::value = FloatType::instance;
template<> Type & StaticType<TypeId_Double>::value = DoubleType::instance;
template<> Type & StaticType<TypeId_UnsizedInt>::value = UnsizedIntType::instance;

template<> Type & StaticType<TypeId_String>::value = Builtins::typeAliasString;

template<> const char ParameterName<0>::value[3] = { 'a', '0', 0 };
template<> const char ParameterName<1>::value[3] = { 'a', '1', 0 };
template<> const char ParameterName<2>::value[3] = { 'a', '2', 0 };
template<> const char ParameterName<3>::value[3] = { 'a', '3', 0 };

}
