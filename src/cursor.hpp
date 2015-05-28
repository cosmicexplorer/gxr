#ifndef CURSOR_HPP
#define CURSOR_HPP

// std includes
#include <list>
// external includes
#include <clang-c/Index.h>
// local includes
#include "utilities.hpp"

namespace semantic_code_browser {

namespace frontend {

namespace libclang_utils {

// disposes of input CXString; don't attempt to use or dispose the input after
// calling this
std::string GetStringAndDispose(CXString);

std::string GetCursorSpelling(CXCursor);
} /* libclang_utils */

namespace entity_traits {

MAKE_ENUM_STRUCT(specifier, variable, function, type);

template <specifier S>
const CXCursorKind Kinds[];

// class scope {
//  private:
//   std::string mScope;

//   static CXCursorKind ScopeCursorKinds[];

//   static CXCursor GetEnclosingScope(CXCursor);

//  public:
//   scope(CXCursor);
//   // deserialization
//   scope(std::string);
//   scope(std::list<std::string>);

//   // serialization
//   std::string toString();
//   std::list<std::string> toList();
// }; /* scope */
} /* entity_traits */

// namespace cursor_traits {
// enum class type { declaration, reference, definition, call };
// enum class language { c };
// } /* cursor_traits */

// struct cursor {
//   // index contents
//   std::string file;
//   unsigned long long offset;
//   unsigned long long line;
//   unsigned long long col;
//   cursor_traits::type cursorType;
//   entity_traits::specifier entitySpec;
//   std::string type;
//   cursor_traits::language language;
//   std::string name;
//   entity_traits::scope scope;

//   // ctors
//   cursor(CXCursor);
//   cursor(std::string);

//   // serialization (csv)
//   std::string toString();
// }; /* cursor */
} /* frontend */
} /* semantic_code_browser */
#endif /* CURSOR_HPP */
