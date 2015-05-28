// std includes
#include <sstream>
#include <algorithm>
// local includes
#include "cursor.hpp"
#include "cursor-index.hpp"

namespace semantic_code_browser {

namespace frontend {

namespace libclang_utils {

std::string GetStringAndDispose(CXString cxs) {
  std::string ret(clang_getCString(cxs));
  clang_disposeString(cxs);
  return ret;
}

std::string GetCursorSpelling(CXCursor c) {
  return GetStringAndDispose(clang_getCursorSpelling(c));
}
} /* libclang_utils */

namespace entity_traits {

specifier_enum specifier::parse_from_cursor(CXCursor c) {
  CXCursorKind k = clang_getCursorType(c);
  if (k == CXCursor_DeclRefExpr) {

  } else {
    for (CXCursorKind & kind : Kinds<type>) {
      if (kind == k) {
        return type;
      }
    }
    for (CXCursorKind & kind : Kinds<variable>) {
      if (kind == k) {
        return variable;
      }
    }
    for (CXCursorKind & kind : Kinds<function>) {

    }
  }
}

template <>
const CXCursorKind Kinds<type> = {CXCursor_EnumDecl, CXCursor_TypeRef};
template <>
const CXCursorKind Kinds<variable> = {CXCursor_EnumConstantDecl,
                                      CXCursor_VarDecl, CXCursor_ParmDecl,
                                      CXCursor_VariableRef};
template <>
const CXCursorKind Kinds<function> = {CXCursor_FunctionDecl, CXCursor_CallExpr};

// scope::ScopeCursorKinds = {CXCursor_FunctionDecl};

// CXCursor scope::GetEnclosingScope(CXCursor c) {
//   CXCursor tmp(c);
//   while (not clang_equalCursors(tmp, clang_getCursorSemanticParent(tmp))) {
//     tmp = clang_getCursorSemanticParent(tmp);
//     for (auto & scopeKind : ScopeCursorKinds) {
//       if (scopeKind == clang_getCursorKind(tmp)) {
//         return tmp;
//       }
//     }
//   }
//   return tmp;
// }

// scope::scope(CXCursor thisCursor)
//    : mScope(std::string("::") + libclang_utils::GetStringAndDispose(
//                                  GetEnclosingScope(thisCursor))) {
// }

// const std::string & scope::get() {
//   return mScope;
// }
} /* entity_traits */

// cursor::cursor(CXCursor c) : file(libclang_utils::GetStringAndDispose()) {
// }

// /* cursor types */
// const CXCursorKind Cursor::TypeDeclCursorKinds[] = {
//  CXCursor_StructDecl, CXCursor_UnionDecl, CXCursor_EnumDecl};
// const CXCursorKind Cursor::ValDeclCursorKinds[] = {
//  CXCursor_EnumConstantDecl, CXCursor_FunctionDecl, CXCursor_VarDecl,
//  CXCursor_ParmDecl};
// const CXCursorKind Cursor::TypeRefCursorKinds[] = {CXCursor_TypeRef};
// const CXCursorKind Cursor::ValRefCursorKinds[] = {CXCursor_VariableRef,
//                                                   CXCursor_DeclRefExpr};
// const CXCursorKind Cursor::ScopeCursorKinds[] = {CXCursor_FunctionDecl};
} /* frontend */
} /* semantic_code_browser */
