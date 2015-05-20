#include "cursor.hpp"

namespace semantic_code_browser {
const CXCursorKind Cursor::TypeDeclCursorKinds[] = {
 CXCursor_StructDecl, CXCursor_UnionDecl, CXCursor_EnumDecl};
const CXCursorKind Cursor::ValDeclCursorKinds[] = {
 CXCursor_EnumConstantDecl, CXCursor_FunctionDecl, CXCursor_VarDecl,
 CXCursor_ParmDecl};
const CXCursorKind Cursor::TypeRefCursorKinds[] = {CXCursor_TypeRef};
const CXCursorKind Cursor::ValRefCursorKinds[] = {CXCursor_VariableRef,
                                                  CXCursor_DeclRefExpr};

Cursor * Cursor::MakeCursor(CXCursor c, CXTranslationUnit & tu) {
  for (auto & typeDeclCursorKind : TypeDeclCursorKinds) {
    if (typeDeclCursorKind == clang_getCursorKind(c)) {
      return new DeclCursor<Type>(c, tu);
    }
  }
  for (auto & valDeclCursorKind : ValDeclCursorKinds) {
    if (valDeclCursorKind == clang_getCursorKind(c)) {
      return new DeclCursor<Value>(c, tu);
    }
  }
  for (auto & typeRefCursorKind : TypeRefCursorKinds) {
    if (typeRefCursorKind == clang_getCursorKind(c)) {
      return new RefCursor<Type>(c, tu);
    }
  }
  for (auto & valRefCursorKind : ValRefCursorKinds) {
    if (valRefCursorKind == clang_getCursorKind(c)) {
      return new RefCursor<Value>(c, tu);
    }
  }
  // if we don't care about the kind of cursor this is pointing to
  return nullptr;
}

Cursor::Cursor(CXCursor c, CXTranslationUnit & tu) : mCursor(c), mTU(tu) {
}

template <Specifier S>
EntityCursor<S>::EntityCursor(CXCursor c, CXTranslationUnit & tu)
   : Cursor(c, tu) {
}

template <Specifier S>
DeclCursor<S>::DeclCursor(CXCursor c, CXTranslationUnit & tu)
   : EntityCursor<S>(c, tu) {
}

template <Specifier S>
RefCursor<S>::RefCursor(CXCursor c, CXTranslationUnit & tu)
   : EntityCursor<S>(c, tu) {
}
}
