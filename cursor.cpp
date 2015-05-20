#include "cursor.hpp"

namespace semantic_code_browser {
/* cursor types */
const CXCursorKind Cursor::TypeDeclCursorKinds[] = {
 CXCursor_StructDecl, CXCursor_UnionDecl, CXCursor_EnumDecl};
const CXCursorKind Cursor::ValDeclCursorKinds[] = {
 CXCursor_EnumConstantDecl, CXCursor_FunctionDecl, CXCursor_VarDecl,
 CXCursor_ParmDecl};
const CXCursorKind Cursor::TypeRefCursorKinds[] = {CXCursor_TypeRef};
const CXCursorKind Cursor::ValRefCursorKinds[] = {CXCursor_VariableRef,
                                                  CXCursor_DeclRefExpr};

/* static helper methods */
std::string Cursor::GetStringFromCXString(CXString cxs) {
  std::string ret(clang_getCString(cxs));
  clang_disposeString(cxs);
  return ret;
}

/* pointless virtual dtor */
Cursor::~Cursor() {
}

/* entry point to cursor class */
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

/* constructor */
Cursor::Cursor(CXCursor c, CXTranslationUnit & tu)
   : mCursor(c),
     mBegin(clang_getRangeStart(clang_getCursorExtent(c))),
     mEnd(clang_getRangeEnd(clang_getCursorExtent(c))),
     mTU(tu),
     mName(Cursor::GetStringFromCXString(clang_getCursorSpelling(c))) {
}

/* simple accessors */
const CXCursor & Cursor::get() {
  return mCursor;
}

const CXTranslationUnit & Cursor::getTranslationUnit() {
  return mTU;
}

const CXSourceLocation & Cursor::getBegin() {
  return mBegin;
}

const CXSourceLocation & Cursor::getEnd() {
  return mEnd;
}

/* ctors/dtors for derived classes */
template <Specifier S>
EntityCursor<S>::EntityCursor(CXCursor c, CXTranslationUnit & tu)
   : Cursor(c, tu) {
}

template <Specifier S>
EntityCursor<S>::~EntityCursor() {
}

template <Specifier S>
DeclCursor<S>::DeclCursor(CXCursor c, CXTranslationUnit & tu)
   : EntityCursor<S>(c, tu) {
}

template <Specifier S>
DeclCursor<S>::~DeclCursor() {
}

template <Specifier S>
RefCursor<S>::RefCursor(CXCursor c, CXTranslationUnit & tu)
   : EntityCursor<S>(c, tu) {
}

template <Specifier S>
RefCursor<S>::~RefCursor() {
}
}
