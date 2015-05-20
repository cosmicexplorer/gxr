// std includes
#include <sstream>
#include <algorithm>
// local includes
#include "cursor.hpp"
#include "cursor-index.hpp"

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
const CXCursorKind Cursor::ScopeCursorKinds[] = {CXCursor_FunctionDecl};

/* static helper methods */
std::string Cursor::GetStringFromCXString(CXString cxs) {
  std::string ret(clang_getCString(cxs));
  clang_disposeString(cxs);
  return ret;
}

/*
   OPTIMIZATION:
   factor GetFileName and GetOffset into same method and perform initialization
   in ctor. also do this for the initialization of begin and end.
*/
std::string Cursor::GetFileName(CXSourceLocation cxsl) {
  CXFile file;
  unsigned int line, col, offset;
  clang_getSpellingLocation(cxsl, &file, &line, &col, &offset);
  return GetStringFromCXString(clang_getFileName(file));
}

unsigned int Cursor::GetOffset(CXSourceLocation cxsl) {
  CXFile file;
  unsigned int line, col, offset;
  clang_getSpellingLocation(cxsl, &file, &line, &col, &offset);
  return offset;
}

bool Cursor::IsDefinition(CXCursor c) {
  return clang_isCursorDefinition(c);
}

size_t Cursor::AnonymousCounter = 0;

std::string Cursor::GetName(CXCursor c) {
  std::string res(Cursor::GetStringFromCXString(clang_getCursorSpelling(
   clang_isReference(clang_getCursorKind(c)) ? clang_getCursorReferenced(c)
                                             : c)));
  if (res != "") {
    return res;
  } else {
    return "<ANON>" + std::to_string(AnonymousCounter++);
  }
}

std::string Cursor::GetUSR(CXCursor c) {
  std::string res;
  (Cursor::GetStringFromCXString(clang_getCursorUSR(
   clang_isReference(clang_getCursorKind(c)) ? clang_getCursorReferenced(c)
                                             : c)));
  if (res != "") {
    return res;
  } else {
    std::list<CXCursor> enclosingScope(Cursor::GetEnclosingScope(c));
    std::list<std::string> scopeStrings;
    std::transform(enclosingScope.begin(), enclosingScope.end(),
                   std::back_inserter(scopeStrings), [](CXCursor _c) {
                     CXString cxs(clang_getCursorSpelling(_c));
                     std::string s(clang_getCString(cxs));
                     clang_disposeString(cxs);
                     return s;
                   });
    std::string retstr;
    for (auto & scopestr : scopeStrings) {
      retstr += scopestr + "::";
    }
    // if we get here, we will have already incremented AnonymousCounter in
    // GetName
    return retstr + "<ANON>" + std::to_string(AnonymousCounter++);
  }
}

std::list<CXCursor> Cursor::GetEnclosingScope(CXCursor c) {
  CXCursor tmp(c);
  std::list<CXCursor> ret;
  bool found;
  while (not clang_equalCursors(tmp, clang_getCursorSemanticParent(tmp))) {
    found = false;
    tmp = clang_getCursorSemanticParent(tmp);
    for (auto & scopeKind : Cursor::ScopeCursorKinds) {
      if (scopeKind == clang_getCursorKind(tmp)) {
        ret.push_back(tmp);
        break;
      }
    }
  }
  return ret;
}

std::string Cursor::ConvertSpecifier(Specifier s) {
  switch (s) {
  case Type:
    return "Type";
    break;
  case Value:
    return "Value";
    break;
  default:
    return "Unknown";
    break;
  }
}

/* pointless virtual dtor */
Cursor::~Cursor() {
}

/* entry point to cursor class */
Cursor * Cursor::MakeCursor(CXCursor c) {
  for (auto & typeDeclCursorKind : TypeDeclCursorKinds) {
    // TODO: use clang_isReference and friends!
    // if (clang_isReference(c) and clang_)
    if (typeDeclCursorKind == clang_getCursorKind(c)) {
      if (IsDefinition(c)) {
        return new DefnCursor<Type>(c);
      } else {
        return new DeclCursor<Type>(c);
      }
    }
  }
  for (auto & valDeclCursorKind : ValDeclCursorKinds) {
    if (valDeclCursorKind == clang_getCursorKind(c)) {
      if (IsDefinition(c)) {
        return new DefnCursor<Value>(c);
      } else {
        return new DeclCursor<Value>(c);
      }
    }
  }
  for (auto & typeRefCursorKind : TypeRefCursorKinds) {
    if (typeRefCursorKind == clang_getCursorKind(c)) {
      return new RefCursor<Type>(c);
    }
  }
  for (auto & valRefCursorKind : ValRefCursorKinds) {
    if (valRefCursorKind == clang_getCursorKind(c)) {
      return new RefCursor<Value>(c);
    }
  }
  // if we don't care about the kind of cursor this is pointing to
  return nullptr;
}

/* constructor */
Cursor::Cursor(CXCursor c)
   : mCursor(c),
     mCursorKind(clang_getCursorKind(c)),
     mBegin(clang_getRangeStart(clang_getCursorExtent(c))),
     mEnd(clang_getRangeEnd(clang_getCursorExtent(c))),
     mFile(Cursor::GetFileName(mBegin)),
     mOffset(Cursor::GetOffset(mBegin)),
     mName(Cursor::GetName(c)),
     mUSR(Cursor::GetUSR(c)) {
}

/* simple accessors */
const CXCursor & Cursor::get() const {
  return mCursor;
}

const CXCursorKind & Cursor::getKind() const {
  return mCursorKind;
}

const CXSourceLocation & Cursor::getBegin() const {
  return mBegin;
}

const CXSourceLocation & Cursor::getEnd() const {
  return mEnd;
}

const std::string & Cursor::getFile() const {
  return mFile;
}

const unsigned int & Cursor::getOffset() const {
  return mOffset;
}

const std::string & Cursor::getName() const {
  return mName;
}

const std::string & Cursor::getUSR() const {
  return mUSR;
}

std::string Cursor::getDerivedType() const {
  return "Cursor";
}

std::string Cursor::toString() const {
  std::stringstream ss;
  CXString cxs(clang_getCursorKindSpelling(getKind()));
  std::string kind(clang_getCString(cxs));
  clang_disposeString(cxs);
  ss << "spec: " << Cursor::ConvertSpecifier(getSpecifier())
     << ", kind: " << kind << ", file: " << getFile()
     << ", derivedType: " << getDerivedType() << ", offset: " << getOffset()
     << ", name: " << getName() << ", USR: " << getUSR();
  return ss.str();
}

bool Cursor::operator==(Cursor & rhs) const {
  return getUSR() == rhs.getUSR() and getFile() == rhs.getFile() and
         getOffset() == rhs.getOffset() and getKind() == rhs.getKind();
}

/* ctors/dtors for derived classes */
template <Specifier S>
EntityCursor<S>::EntityCursor(CXCursor c)
   : Cursor(c) {
}

template <Specifier S>
EntityCursor<S>::~EntityCursor() {
}

/* mild form of introspection */
template <Specifier S>
Specifier EntityCursor<S>::getSpecifier() const {
  return S;
}

template <Specifier S>
std::string EntityCursor<S>::getDerivedType() const {
  return "EntityCursor";
}

template <Specifier S>
DeclCursor<S>::DeclCursor(CXCursor c)
   : EntityCursor<S>(c) {
}

template <Specifier S>
DeclCursor<S>::~DeclCursor() {
}

template <Specifier S>
std::string DeclCursor<S>::getDerivedType() const {
  return "DeclCursor";
}

template <Specifier S>
bool DeclCursor<S>::accept(EntityIndex * ei) {
  return ei->addDecl(this);
}

template <Specifier S>
RefCursor<S>::RefCursor(CXCursor c)
   : EntityCursor<S>(c) {
}

template <Specifier S>
RefCursor<S>::~RefCursor() {
}

template <Specifier S>
std::string RefCursor<S>::getDerivedType() const {
  return "RefCursor";
}

template <Specifier S>
bool RefCursor<S>::accept(EntityIndex * ei) {
  return ei->addRef(this);
}

template <Specifier S>
DefnCursor<S>::DefnCursor(CXCursor c)
   : DeclCursor<S>(c) {
}

template <Specifier S>
DefnCursor<S>::~DefnCursor() {
}

template <Specifier S>
std::string DefnCursor<S>::getDerivedType() const {
  return "DefnCursor";
}

template <Specifier S>
bool DefnCursor<S>::accept(EntityIndex * ei) {
  return ei->addDefn(this);
}
}
