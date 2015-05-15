#include <iostream> // TODO: REMOVE THIS
#include <algorithm>
#include "cursor.h"

namespace semantic_code_browser {

std::unordered_map<std::string, Cursor *> Cursor::nodes =
 std::unordered_map<std::string, Cursor *>();

const CXCursorKind Cursor::DeclCursorKinds[] = {
 CXCursor_StructDecl, CXCursor_UnionDecl,        CXCursor_EnumDecl,
 CXCursor_FieldDecl,  CXCursor_EnumConstantDecl, CXCursor_FunctionDecl,
 CXCursor_VarDecl,    CXCursor_ParmDecl,         CXCursor_LabelStmt};
const CXCursorKind Cursor::RefCursorKinds[] = {
 CXCursor_TypeRef,     CXCursor_MemberRef,   CXCursor_LabelRef,
 CXCursor_VariableRef, CXCursor_DeclRefExpr, CXCursor_MemberRefExpr};
const CXCursorKind Cursor::AliasCursorKinds[] = {CXCursor_TypedefDecl};

// Cursor defns
Cursor::Cursor(CXCursor c, CXTranslationUnit & t) : cur(c), tu(t) {
}

Cursor * Cursor::findCanonical() {
}

Cursor * Cursor::findDefinition() {
}

// TODO: begin the insertion process!
void Cursor::insertCursorIntoMap(std::string s,
                                 Cursor * c __attribute__((unused))) {
  std::cerr << s << std::endl;
}

Cursor::~Cursor() {
  // only delete things if they are the canonical cursor; otherwise preserve all
  // data (Cursor will only be allocated with new and delete will only be called
  // outside of this destructor on the canonical Cursor)
  if (this == canonical) {
    for (auto & dec : decls) {
      if (dec != this) {
        delete dec;
      }
    }
    if (def != this) {
      delete def;
    }
    for (auto & ref : refs) {
      if (ref != this) {
        delete ref;
      }
    }
  }
}

CXCursor & Cursor::get() {
  return cur;
}

CXTranslationUnit & Cursor::getTranslationUnit() {
  return tu;
}

const Cursor * Cursor::MakeCursor(CXCursor c, CXTranslationUnit & t) {
  for (auto & declCursorKind : DeclCursorKinds) {
    if (declCursorKind == clang_getCursorKind(c)) {
      return new DeclCursor(c, t);
    }
  }
  for (auto & refCursorKind : RefCursorKinds) {
    if (refCursorKind == clang_getCursorKind(c)) {
      return new RefCursor(c, t);
    }
  }
  for (auto & aliasCursorKind : AliasCursorKinds) {
    if (aliasCursorKind == clang_getCursorKind(c)) {
      return new AliasCursor(c, t);
    }
  }
  // if it's a cursor type we don't care about, ignore
  return nullptr;
}

void Cursor::ForEach(std::function<void(std::string, const Cursor *) > f) {
  for (auto & cur : nodes) {
    f(cur.first, cur.second);
  }
}

void Cursor::FreeAll() {
  for (auto & cur : nodes) {
    delete cur.second;
  }
  nodes.clear();
}

bool Cursor::operator==(Cursor & rhs) {
  // TODO: how does this work with multiple files (translation units)? does
  // clang_equalCursors act the same? or do we have to go through some bs?
  return clang_equalCursors(cur, rhs.cur);
}
bool Cursor::operator!=(Cursor & rhs) {
  return not(rhs == *this);
}

std::list<Cursor *> Cursor::getDecls() {
  return decls;
}

std::list<Cursor *> Cursor::getCanonicals() {
  return canonicals;
}

std::list<Cursor *> Cursor::getDefinitions() {
  return defns;
}

std::list<Cursor *> Cursor::getRefs() {
  return refs;
}
