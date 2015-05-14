#include <iostream>             // TODO: REMOVE THIS
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

// TODO: begin the insertion process!
void Cursor::insertCursorIntoMap(std::string s,
                                 Cursor * c __attribute__((unused))) {
  std::cerr << s << std::endl;
}

Cursor::~Cursor() {
  for (auto & dec : decls) {
    if (dec != this) {
      delete dec;
    }
  }
  for (auto & can : canonicals) {
    if (can != this) {
      delete can;
    }
  }
  for (auto & def : defns) {
    if (def != this) {
      delete def;
    }
  }
  for (auto & ref : refs) {
    if (ref != this) {
      delete ref;
    }
  }
}

CXCursor & Cursor::get() {
  return cur;
}

CXTranslationUnit & Cursor::getTranslationUnit() {
  return tu;
}

void Cursor::InsertCursor(CXCursor c, CXTranslationUnit & t) {
  CXString name = clang_getCursorSpelling(c);
  std::string name_str = std::string(clang_getCString(name));
  for (auto & cursorKind : DeclCursorKinds) {
    if (cursorKind == clang_getCursorKind(c)) {
      insertCursorIntoMap(name_str, DeclCursor::MakeCursor(c, t));
    }
  }
  for (auto & cursorKind : RefCursorKinds) {
    if (cursorKind == clang_getCursorKind(c)) {
      insertCursorIntoMap(name_str, RefCursor::MakeCursor(c, t));
    }
  }
  for (auto & cursorKind : AliasCursorKinds) {
    if (cursorKind == clang_getCursorKind(c)) {
      insertCursorIntoMap(name_str, AliasCursor::MakeCursor(c, t));
    }
  }
  clang_disposeString(name);
  // if it's a cursor type we don't care about, ignore
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

// DeclCursor defns
DeclCursor * DeclCursor::MakeCursor(CXCursor c, CXTranslationUnit & t) {
  switch (c.kind) {
  case CXCursor_StructDecl:
    return new StructDeclCursor(c, t);
    break;
  case CXCursor_UnionDecl:
    return new UnionDeclCursor(c, t);
    break;
  case CXCursor_EnumDecl:
    return new EnumDeclCursor(c, t);
    break;
  case CXCursor_FieldDecl:
    return new FieldDeclCursor(c, t);
    break;
  case CXCursor_EnumConstantDecl:
    return new EnumConstantDeclCursor(c, t);
    break;
  case CXCursor_FunctionDecl:
    return new FunctionDeclCursor(c, t);
    break;
  case CXCursor_VarDecl:
    return new VarDeclCursor(c, t);
    break;
  case CXCursor_ParmDecl:
    return new ParmDeclCursor(c, t);
    break;
  case CXCursor_LabelStmt:
    return new LabelStmtCursor(c, t);
    break;
  default:
    throw Cursor::CursorKindNotFoundException(
     "No such DeclCursor found! This is a bug.");
  }
}

// RefCursor defns
RefCursor * RefCursor::MakeCursor(CXCursor c, CXTranslationUnit & t) {
  switch (c.kind) {
  case CXCursor_TypeRef:
    return new TypeRefCursor(c, t);
    break;
  case CXCursor_MemberRef:
    return new MemberRefCursor(c, t);
    break;
  case CXCursor_LabelRef:
    return new LabelRefCursor(c, t);
    break;
  case CXCursor_VariableRef:
    return new VariableRefCursor(c, t);
    break;
  case CXCursor_DeclRefExpr:
    return new DeclRefExprCursor(c, t);
    break;
  case CXCursor_MemberRefExpr:
    return new MemberRefExprCursor(c, t);
    break;
  default:
    throw Cursor::CursorKindNotFoundException(
     "No such RefCursor found! This is a bug.");
  }
}

// AliasCursor defns
AliasCursor * AliasCursor::MakeCursor(CXCursor c, CXTranslationUnit & t) {
  switch (c.kind) {
  case CXCursor_TypedefDecl:
    return new TypedefDeclCursor(c, t);
    break;
  default:
    throw Cursor::CursorKindNotFoundException(
     "No such AliasCursor found! This is a bug.");
  }
}

// stub ctors
DeclCursor::DeclCursor(CXCursor c, CXTranslationUnit & t) : Cursor(c, t) {
}
RefCursor::RefCursor(CXCursor c, CXTranslationUnit & t) : Cursor(c, t) {
}
AliasCursor::AliasCursor(CXCursor c, CXTranslationUnit & t) : Cursor(c, t) {
}
StructDeclCursor::StructDeclCursor(CXCursor c, CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
UnionDeclCursor::UnionDeclCursor(CXCursor c, CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
EnumDeclCursor::EnumDeclCursor(CXCursor c, CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
FieldDeclCursor::FieldDeclCursor(CXCursor c, CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
EnumConstantDeclCursor::EnumConstantDeclCursor(CXCursor c,
                                               CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
FunctionDeclCursor::FunctionDeclCursor(CXCursor c, CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
VarDeclCursor::VarDeclCursor(CXCursor c, CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
ParmDeclCursor::ParmDeclCursor(CXCursor c, CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
LabelStmtCursor::LabelStmtCursor(CXCursor c, CXTranslationUnit & t)
   : DeclCursor(c, t) {
}
TypeRefCursor::TypeRefCursor(CXCursor c, CXTranslationUnit & t)
   : RefCursor(c, t) {
}
MemberRefCursor::MemberRefCursor(CXCursor c, CXTranslationUnit & t)
   : RefCursor(c, t) {
}
LabelRefCursor::LabelRefCursor(CXCursor c, CXTranslationUnit & t)
   : RefCursor(c, t) {
}
VariableRefCursor::VariableRefCursor(CXCursor c, CXTranslationUnit & t)
   : RefCursor(c, t) {
}
DeclRefExprCursor::DeclRefExprCursor(CXCursor c, CXTranslationUnit & t)
   : RefCursor(c, t) {
}
MemberRefExprCursor::MemberRefExprCursor(CXCursor c, CXTranslationUnit & t)
   : RefCursor(c, t) {
}
TypedefDeclCursor::TypedefDeclCursor(CXCursor c, CXTranslationUnit & t)
   : AliasCursor(c, t) {
}
}
