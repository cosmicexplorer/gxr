#ifndef CURSOR_H
#define CURSOR_H

// std includes
#include <list>
#include <stdexcept>
#include <unordered_map>
#include <functional>
// external includes
#include <clang-c/Index.h>

namespace semantic_code_browser {
// root of inheritance hierarchy
class Cursor {
 protected:
  CXCursor cur;
  CXTranslationUnit & tu;

  std::list<Cursor *> decls;
  std::list<Cursor *> canonicals;
  std::list<Cursor *> defns;
  std::list<Cursor *> refs;

  Cursor(CXCursor, CXTranslationUnit &);

  // CXCursorKind corresponding to each top-level Cursor subclass
  // declarations of variables, types, functions, fields, labels
  static const CXCursorKind DeclCursorKinds[];
  // references to above
  static const CXCursorKind RefCursorKinds[];
  // typedef
  static const CXCursorKind AliasCursorKinds[];

  static std::unordered_map<std::string, Cursor *> nodes;

  static void insertCursorIntoMap(std::string, Cursor *);

 public:
  virtual ~Cursor();

  CXCursor & get();

  CXTranslationUnit & getTranslationUnit();

  class CursorKindNotFoundException : public std::runtime_error {
   public:
    CursorKindNotFoundException(std::string str) : std::runtime_error(str) {
    }
  };

  static void InsertCursor(CXCursor, CXTranslationUnit &);
  static void ForEach(std::function<void(std::string, const Cursor *) >);
  static void FreeAll();

  bool operator==(Cursor &);
  bool operator!=(Cursor &);

  std::list<Cursor *> getDecls();
  std::list<Cursor *> getCanonicals();
  std::list<Cursor *> getDefinitions();
  std::list<Cursor *> getRefs();
  // TODO: declare all appropriate accessors here
};

// level 1 of inheritance hierarchy
class DeclCursor : public Cursor {
 protected:
  DeclCursor(CXCursor, CXTranslationUnit &);

 public:
  static DeclCursor * MakeCursor(CXCursor, CXTranslationUnit &);
};

class RefCursor : public Cursor {
 protected:
  RefCursor(CXCursor, CXTranslationUnit &);

 public:
  static RefCursor * MakeCursor(CXCursor, CXTranslationUnit &);
};

class AliasCursor : public Cursor {
 protected:
  AliasCursor(CXCursor, CXTranslationUnit &);

 public:
  static AliasCursor * MakeCursor(CXCursor, CXTranslationUnit &);
};

// level 2 of inheritance hierarchy
// inherit from DeclCursor
class StructDeclCursor : public DeclCursor {
 public:
  StructDeclCursor(CXCursor, CXTranslationUnit &);
};
class UnionDeclCursor : public DeclCursor {
 public:
  UnionDeclCursor(CXCursor, CXTranslationUnit &);
};
class EnumDeclCursor : public DeclCursor {
 public:
  EnumDeclCursor(CXCursor, CXTranslationUnit &);
};
class FieldDeclCursor : public DeclCursor {
 public:
  FieldDeclCursor(CXCursor, CXTranslationUnit &);
};
class EnumConstantDeclCursor : public DeclCursor {
 public:
  EnumConstantDeclCursor(CXCursor, CXTranslationUnit &);
};
class FunctionDeclCursor : public DeclCursor {
 public:
  FunctionDeclCursor(CXCursor, CXTranslationUnit &);
};
class VarDeclCursor : public DeclCursor {
 public:
  VarDeclCursor(CXCursor, CXTranslationUnit &);
};
class ParmDeclCursor : public DeclCursor {
 public:
  ParmDeclCursor(CXCursor, CXTranslationUnit &);
};
class LabelStmtCursor : public DeclCursor {
 public:
  LabelStmtCursor(CXCursor, CXTranslationUnit &);
};

// inherit from RefCursor
class TypeRefCursor : public RefCursor {
 public:
  TypeRefCursor(CXCursor, CXTranslationUnit &);
};
class MemberRefCursor : public RefCursor {
 public:
  MemberRefCursor(CXCursor, CXTranslationUnit &);
};
class LabelRefCursor : public RefCursor {
 public:
  LabelRefCursor(CXCursor, CXTranslationUnit &);
};
class VariableRefCursor : public RefCursor {
 public:
  VariableRefCursor(CXCursor, CXTranslationUnit &);
};
class DeclRefExprCursor : public RefCursor {
 public:
  DeclRefExprCursor(CXCursor, CXTranslationUnit &);
};
class MemberRefExprCursor : public RefCursor {
 public:
  MemberRefExprCursor(CXCursor, CXTranslationUnit &);
};

// inherit from AliasCursor
class TypedefDeclCursor : public AliasCursor {
 public:
  TypedefDeclCursor(CXCursor, CXTranslationUnit &);
};
}

#endif /* CURSOR_H */
