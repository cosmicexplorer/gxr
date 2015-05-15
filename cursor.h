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

  Cursor * canonical;
  Cursor * defn;

  std::list<Cursor *> decls;
  std::list<Cursor *> refs;

  Cursor(CXCursor, CXTranslationUnit &);

  Cursor * findCanonical();
  Cursor * findDefinition();

  // CXCursorKind corresponding to each top-level Cursor subclass
  // declarations of variables, types, functions, fields, labels
  static const CXCursorKind DeclCursorKinds[];
  // references to above
  static const CXCursorKind RefCursorKinds[];
  // typedef
  static const CXCursorKind AliasCursorKinds[];

  static std::unordered_map<std::string, Cursor *> nodes;

 public:
  virtual ~Cursor();

  CXCursor & get();

  CXTranslationUnit & getTranslationUnit();

  class CursorKindNotFoundException : public std::runtime_error {
   public:
    CursorKindNotFoundException(std::string str) : std::runtime_error(str) {
    }
  };

  static const Cursor * MakeCursor(CXCursor, CXTranslationUnit &);
  static void ForEach(std::function<void(std::string, const Cursor *) >);
  static void FreeAll();

  bool operator==(Cursor &);
  bool operator!=(Cursor &);

  std::list<Cursor *> getDecls();
  Cursor * getCanonical();
  Cursor * getDefinition();
  std::list<Cursor *> getRefs();
  // TODO: declare all appropriate accessors here
};

// level 1 of inheritance hierarchy
class DeclCursor : public Cursor {
 protected:
  DeclCursor(CXCursor, CXTranslationUnit &);
};

class RefCursor : public Cursor {
 protected:
  RefCursor(CXCursor, CXTranslationUnit &);
};

class AliasCursor : public Cursor {
 protected:
  AliasCursor(CXCursor, CXTranslationUnit &);
};

#endif /* CURSOR_H */
