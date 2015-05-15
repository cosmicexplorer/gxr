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

class Cursor;
class DeclCursor;
class RefCursor;
class CanonicalDeclCursor;
class DefinitionDeclCursor;
class AliasCursor;

class Type {
 protected:
  std::list<AliasCursor *> aliases;
};
class Value {};

// root of inheritance hierarchy
class Cursor {
 protected:
  const CXCursor cur;
  const CXTranslationUnit & tu;

  const CanonicalDeclCursor * canonical;
  const DefinitionDeclCursor * defn;
  std::list<const DeclCursor *> decls;
  std::list<const RefCursor *> refs;

  static std::unordered_map<const std::string, const CanonicalDeclCursor *>
   nodes;

  Cursor(const CXCursor, const CXTranslationUnit &);

  const CanonicalDeclCursor * findCanonical();
  const DefinitionDeclCursor * findDefinition();

  // CXCursorKind corresponding to each top-level Cursor subclass
  // declarations of variables, types, functions, fields, labels
  static const CXCursorKind DeclCursorKinds[];
  // references to above
  static const CXCursorKind RefCursorKinds[];
  // typedef
  static const CXCursorKind AliasCursorKinds[];

 public:
  virtual ~Cursor();

  const CXCursor & get() const;

  const CXTranslationUnit & getTranslationUnit() const;

  class CursorKindNotFoundException : public std::runtime_error {
   public:
    CursorKindNotFoundException(std::string str) : std::runtime_error(str) {
    }
  };

  static const Cursor * MakeCursor(const CXCursor, const CXTranslationUnit &);
  static void ForEach(std::function<void(const std::string, const Cursor *) >);
  static void FreeAll();

  bool operator==(const Cursor &) const;
  bool operator!=(const Cursor &) const;

  std::list<const DeclCursor *> getDecls() const;
  const CanonicalDeclCursor * getCanonical() const;
  const DefinitionDeclCursor * getDefinition() const;
  std::list<const RefCursor *> getRefs() const;
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
  std::pair<Cursor *, Cursor *> typeAlias;
};

// level 2 of inheritance hierarchy
class CanonicalDeclCursor : public DeclCursor {
 protected:
  CanonicalDeclCursor(CXCursor, CXTranslationUnit &);
}

class DefinitionDeclCursor : public DeclCursor {
 protected:
  CanonicalDeclCursor(CXCursor, CXTranslationUnit &);
}
}
#endif /* CURSOR_H */
