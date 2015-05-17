#ifndef CURSOR_H
#define CURSOR_H

// std includes
#include <stdexcept>
// external includes
#include <clang-c/Index.h>

namespace semantic_code_browser {

/*

Cursor <- EntityCursor<Type>  <- AliasableCursor<Type> <- (Decl|Ref)Cursor<Type>
       <- EntityCursor<Value> <- (Decl|Ref)Cursor<Value>
       <- AliasCursor<Type>

DeclCursor<Type|Value> <- CanonicalDeclCursor<Type|Value>
                       <- DefinitionDeclCursor<Type|Value>

 */

enum Specifier { Type, Value };

class Cursor;

template <Specifier S>
class EntityCursor;

template <Specifier S>
class DeclCursor;
template <Specifier S>
class RefCursor;

template <Specifer S>
class CanonicalDeclCursor;
template <Specifier S>
class DefinitionDeclCursor;

template <Specifier S>
class AliasCursor;

// root of inheritance hierarchy
class Cursor {
 protected:
  const CXCursor cur;
  const CXTranslationUnit & tu;

  Cursor(const CXCursor, const CXTranslationUnit &);

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
  static void
   ForEachEntity(std::function<void(const std::string, const Cursor *) >);
  static void FreeAll();

  bool operator==(const Cursor &) const;
  bool operator!=(const Cursor &) const;
};

// level 1 of inheritance hierarchy
template <Specifier S>
class EntityCursor : public Cursor {
 private:
  const CanonicalDeclCursor * canonical;
  const DefinitionDeclCursor * defn;
  std::list<const DeclCursor *> decls;
  std::list<const RefCursor *> refs;

  static std::unordered_map<const std::string, const CanonicalDeclCursor *>
   nodes;

 public:
  std::list<const DeclCursor *> getDecls() const;
  const CanonicalDeclCursor * getCanonical() const;
  const DefinitionDeclCursor * getDefinition() const;
  std::list<const RefCursor *> getRefs() const;
};

template <Specifier S>
class AliasCursor : public Cursor {
  static_assert(S == Type, "Alias only has meaning for types.");

 protected:
  AliasCursor(CXCursor, CXTranslationUnit &);
  std::pair<Cursor *, Cursor *> typeAlias;
};

// level 2 of inheritance hierarchy
template <Specifier S>
class DeclCursor : public EntityCursor<S> {
 protected:
  DeclCursor(CXCursor, CXTranslationUnit &);
};

template <Specifier S>
class RefCursor : public EntityCursor<S> {
 protected:
  RefCursor(CXCursor, CXTranslationUnit &);
};

// level 3 of inheritance hierarchy
template <Specifier S>
class CanonicalDeclCursor : public DeclCursor<S> {
 protected:
  CanonicalDeclCursor(CXCursor, CXTranslationUnit &);
}

template <Specifier S>
class DefinitionDeclCursor : public DeclCursor<S> {
 protected:
  CanonicalDeclCursor(CXCursor, CXTranslationUnit &);
}
}
#endif /* CURSOR_H */
