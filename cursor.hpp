#ifndef CURSOR_H
#define CURSOR_H

// std includes
#include <stdexcept>
// external includes
#include <clang-c/Index.h>

namespace semantic_code_browser {
class Cursor {
 protected:
  Cursor(CXCursor, CXTranslationUnit &);

  CXCursor mCursor;
  CXTranslationUnit & mTU;

  static const CXCursorKind TypeDeclCursorKinds[];
  static const CXCursorKind ValDeclCursorKinds[];
  static const CXCursorKind TypeRefCursorKinds[];
  static const CXCursorKind ValRefCursorKinds[];

 public:
  static Cursor * MakeCursor(CXCursor, CXTranslationUnit &);
  // add some pure virtual methods here maybe

  CXCursor & get();
  CXTranslationUnit & getTranslationUnit();
};

enum Specifier { Type, Value };

template <Specifier S>
class EntityCursor : public Cursor {
 protected:
  EntityCursor(CXCursor, CXTranslationUnit &);
  // add some pure virtual methods here maybe
};

template <Specifier S>
class DeclCursor : public EntityCursor<S> {
 public:
  DeclCursor(CXCursor, CXTranslationUnit &);
};

template <Specifier S>
class RefCursor : public EntityCursor<S> {
 public:
  RefCursor(CXCursor, CXTranslationUnit &);
};
}
#endif /* CURSOR_H */
