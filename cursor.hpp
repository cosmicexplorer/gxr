#ifndef CURSOR_H
#define CURSOR_H

// std includes
#include <tuple>
// external includes
#include <clang-c/Index.h>

namespace semantic_code_browser {
class Cursor {
 protected:
  /* types of cursors used in multiplexing Cursor derived class in MakeCursor */
  static const CXCursorKind TypeDeclCursorKinds[];
  static const CXCursorKind ValDeclCursorKinds[];
  static const CXCursorKind TypeRefCursorKinds[];
  static const CXCursorKind ValRefCursorKinds[];

  /* static helper methods */
  /*
    THIS DISPOSES OF THE CXSTRING AND WILL CAUSE MEMORY ERRORS IF YOU ATTEMPT
    TO USE THE CXSTRING AFTER USING IT IN THIS FUNCTION
  */
  static std::string GetStringFromCXString(CXString);

  /* constructor and members */
  Cursor(CXCursor, CXTranslationUnit &);

  const CXCursor mCursor;
  const CXSourceLocation mBegin;
  const CXSourceLocation mEnd;
  const CXTranslationUnit & mTU;
  const std::string mName;

 public:
  virtual ~Cursor();

  /* entry point to cursor class */
  static Cursor * MakeCursor(CXCursor, CXTranslationUnit &);

  /* simple accessors */
  const CXCursor & get();
  const CXTranslationUnit & getTranslationUnit();
  const CXSourceLocation & getBegin();
  const CXSourceLocation & getEnd();
  const std::string & getName();
};

/* cursor variants specification */
// this doesn't matter now, but it will when we introduce aliases, and
// especially when we introduce namespaces
enum Specifier { Type, Value };

/* inheritance hierarchy */
template <Specifier S>
class EntityCursor : public Cursor {
 protected:
  EntityCursor(CXCursor, CXTranslationUnit &);

 public:
  virtual ~EntityCursor();
};

template <Specifier S>
class DeclCursor : public EntityCursor<S> {
 public:
  DeclCursor(CXCursor, CXTranslationUnit &);
  ~DeclCursor();
};

template <Specifier S>
class RefCursor : public EntityCursor<S> {
 public:
  RefCursor(CXCursor, CXTranslationUnit &);
  ~RefCursor();
};
}
#endif /* CURSOR_H */
