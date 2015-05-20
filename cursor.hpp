#ifndef CURSOR_HPP
#define CURSOR_HPP

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
  static std::string GetFileName(CXSourceLocation);
  static unsigned int GetOffset(CXSourceLocation);
  static bool IsDefinition(CXCursor);

  /* constructor and members */
  Cursor(CXCursor, CXTranslationUnit &);

  const CXCursor mCursor;
  const CXSourceLocation mBegin;
  const CXSourceLocation mEnd;
  const std::string mFile;
  const unsigned int mOffset;
  const CXTranslationUnit & mTU;
  const std::string mName;
  const std::string mUSR;

 public:
  virtual ~Cursor();

  /* entry point to cursor class */
  static Cursor * MakeCursor(CXCursor, CXTranslationUnit &);

  /* simple accessors */
  const CXCursor & get() const;
  const CXTranslationUnit & getTranslationUnit() const;
  const CXSourceLocation & getBegin() const;
  const CXSourceLocation & getEnd() const;
  const std::string & getFile() const;
  const unsigned int & getOffset() const;
  const std::string & getName() const;
  const std::string & getUSR() const;

  /* more complex processing */
  /*
    One might naively assume that this calls clang_cursorEquals under the
    covers. In fact, it does not. This
  */
  bool operator== (Cursor &) const;

  /*
    A USR is what will identify our entity in the given compilation target (set
    of source files which will all be linked together). This is useful
    internally, but for presentation purposes, we wish to display the fully
    qualified name of the entity, and not in the mangled form that the USR
    presents. This is computationally complex (slightly), and will hopefully be
    called only once per entity, so it is not backed by a data member (which
    would require construction of the fully qualified name on each cursor
    instantiation).
  */
  std::string getFullyQualifiedName() const;
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

template <Specifier S>
class DefnCursor : public DeclCursor<S> {
 public:
  DefnCursor(CXCursor, CXTranslationUnit &);
  ~DefnCursor();
};
}
#endif /* CURSOR_HPP */
