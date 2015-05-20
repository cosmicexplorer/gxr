#ifndef CURSOR_INDEX_HPP
#define CURSOR_INDEX_HPP

// std includes
#include <set>
#include <unordered_set>
#include <unordered_map>
// local includes
#include "cursor.hpp"

namespace semantic_code_browser {
struct CursorHasher {
  size_t operator()(const Cursor * c) const;
};

struct CursorLocationComparer {
  bool operator()(const Cursor * lhs, const Cursor * rhs) const;
};

class EntityIndex {
 protected:
  EntityIndex();

 public:
  virtual ~EntityIndex();

  /* debugging */
  virtual size_t getSetSize() const = 0;

  static EntityIndex * MakeEntityIndex(Specifier);

  virtual bool addDecl(Cursor *) = 0;
  virtual bool addRef(Cursor *) = 0;
  virtual bool addDefn(Cursor *) = 0;
};

template <Specifier S>
struct TypedEntityIndex : public EntityIndex {
 private:
  // tells you if it exists, and inserts into hashset if not.
  bool existsAndInserts(EntityCursor<S> *);

  std::unordered_set<Cursor *, CursorHasher> mMemberSet;
  std::set<DeclCursor<S> *, CursorLocationComparer> mDeclSet;
  std::set<RefCursor<S> *, CursorLocationComparer> mRefSet;
  std::set<DefnCursor<S> *, CursorLocationComparer> mDefnSet;

 public:
  TypedEntityIndex();
  ~TypedEntityIndex();

  size_t getSetSize() const;

  bool addDecl(Cursor *);
  bool addRef(Cursor *);
  bool addDefn(Cursor *);
};

class CursorIndex {
 protected:
  std::unordered_map<std::string, EntityIndex *> mEntityMap;

 public:
  CursorIndex();
  ~CursorIndex();

  /* debugging */
  size_t getMapSize() const;

  /*
     performs double dispatch from cursor::accept to call appropriate add* in
     EntityIndex. WARNING: DELETES CURSOR GIVEN IF EQUAL CURSOR ALREADY PRESENT
     IN INDEX. why? it simplifies the api.
  */
  void insert(Cursor *);
};
}
#endif /* CURSOR_INDEX_HPP */
