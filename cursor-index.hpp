#ifndef CURSOR_INDEX_HPP
#define CURSOR_INDEX_HPP

// std includes
#include <set>
#include <unordered_set>
#include <functional>
// local includes
#include "cursor.hpp"

namespace semantic_code_browser {
struct CursorHasher {
  size_t operator()(const Cursor *) const;
};

struct CursorLocationComparer {
  bool operator()(const Cursor *, const Cursor *) const;
};

// FIXME: remove this and add a real interface!
class CursorIndex;

class EntityIndex {
 protected:
  friend class CursorIndex;

  EntityIndex(Cursor *);

  Cursor * mTypicalCursor;

  // FIXME: remove these and add a real interface!
  virtual void forEachDecl(std::function<void(Cursor *) >) = 0;
  virtual void forEachRef(std::function<void(Cursor *) >) = 0;
  virtual void forEachDefn(std::function<void(Cursor *) >) = 0;

 public:
  virtual ~EntityIndex();

  /* debugging */
  virtual size_t getSetSize() const = 0;

  static EntityIndex * MakeEntityIndex(Cursor *);

  bool isAnon() const;
  std::string getName() const;
  std::string getUSR() const;
  const Cursor * getTypicalCursor() const;

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

  void forEachDecl(std::function<void(Cursor *) >);
  void forEachRef(std::function<void(Cursor *) >);
  void forEachDefn(std::function<void(Cursor *) >);

 public:
  TypedEntityIndex(Cursor *);
  ~TypedEntityIndex();

  size_t getSetSize() const;

  bool addDecl(Cursor *);
  bool addRef(Cursor *);
  bool addDefn(Cursor *);
};

struct EntityHasher {
  size_t operator()(const EntityIndex *) const;
};

class CursorIndex {
 protected:
  std::unordered_set<EntityIndex *, EntityHasher> mEntitySet;

 public:
  CursorIndex();
  ~CursorIndex();

  /* debugging */
  size_t getSetSize() const;
  std::string displayContents() const;

  /*
     performs double dispatch from cursor::accept to call appropriate add* in
     EntityIndex. WARNING: DELETES CURSOR GIVEN IF EQUAL CURSOR ALREADY PRESENT
     IN INDEX. why? it simplifies the api.
  */
  void insert(Cursor *);
};
}
#endif /* CURSOR_INDEX_HPP */
