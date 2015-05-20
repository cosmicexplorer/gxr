#ifndef CURSOR_INDEX_HPP
#define CURSOR_INDEX_HPP

// std includes
#include <map>
#include <set>
#include <unordered_set>
// local includes
#include "cursor.hpp"

namespace semantic_code_browser {
// struct CursorLocationComparer {
//   bool operator()(const Cursor * lhs, const Cursor * rhs) const;
// };

// class EntityIndex {
//  protected:
//   bool exists(Cursor *) const;
//   std::unordered_set<Cursor *> mMemberSet;
//   std::set<Cursor *, CursorLocationComparer> mDeclSet;
//   std::set<Cursor *, CursorLocationComparer> mRefSet;
//   std::set<Cursor *, CursorLocationComparer> mDefnSet;

//  public:
//   void addDecl(Cursor *);
//   void addRef(Cursor *);
//   void addDefn(Cursor *);
// };

// template <Specifier S>
// struct ScopedEntity : public EntityIndex {
//  public:
//   ScopedEntity();
// };

// class CursorIndex {
//  protected:
//   CursorIndex();

//   std::unordered_set<EntityIndex *>;

//  public:
//   static CursorIndex * MakeCursorIndex();

//   void insert(Cursor *);
// };
}
#endif /* CURSOR_INDEX_HPP */
