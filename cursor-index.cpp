// std includes
#include <cassert>
// local includes
#include "cursor-index.hpp"

namespace semantic_code_browser {
size_t CursorHasher::operator()(const Cursor * c) const {
  return std::hash<std::string>()(c->getUSR()) +
         std::hash<std::string>()(c->getFile()) +
         std::hash<unsigned int>()(c->getOffset()) +
         std::hash<typename std::underlying_type<CXCursorKind>::type>()(
          c->getKind());
}

bool CursorLocationComparer::operator()(const Cursor * lhs,
                                        const Cursor * rhs) const {
  std::string leftFile(lhs->getFile()), rightFile(rhs->getFile());
  if (leftFile != rightFile) {
    return leftFile < rightFile;
  } else {
    return lhs->getOffset() < rhs->getOffset();
  }
}

EntityIndex::EntityIndex() {
}

EntityIndex::~EntityIndex() {
}

EntityIndex * EntityIndex::MakeEntityIndex(const Specifier S) {
  if (Type == S) {
    return new TypedEntityIndex<Type>();
  } else if (Value == S) {
    return new TypedEntityIndex<Value>();
  } else {
    return nullptr;
  }
}

template <Specifier S>
bool TypedEntityIndex<S>::existsAndInserts(EntityCursor<S> * e) {
  auto res(mMemberSet.insert(e));
  return res.second;
}

template <Specifier S>
TypedEntityIndex<S>::TypedEntityIndex()
   : EntityIndex(), mMemberSet(), mDeclSet(), mRefSet(), mDefnSet() {
}

template <Specifier S>
TypedEntityIndex<S>::~TypedEntityIndex() {
  for (auto & entry : mMemberSet) {
    delete entry;
  }
}

template <Specifier S>
bool TypedEntityIndex<S>::addDecl(Cursor * c) {
  // FIXME: dynamic_cast here sucks, annoying, fix
  auto dcl(dynamic_cast<DeclCursor<S> *>(c));
  assert(nullptr != dcl);
  if (!existsAndInserts(dcl)) {
    mDeclSet.insert(dcl);
    return true;
  } else {
    return false;
  }
}

template <Specifier S>
bool TypedEntityIndex<S>::addRef(Cursor * c) {
  auto ref(dynamic_cast<RefCursor<S> *>(c));
  assert(nullptr != ref);
  if (!existsAndInserts(ref)) {
    mRefSet.insert(ref);
    return true;
  } else {
    return false;
  }
}

template <Specifier S>
bool TypedEntityIndex<S>::addDefn(Cursor * c) {
  auto dfn(dynamic_cast<DefnCursor<S> *>(c));
  assert(nullptr != dfn);
  if (!existsAndInserts(dfn)) {
    mDefnSet.insert(dfn);
    return true;
  } else {
    return false;
  }
}

CursorIndex::CursorIndex() : mEntityMap() {
}

CursorIndex::~CursorIndex() {
  for (auto & entry : mEntityMap) {
    delete entry.second;
  }
}

void CursorIndex::insert(Cursor * c) {
  if (nullptr == c) {
    return;
  }
  const std::string & usr = c->getUSR();
  auto entityIterator(mEntityMap.find(usr));
  // if not in hash map already
  if (mEntityMap.end() == entityIterator) {
    entityIterator =
     mEntityMap.emplace(usr, EntityIndex::MakeEntityIndex(c->getSpecifier()))
      .first;
  }
  if (!c->accept(entityIterator->second)) {
    delete c;
  }
}
}
