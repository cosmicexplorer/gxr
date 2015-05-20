// std includes
#include <cassert>
#include <iostream>
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

EntityIndex * EntityIndex::MakeEntityIndex(Specifier s) {
  if (Type == s) {
    return new TypedEntityIndex<Type>();
  } else if (Value == s) {
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
size_t TypedEntityIndex<S>::getSetSize() const {
  return mMemberSet.size();
}

template <Specifier S>
bool TypedEntityIndex<S>::addDecl(Cursor * c) {
  std::cerr << "attempted spec: " << Cursor::ConvertSpecifier(S) << std::endl;
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
  std::cerr << "attempted spec: " << Cursor::ConvertSpecifier(S) << std::endl;
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
  std::cerr << "attempted spec: " << Cursor::ConvertSpecifier(S) << std::endl;
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

size_t CursorIndex::getMapSize() const {
  return mEntityMap.size();
}

void CursorIndex::insert(Cursor * c) {
  if (nullptr == c) {
    std::cerr << "null??" << std::endl;
    return;
  }
  std::cerr << c->toString() << std::endl;
  const std::string & usr = c->getUSR();
  auto entityIterator(mEntityMap.find(usr));
  // if not in hash map already
  if (mEntityMap.end() == entityIterator) {
    std::cerr << "new entry in hash map: size == " << getMapSize() << std::endl;
    entityIterator =
     mEntityMap.emplace(usr, EntityIndex::MakeEntityIndex(c->getSpecifier()))
      .first;
  } else {
    std::cerr << "NOT new entry in hash map: size == " << getMapSize()
              << std::endl;
  }
  if (!c->accept(entityIterator->second)) {
    delete c;
  }
}
}
