// std includes
#include <cassert>
#include <iostream>
#include <sstream>
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

EntityIndex::EntityIndex(Cursor * typicalCursor)
   : mTypicalCursor(typicalCursor) {
}

EntityIndex::~EntityIndex() {
}

EntityIndex * EntityIndex::MakeEntityIndex(Cursor * c) {
  Specifier s = c->getSpecifier();
  if (Type == s) {
    return new TypedEntityIndex<Type>(c);
  } else if (Value == s) {
    return new TypedEntityIndex<Value>(c);
  } else {
    return nullptr;
  }
}

bool EntityIndex::isAnon() const {
  return mTypicalCursor->isAnon();
}

std::string EntityIndex::getName() const {
  return mTypicalCursor->getName();
}

std::string EntityIndex::getUSR() const {
  return mTypicalCursor->getUSR();
}

const Cursor * EntityIndex::getTypicalCursor() const {
  return mTypicalCursor;
}

template <Specifier S>
bool TypedEntityIndex<S>::existsAndInserts(EntityCursor<S> * e) {
  auto res(mMemberSet.insert(e));
  return res.second;
}

template <Specifier S>
void TypedEntityIndex<S>::forEachDecl(std::function<void(Cursor *) > f) {
  for (auto & entry : mDeclSet) {
    f(entry);
  }
}

template <Specifier S>
void TypedEntityIndex<S>::forEachRef(std::function<void(Cursor *) > f) {
  for (auto & entry : mRefSet) {
    f(entry);
  }
}

template <Specifier S>
void TypedEntityIndex<S>::forEachDefn(std::function<void(Cursor *) > f) {
  for (auto & entry : mDefnSet) {
    f(entry);
  }
}

template <Specifier S>
TypedEntityIndex<S>::TypedEntityIndex(Cursor * c)
   : EntityIndex(c), mMemberSet(), mDeclSet(), mRefSet(), mDefnSet() {
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

size_t EntityHasher::operator()(const EntityIndex * ei) const {
  return ei->isAnon() ? CursorHasher()(ei->getTypicalCursor())
                      : std::hash<std::string>()(ei->getUSR());
}

CursorIndex::CursorIndex() : mEntitySet() {
}

CursorIndex::~CursorIndex() {
  // FIXME: deletion causes segfaults!!!
  // for (auto & entry : mEntitySet) {
  //   delete entry;
  // }
}

size_t CursorIndex::getSetSize() const {
  return mEntitySet.size();
}

std::string CursorIndex::displayContents() const {
  std::stringstream ss;
  for (EntityIndex * entry : mEntitySet) {
    ss << (entry->isAnon() ? std::string("<ANON>") : entry->getName()) + ":"
       << std::endl;
    ss << "DECLS: {" << std::endl;
    std::function<void(Cursor *) > f(
     [&](Cursor * c) { ss << "\t" << c->toString() << std::endl; });
    entry->forEachDecl(f);
    ss << "}" << std::endl
       << "REFS: {" << std::endl;
    entry->forEachRef(f);
    ss << "}" << std::endl
       << "DEFNS: {" << std::endl;
    entry->forEachDefn(f);
    ss << "}" << std::endl;
  }
  return ss.str();
}

void CursorIndex::insert(Cursor * c) {
  if (nullptr == c) {
    return;
  }
  std::cerr << c->toString() << std::endl;
  EntityIndex * ei(EntityIndex::MakeEntityIndex(c));
  auto entityIterator(mEntitySet.find(ei));
  // if not in hash map already
  if (mEntitySet.end() == entityIterator) {
    std::cerr << "new entry in hash set: size == " << getSetSize() << std::endl;
    entityIterator = mEntitySet.emplace(ei).first;
  } else {
    delete ei;
    std::cerr << "NOT new entry in hash set: size == " << getSetSize()
              << std::endl;
  }
  if (!c->accept(*entityIterator)) {
    delete c;
  }
}
}
