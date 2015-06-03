/* std includes */
#include <regex>
#include <sstream>
#include <iostream> /* todo: remove */
/* external includes */
#include <boost/algorithm/string/join.hpp>
/* local includes */
#include "utilities.hpp"
#include "cursor.hpp"

namespace semantic_code_browser {

namespace frontend {

namespace libclang_utils {

std::string GetStringAndDispose(CXString cxs) {
  const char * cstr(clang_getCString(cxs));
  if (nullptr == cstr) {
    return "";
  }
  std::string ret(cstr);
  clang_disposeString(cxs);
  return ret;
}
} /* libclang_utils */

namespace cursor_traits {} /* cursor_traits */

std::tuple<std::string, unsigned int, unsigned int, unsigned int, std::string,
           unsigned int, unsigned int, unsigned int>
 cursor::setupLocations(CXCursor c) {
  CXSourceRange range(clang_getCursorExtent(c));
  CXSourceLocation begin_loc(clang_getRangeStart(range));
  CXSourceLocation fin_loc(clang_getRangeEnd(range));
  /* get beginning stats */
  CXFile beg_file;
  unsigned int beg_line(0), beg_col(0), beg_offset(0);
  /* TODO: refactor and use clang_getFileLocation for macros and macro
     expansions! */
  clang_getSpellingLocation(begin_loc, &beg_file, &beg_line, &beg_col,
                            &beg_offset);
  /* get end stats */
  CXFile fin_file;
  unsigned int fin_line(0), fin_col(0), fin_offset(0);
  clang_getSpellingLocation(fin_loc, &fin_file, &fin_line, &fin_col,
                            &fin_offset);
  /* the +-1 is because libclang's representation of file positions is off
     that amount from what most other programs that manage files (emacs, etc)
     use; i'm not sure why */
  return std::make_tuple(
   libclang_utils::GetStringAndDispose(clang_getFileName(beg_file)),
   beg_offset + 1, beg_line, beg_col - 1,
   libclang_utils::GetStringAndDispose(clang_getFileName(fin_file)),
   fin_offset + 1, fin_line, fin_col - 1);
}

std::string cursor::setup_cursor_type(CXCursor c) {
  /* TODO: see if this is ever called, and *isn't* also a declaration */
  if (clang_isCursorDefinition(c)) {
    return "definition";
  }
  switch (clang_getCursorKind(c)) {
  case CXCursor_EnumDecl:
  case CXCursor_EnumConstantDecl:
  case CXCursor_FunctionDecl:
  case CXCursor_VarDecl:
  case CXCursor_ParmDecl:
    return "declaration";
    break;
  case CXCursor_TypeRef:
  case CXCursor_VariableRef:
  case CXCursor_DeclRefExpr:
    return "reference";
    break;
  /* TODO: flesh out the difference between "reference" and "call;" see which */
  /* one is actually used (or if both are used) when a function is called, or if
   */
  /* CallExpr is ever used at all */
  case CXCursor_CallExpr:
    return "call";
    break;
  default:
    return ""; /* invalid result */
    break;
  }
}

std::string cursor::setup_entity_spec(CXCursor c) {
  switch (clang_getCursorKind(c)) {
  case CXCursor_EnumDecl:
  case CXCursor_TypeRef:
    return "type";
    break;
  case CXCursor_EnumConstantDecl:
  case CXCursor_VarDecl:
  case CXCursor_ParmDecl:
  case CXCursor_VariableRef:
  /* TODO: i think CXCursor_DeclRefExpr is used for both calling functions and
   */
  /* referring to them. more processing is needed for this one */
  case CXCursor_DeclRefExpr:
    return "variable";
    break;
  case CXCursor_CallExpr:
  case CXCursor_FunctionDecl:
    return "function";
    break;
  default:
    return ""; /* invalid result */
    break;
  }
}

std::string cursor::setup_type(CXCursor c) {
  /* returns empty string (invalid) if having a "type" doesn't make sense for
   * the cursor */
  return libclang_utils::GetStringAndDispose(
   clang_getTypeSpelling(clang_getCursorType(c)));
}

std::string cursor::setup_name(CXCursor c) {
  return libclang_utils::GetStringAndDispose(clang_getCursorSpelling(c));
}

std::string cursor::setup_scope(CXCursor c) {
  using libclang_utils::GetStringAndDispose;
  std::string scope_str;
  if (clang_getCursorLinkage(c) == CXLinkage_Internal) {
    scope_str = ">";
  }
  scope_str += "::";
  CXCursor tmp(c);
  /* TODO: if static linkage, note that at front, obv */
  while (not clang_equalCursors(tmp, clang_getCursorSemanticParent(tmp))) {
    tmp = clang_getCursorSemanticParent(tmp);
    auto it =
     std::find_if(std::begin(ScopeKinds), std::end(ScopeKinds), [&tmp](auto p) {
       return p.first == clang_getCursorKind(tmp);
     });
    if (std::end(ScopeKinds) != it) {
      scope_str +=
       GetStringAndDispose(clang_getCursorSpelling(tmp)) + it->second;
    }
  }
  return scope_str;
}

std::string cursor::setup_ref_scope(CXCursor c) {
  return setup_scope(clang_getCursorReferenced(c));
}

cursor::cursor(CXCursor c)
   : cursorType(setup_cursor_type(c)),
     entitySpec(setup_entity_spec(c)),
     type(setup_type(c)),
     name(setup_name(c)),
     scope(setup_scope(c)),
     ref_scope(setup_ref_scope(c)) {
  std::tie(begin_file, begin_offset, begin_line, begin_col, end_file,
           end_offset, end_line, end_col) = setupLocations(c);
}

bool cursor::isValidType(std::string type_arg) {
  return utilities::is_in_container(entitySpec, UntypedEntitySpecifiers) or
         not type_arg.empty();
}

bool cursor::isValidFilename(std::string s) {
  return not utilities::is_in_container('\0', s);
}

const std::unordered_map<CXCursorKind, std::string> cursor::ScopeKinds{
 {CXCursor_FunctionDecl, "@"}};

namespace {
/* TODO: remove. here for debugging */
static std::string output_scope_regex(std::string s) {
  std::cerr << s << std::endl;
  return s;
}
}

const std::string cursor::IdentifierRegexString("[a-zA-Z_][a-zA-Z_0-9]*");

const std::regex cursor::IdentifierRegex(IdentifierRegexString,
                                         std::regex_constants::extended);

/* (filename>)?::(identifier::|identifier@)* */
const std::regex cursor::ScopeRegex(
 output_scope_regex(
  ">?::(" +
  boost::algorithm::join(
   utilities::transformer<true>::map<std::vector<std::string>>(
    ScopeKinds, [](auto p) { return IdentifierRegexString + p.second; }),
   "|") +
  ")*"),
 std::regex_constants::extended);

bool cursor::isValidScope(std::string s) {
  return std::regex_match(s, ScopeRegex);
}

bool cursor::isValidIdentifier(std::string s) {
  return std::regex_match(s, IdentifierRegex);
}

const std::unordered_set<std::string> cursor::CursorTypes{
 "declaration", "reference", "definition", "call"};

const std::unordered_set<std::string> cursor::EntitySpecifiers{
 "type", "variable", "function"};

namespace {
static std::unordered_set<std::string> untyped_entity_specifiers_impl{"type"};
static std::unordered_set<std::string> create_untyped_entity_specifiers() {
  if (utilities::is_subset(untyped_entity_specifiers_impl,
                           cursor::EntitySpecifiers)) {
    return untyped_entity_specifiers_impl;
  } else {
    throw ValidityError(
     "untyped entity specifiers are not a subset of entity specifiers");
  }
}
}
const std::unordered_set<std::string>
 cursor::UntypedEntitySpecifiers(create_untyped_entity_specifiers());

bool cursor::isValid() {
  bool res = true;
  if (begin_file != end_file) {
    std::cerr << "file name mismatch" << std::endl;
    res = false;
  }
  if (not isValidFilename(begin_file)) {
    std::cerr << "invalid filename" << std::endl;
    res = false;
  }
  if (not utilities::is_in_container(cursorType, CursorTypes)) {
    std::cerr << "invalid cursor type" << std::endl;
    res = false;
  }
  if (not utilities::is_in_container(entitySpec, EntitySpecifiers)) {
    std::cerr << "invalid entity specifier" << std::endl;
    res = false;
  }
  if (not isValidType(type)) {
    std::cerr << "invalid type" << std::endl;
    res = false;
  }
  if (not isValidIdentifier(name)) {
    std::cerr << "invalid name" << std::endl;
    res = false;
  }
  if (not isValidScope(scope)) {
    std::cerr << "invalid scope" << std::endl;
    res = false;
  }
  if (not res) {
    std::cerr << "failed at " << toString() << std::endl;
  }
  return res;
}

std::string cursor::toString() {
  std::stringstream ss;
  ss << begin_file << ',' << begin_offset << ',' << begin_line << ','
     << begin_col << ',' << end_file << ',' << end_offset << ',' << end_line
     << ',' << end_col << ',' << cursorType << ',' << entitySpec << ',' << type
     << ',' << name << ',' << scope << "," << ref_scope;
  return ss.str();
}
} /* frontend */
} /* semantic_code_browser */
