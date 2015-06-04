/* std includes */
#include <regex>
#include <sstream>
#ifdef DEBUG
#include <iostream>
#endif
/* external includes */
#include <boost/algorithm/string/join.hpp>
/* local includes */
#include "utilities.hpp"
#include "cursor.hpp"

namespace semantic_code_browser {

namespace backend {

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

/* this allows empty strings, since most of the compiler-specific macros are
   actually defined to be in a file with an empty string for a name */
const std::string cursor::FilenameRegexString("[^\\0]*");
const std::regex cursor::FilenameRegex(FilenameRegexString,
                                       std::regex_constants::extended);

bool cursor::isValidFilename(std::string s) {
  return std::regex_match(s, FilenameRegex);
}

const std::unordered_map<CXCursorKind, std::string> cursor::ScopeKinds{
 {CXCursor_FunctionDecl, "@"}};

namespace {
#ifdef DEBUG
static std::string output_scope_regex(std::string s) {
  std::cerr << s << std::endl;
  return s;
}
#else
#define output_scope_regex(s) s
#endif
}

const std::string cursor::IdentifierRegexString("[a-zA-Z_][a-zA-Z_0-9]*");

const std::regex cursor::IdentifierRegex(IdentifierRegexString,
                                         std::regex_constants::extended);

/* (filename>)?::(identifier::|identifier@)* */
const std::regex cursor::ScopeRegex(
 output_scope_regex(
  "(" + FilenameRegexString + ">)?::(" +
  boost::algorithm::join(
   utilities::transformer<true>::map<std::vector<std::string>>(
    ScopeKinds, [](auto p) { return IdentifierRegexString + p.second; }),
   "|") +
  ")*"),
 std::regex_constants::extended);

std::tuple<std::string, unsigned int, unsigned int, unsigned int, std::string,
           unsigned int, unsigned int, unsigned int>
 cursor::setup_locations(CXCursor c) {
  CXSourceRange range(clang_getCursorExtent(c));
  /* CXToken * tokens = 0;
     unsigned int nTokens = 0;
     CXTranslationUnit tu(clang_Cursor_getTranslationUnit(c));
     clang_tokenize(tu, range, &tokens, &nTokens);
     std::cerr << "range: ";
     for (size_t i = 0; i < nTokens; ++i) {
     std::cerr << libclang_utils::GetStringAndDispose(clang_getTokenSpelling(
     tu, tokens[i])) << (nTokens - 1 == i ? "" : " ");
     }
     std::cerr << std::endl; */
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

/* macro expansion and macro instantion are the same enum, currently, so
   switch yells if we try to case both of them; if that ever changes, let's
   break the build  */
#define ASSERT_EQUAL_ENUMS(x, y)                     \
  static_assert(x == y, #x " should be equal to " #y \
                           "; the libclang api has changed!");

std::string cursor::setup_cursor_type(CXCursor c) {
  ASSERT_EQUAL_ENUMS(CXCursor_MacroExpansion, CXCursor_MacroInstantiation);
  const CXCursorKind k = clang_getCursorKind(c);
  if (clang_isCursorDefinition(c)) {
    return "definition";
  }
  switch (k) {
  case CXCursor_EnumDecl:
  case CXCursor_EnumConstantDecl:
  case CXCursor_FunctionDecl:
  case CXCursor_VarDecl:
  case CXCursor_ParmDecl:
    return "declaration";
  case CXCursor_TypeRef:
  case CXCursor_VariableRef:
  case CXCursor_DeclRefExpr:
  case CXCursor_MacroExpansion:
    return "reference";
  case CXCursor_CallExpr:
    return "call";
  case CXCursor_MacroDefinition:
    return "definition";
  default:
    return "";
  }
}

std::string cursor::setup_entity_spec(CXCursor c) {
  ASSERT_EQUAL_ENUMS(CXCursor_MacroExpansion, CXCursor_MacroInstantiation);
  switch (clang_getCursorKind(c)) {
  case CXCursor_EnumDecl:
  case CXCursor_TypeRef:
    return "type";
  case CXCursor_EnumConstantDecl:
  case CXCursor_VarDecl:
  case CXCursor_ParmDecl:
  case CXCursor_VariableRef:
  case CXCursor_DeclRefExpr:
    return "variable";
  case CXCursor_CallExpr:
  case CXCursor_FunctionDecl:
    return "function";
  case CXCursor_MacroDefinition:
  case CXCursor_MacroExpansion:
    return "macro";
  default:
    return "";
  }
}

std::string cursor::setup_type(CXCursor c) {
  /* returns empty string if a "type" doesn't make sense for the cursor */
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
    std::string beginfile, endfile;
    unsigned int beginoff, endoff, beginline, endline, begincol, endcol;
    std::tie(beginfile, beginoff, beginline, begincol, endfile, endoff, endline,
             endcol) = setup_locations(c);
    scope_str = beginfile + ">";
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
           end_offset, end_line, end_col) = setup_locations(c);
}

bool cursor::isValidType(std::string type_arg) {
  return utilities::is_in_container(entitySpec, UntypedEntitySpecifiers) or
         not type_arg.empty();
}

bool cursor::isValidScope(std::string s) {
  return std::regex_match(s, ScopeRegex);
}

bool cursor::isValidIdentifier(std::string s) {
  return std::regex_match(s, IdentifierRegex);
}

const std::unordered_set<std::string> cursor::CursorTypes{
 "declaration", "reference", "definition", "call"};

const std::unordered_set<std::string> cursor::EntitySpecifiers{
 "type", "variable", "function", "macro"};

/* this level of indirection is done so that we are assured of the validity of
   the compile-time choice of UntypedEntitySpecifiers, even if the actual
   checking must unfortunately be done at runtime (somewhat
   catastrophically). this is better accomplished with a solution of variadic
   templates and enums, but there's really no point in adding additional
   complexity, so we use this hack instead */
namespace {
static std::unordered_set<std::string> untyped_entity_specifiers_impl{"type",
                                                                      "macro"};
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
  using utilities::is_in_container;
#ifdef DEBUG
  bool res = true;
  if (not isValidFilename(begin_file) or not isValidFilename(end_file)) {
    std::cerr << "invalid filename" << std::endl;
    res = false;
  }
  if (not is_in_container(cursorType, CursorTypes)) {
    std::cerr << "invalid cursor type" << std::endl;
    res = false;
  }
  if (not is_in_container(entitySpec, EntitySpecifiers)) {
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
  if (not isValidScope(ref_scope)) {
    std::cerr << "invalid ref scope" << std::endl;
    res = false;
  }
  if (not res) {
    std::cerr << "failed at " << toString() << std::endl;
  }
  return res;
#else
  return isValidFilename(begin_file) and isValidFilename(end_file) and
         is_in_container(cursorType, CursorTypes) and
         is_in_container(entitySpec, EntitySpecifiers) and isValidType(type) and
         isValidIdentifier(name) and isValidScope(scope) and
         isValidScope(ref_scope);
#endif
}

std::string cursor::toString() {
  std::stringstream ss;
  ss << begin_file << ',' << begin_offset << ',' << begin_line << ','
     << begin_col << ',' << end_file << ',' << end_offset << ',' << end_line
     << ',' << end_col << ',' << cursorType << ',' << entitySpec << ',' << type
     << ',' << name << ',' << scope << "," << ref_scope;
  return ss.str();
}
} /* backend */
} /* semantic_code_browser */
