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

namespace cursor_traits {

const std::string IdentifierRegexString("[a-zA-Z_][a-zA-Z_0-9]*");

const std::regex IdentifierRegex(IdentifierRegexString,
                                 std::regex_constants::extended);

bool IsValidIdentifier(std::string s) {
  return std::regex_match(s, IdentifierRegex);
}

const std::list<std::string> CursorTypes{"declaration", "reference",
                                         "definition", "call"};

const std::list<std::string> EntitySpecifiers{"type", "variable", "function"};

const std::unordered_map<CXCursorKind, std::string> ScopeKinds{
 {CXCursor_FunctionDecl, "@"}};

bool IsValidFilename(std::string s) {
  for (char & c : s) {
    if ('\0' == c) {
      return false;
    }
  }
  return true;
}

bool IsValidType(std::string s) {
  return not s.empty();
}

using namespace utilities;
/* (filename>)?::(identifier::|identifier@)* */
const std::regex ScopeRegex(
 "(.+>)?::(" +
  boost::algorithm::join(
   transformer<true>::map<std::vector<std::string>>(
    ScopeKinds, [](auto p) { return IdentifierRegexString + p.second; }),
   "|") +
  ")*",
 std::regex_constants::extended);

bool IsValidScope(std::string s) {
  std::smatch matches;
  if (not std::regex_search(s, matches, ScopeRegex)) {
    return false;
  } else {
    return (not matches[1].matched) or IsValidFilename(matches[1]);
  }
}
} /* cursor_traits */

std::tuple<std::string, unsigned int, unsigned int, unsigned int, std::string,
           unsigned int, unsigned int, unsigned int>
 cursor::setup_locations(CXCursor c) {
  CXSourceRange range(clang_getCursorExtent(c));
  CXSourceLocation begin_loc(clang_getRangeStart(range));
  CXSourceLocation fin_loc(clang_getRangeEnd(range));
  /* get beginning stats */
  CXFile beg_file;
  unsigned int beg_line(0), beg_col(0), beg_offset(0);
  /* TODO: refactor and use clang_getFileLocation for macros! */
  clang_getSpellingLocation(begin_loc, &beg_file, &beg_line, &beg_col,
                            &beg_offset);
  /* get end stats */
  CXFile fin_file;
  unsigned int fin_line(0), fin_col(0), fin_offset(0);
  clang_getSpellingLocation(fin_loc, &fin_file, &fin_line, &fin_col,
                            &fin_offset);
  try {
    return std::make_tuple(
     libclang_utils::GetStringAndDispose(clang_getFileName(beg_file)),
     beg_offset, beg_line, beg_col,
     libclang_utils::GetStringAndDispose(clang_getFileName(fin_file)),
     fin_offset, fin_line, fin_col);
  } catch (...) {
    std::cerr << "GOTCHA2" << std::endl;
    exit(1);
  }
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
  /* returns empty string (invalid) if having a "type" doesn't make sense for */
  /* the cursor */
  return libclang_utils::GetStringAndDispose(
   clang_getTypeSpelling(clang_getCursorType(c)));
}

std::string cursor::setup_name(CXCursor c) {
  return libclang_utils::GetStringAndDispose(clang_getCursorSpelling(c));
}

std::string cursor::setup_scope(CXCursor c) {
  CXCursor tmp(c);
  std::string scope_str("::");
  /* TODO: if static linkage, note that at front, obv */
  while (not clang_equalCursors(tmp, clang_getCursorSemanticParent(tmp))) {
    tmp = clang_getCursorSemanticParent(tmp);
    /* TODO: add namespace and class scope */
    /* for (const auto & scopeKind : cursor_traits::ScopeKinds) { */
    /*   if (scopeKind == clang_getCursorKind(c)) { */
    /*     scope_str += ; */
    /*   } */
    /* } */
    if (clang_getCursorKind(tmp) == CXCursor_FunctionDecl) {
      scope_str +=
       libclang_utils::GetStringAndDispose(clang_getCursorSpelling(tmp)) + "@";
    }
  }
  return scope_str;
}

cursor::cursor(CXCursor c)
   : cursorType(setup_cursor_type(c)),
     entitySpec(setup_entity_spec(c)),
     type(setup_type(c)),
     name(setup_name(c)),
     scope(setup_scope(c)) {
  std::tie(begin_file, begin_offset, begin_line, begin_col, end_file,
           end_offset, end_line, end_col) = setup_locations(c);
}

bool cursor::isValid() {
  using namespace cursor_traits;
  using utilities::is_in_container;
  /* TODO: not sure begin_file == end_file is necessary */
  bool res = true;
  if (begin_file != end_file) {
    std::cerr << "file name mismatch" << std::endl;
    res = false;
  }
  if (not IsValidFilename(begin_file)) {
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
  if (not IsValidType(type)) {
    std::cerr << "invalid type" << std::endl;
    res = false;
  }
  if (not IsValidIdentifier(name)) {
    std::cerr << "invalid name" << std::endl;
    res = false;
  }
  if (not IsValidScope(scope)) {
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
     << ',' << name << ',' << scope;
  return ss.str();
}
} /* frontend */
} /* semantic_code_browser */
