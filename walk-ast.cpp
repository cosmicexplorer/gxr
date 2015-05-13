// this is all wrenched from
// https://gist.github.com/yifu/3761845

// c++ includes
#include <iostream>  // for I/O
#include <tuple>     // for parseArgs return type
#include <sstream>   // for string manipulation in getFile
#include <regex>     // for parseArgs regex matching
#include <stdexcept> // for ArgumentError
#include <list>      // for std::list to debug failure
// libclang includes
#include <clang-c/Index.h> // for clang parsing
// local includes
#include "Stack.h"

// utility function used in closing delimiters on output
// i literally don't remember how i came up with this but it works according to
// the parenthesis counts (and lisp doesn't choke on it so that's cool)
inline size_t CLOSE_DELIMS(size_t num_pops) {
  return 2 * num_pops - 1;
}

class ArgumentError : public std::runtime_error {
 public:
  ArgumentError(std::string str) : std::runtime_error(str) {
  }
};

/* Types of motion that can occur along the AST:
 *
 * Sibling: when the node visited is a direct sibling of the previous node.
 * Child: when the node visited is a direct child of the previous node.
 * HigherSibling: else (when the node visited is neither a sibling nor a child
 *   of the previous node)
 */
enum class TreeMotion { Sibling, Child, HigherSibling };

// validates input
std::tuple<const char *, char **, int> parseArgs(int argc, char ** argv);

// clang visitor functions
std::string getClangFileName(const CXFile & file);
std::string getDiagInfos(CXDiagnostic diag);
std::string getFixIts(CXDiagnostic diag);
enum CXChildVisitResult visit(CXCursor cursor, CXCursor parent,
                              CXClientData client_data);

// globals
// name of file we're currently parsing
std::string infile_str;
bool do_parse_other_files(true);
// stack we use to traverse the tree
std::stack<CXCursor> ast_stack;
// SCB::Stack<CXCursor> ast_stack;
// base translation unit for the file we're compilingx
CXTranslationUnit * infile_ast;
// used in traversal
CXCursor prev_cursor;
CXCursor prev_parent;

int main(int argc, char ** argv) {
  const char * infile;
  char ** clangArgs;
  int numArgs;
  try {
    std::tie(infile, clangArgs, numArgs) = parseArgs(argc, argv);
    infile_str = std::string(infile);
  } catch (ArgumentError & e) {
    std::cerr << e.what() << std::endl;
    std::cerr << "Usage: walk-ast INFILE SAME-FILE [ARGS...]" << std::endl;
    exit(1);
  }
  CXIndex index(clang_createIndex(0, 0));
  CXTranslationUnit tu =
   clang_parseTranslationUnit(index, infile, clangArgs, numArgs, nullptr, 0,
                              CXTranslationUnit_DetailedPreprocessingRecord);
  int n = clang_getNumDiagnostics(tu);
  for (int i = 0; i < n; ++i) {
    CXDiagnostic diag(clang_getDiagnostic(tu, i));
    CXString diag_str(
     clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions()));
    std::cerr << clang_getCString(diag_str) << std::endl;
    clang_disposeString(diag_str);
    std::cerr << getDiagInfos(diag) << std::endl;
    std::cerr << getFixIts(diag) << std::endl;
  }
  prev_cursor = prev_parent = clang_getTranslationUnitCursor(tu);
  ast_stack.push(prev_cursor);
  infile_ast = &tu;
  std::cout << "(";
  clang_visitChildren(clang_getTranslationUnitCursor(tu), visit, nullptr);
  clang_disposeTranslationUnit(tu);
  clang_disposeIndex(index);
  for (size_t i = 0; i < CLOSE_DELIMS(ast_stack.size()); ++i) {
    std::cout << ")";
  }
  std::cout << std::endl;
}

std::string getLocation(CXSourceLocation loc) {
  std::stringstream out;
  CXFile file;
  unsigned int line(0), col(0), offset(0);
  clang_getSpellingLocation(loc, &file, &line, &col, &offset);
  out << "(:line " << line << " :col " << col << " :offset " << offset << ")";
  return out.str();
}

std::string getDiagInfos(CXDiagnostic diag) {
  std::stringstream out;
  out << "severity: " << clang_getDiagnosticSeverity(diag);
  out << ", location: " << getLocation(clang_getDiagnosticLocation(diag));
  CXString diagSpellingStr(clang_getDiagnosticSpelling(diag));
  out << ", spelling: " << clang_getCString(diagSpellingStr);
  clang_disposeString(diagSpellingStr);
  return out.str();
}

std::string getFixIts(CXDiagnostic diag) {
  std::stringstream out;
  int numFixIts(clang_getDiagnosticNumFixIts(diag));
  for (int i = 0; i < numFixIts; ++i) {
    CXSourceRange srcRange;
    CXString fixItStr(clang_getDiagnosticFixIt(diag, i, &srcRange));
    out << clang_getCString(fixItStr) << ",";
  }
  return out.str();
}

std::tuple<const char *, char **, int> parseArgs(int argc, char ** argv) {
  if (1 == argc) {
    throw ArgumentError("No arguments passed to program!");
  } else if (2 == argc or
             not std::regex_match(std::string(argv[2]), std::regex("[01]"))) {
    throw ArgumentError("Specify 1 for SAME-FILE to parse all includes, or "
                        "0 to parse only the given source file.");
  } else {
    std::regex allowedFileTypes("\\.(c|cpp|h|(c|h)xx|cc|C)$");
    std::string infile((std::string(argv[1])));
    if (std::regex_search(infile, allowedFileTypes)) {
      char ** ret_argv(nullptr);
      int ret_argc(0);
      do_parse_other_files = std::string(argv[2]) == std::string("1");
      if (3 < argc) {
        ret_argv = argv + 3;
        ret_argc = argc - 3;
      }
      return std::tuple<const char *, char **, int>(argv[1], ret_argv,
                                                    ret_argc);
    } else {
      throw ArgumentError("Input filetype of " + infile + " not recognized!");
    }
  }
}

std::string getClangFileName(const CXFile & file) {
  std::string filename;
  CXString cxfilename = clang_getFileName(file);
  if (clang_getCString(cxfilename) != 0) {
    filename = clang_getCString(cxfilename);
  }
  clang_disposeString(cxfilename);
  return filename;
}

std::string getFile(CXSourceLocation location) {
  CXFile file;
  unsigned int line = 0, col = 0, offset = 0;
  clang_getSpellingLocation(location, &file, &line, &col, &offset);
  return getClangFileName(file);
}

std::string unquoteForeignString(std::string s) {
  return std::regex_replace(std::regex_replace(s, std::regex("\\\\"), "\\\\"),
                            std::regex("\""), "\\\"");
}

std::string getExtent(CXSourceRange range, CXTranslationUnit * tup) {
  CXToken * tokens = 0;
  unsigned int nTokens = 0;
  clang_tokenize(*tup, range, &tokens, &nTokens);
  std::stringstream out;
  for (size_t i = 0; i < nTokens; ++i) {
    CXString cxtoken = clang_getTokenSpelling(*tup, tokens[i]);
    out << clang_getCString(cxtoken) << " ";
    clang_disposeString(cxtoken);
  }
  clang_disposeTokens(*tup, tokens, nTokens);
  // sanitize quotes (i hate the std::regex quoting rules)
  return "\"" + unquoteForeignString(out.str()) + "\"";
}

bool cursorEquals(CXCursor a, CXCursor b) {
  return (a.data[1] == b.data[1] and
          // a.data[0] == b.data[0] and
          a.data[2] == b.data[2]);
}

std::string getCursorPointers(CXCursor cursor) {
  std::stringstream s;
  s << cursor.data[0] << "," << cursor.data[1] << "," << cursor.data[2];
  return s.str();
}

std::tuple<TreeMotion, size_t> getTypeOfTreeMotion(CXCursor parent,
                                                   CXCursor current) {
  TreeMotion retval;
  size_t numPops = 0;
  if (cursorEquals(prev_cursor, parent)) {
    retval = TreeMotion::Child;
  } else if (cursorEquals(prev_parent, parent)) {
    retval = TreeMotion::Sibling;
    if (ast_stack.empty()) {
      std::cerr << "stack is empty at TreeMotion::Sibling!" << std::endl;
    } else {
      ast_stack.pop();
      ++numPops;
    }
  } else {
    std::list<std::string> cursorResults;
    if (ast_stack.empty()) {
      goto error;
    }
    // assert(!ast_stack.empty());
    // a sneaky compiler optimization (that occurs even at -O0! with either gcc
    // or clang!) makes the assertion succeed but the line below it fail. moving
    // this into a separate variable avoids this.
    // CXCursor cur_top(ast_stack.top());
    while (not cursorEquals(parent, ast_stack.top())) {
      cursorResults.emplace_front(getCursorPointers(ast_stack.top()));
      ast_stack.pop();
      ++numPops;
      // assert(!ast_stack.empty());
      // cur_top = ast_stack.top();
      if (ast_stack.empty()) {
        goto error;
      }
    }
    goto success;
  error:
    for (auto & res : cursorResults) {
      std::cerr << "top: " << res << std::endl;
    }
    std::cerr << "parent: " << getCursorPointers(parent)
              << "; child: " << getCursorPointers(current) << std::endl;
    std::cerr << "stack is empty at TreeMotion::HigherSibling!" << std::endl;
  success:
    retval = TreeMotion::HigherSibling;
  }
  ast_stack.push(current);
  prev_cursor = current;
  prev_parent = parent;
  std::string retstr;
  switch (retval) {
  case TreeMotion::HigherSibling:
    retstr = "HigherSibling";
    break;
  case TreeMotion::Sibling:
    retstr = "Sibling";
    break;
  case TreeMotion::Child:
    retstr = "Child";
    break;
  default:
    throw 1;
  }
  // std::cout << std::endl
  //           << getCursorPointers(parent) << " : " <<
  //           getCursorPointers(current)
  //           << " : " << retstr << std::endl;
  return std::tuple<TreeMotion, size_t>(retval, numPops);
}

// dumps out each element of the tree
enum CXChildVisitResult visit(CXCursor cursor, CXCursor parent,
                              CXClientData client_data
                              __attribute__((unused))) {
  std::string fromFile = getFile(clang_getCursorLocation(cursor));
  if (do_parse_other_files or fromFile == infile_str) {
    TreeMotion typeOfMotion;
    size_t numPops;
    std::tie(typeOfMotion, numPops) = getTypeOfTreeMotion(parent, cursor);
    CXString cxcursorStr = clang_getCursorSpelling(cursor);
    CXString cxcursorKindStr =
     clang_getCursorKindSpelling(clang_getCursorKind(cursor));
    if (TreeMotion::Child == typeOfMotion) {
      std::cout << " :children" << std::endl;
      for (size_t i = 0; i < ast_stack.size() - 1; ++i) {
        std::cout << " ";
      }
      std::cout << "((";
    } else if (TreeMotion::Sibling == typeOfMotion) {
      std::cout << ")" << std::endl;
      for (size_t i = 0; i < ast_stack.size() - 1; ++i) {
        std::cout << " ";
      }
      std::cout << "(";
    } else if (TreeMotion::HigherSibling == typeOfMotion) {
      for (size_t i = 0; i < CLOSE_DELIMS(numPops); ++i) {
        std::cout << ")";
      }
      std::cout << std::endl;
      for (size_t i = 0; i < ast_stack.size() - 1; ++i) {
        std::cout << " ";
      }
      std::cout << "(";
    }
    CXSourceRange extent = clang_getCursorExtent(cursor);
    std::cout << ":name "
              << "\"" << unquoteForeignString(clang_getCString(cxcursorStr))
              << "\""
              << " :type "
              // || pipes are because types can have spaces
              << "'|" << clang_getCString(cxcursorKindStr) << "| :file "
              << "\"" << fromFile << "\""
              << " :text " << getExtent(extent, infile_ast)
              << " :extent (:range-start "
              << getLocation(clang_getRangeStart(extent)) << " :range-end "
              << getLocation(clang_getRangeEnd(extent)) << ")";
  }
  return CXChildVisit_Recurse;
}
