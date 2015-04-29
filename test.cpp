// https://gist.github.com/yifu/3761845

// c++ includes
#include <iostream>  // for I/O
#include <tuple>     // for parseArgs return type
#include <sstream>   // for string manipulation in getFile
#include <regex>     // for parseArgs regex matching
#include <stdexcept> // for ArgumentError
// libclang includes
#include <clang-c/Index.h> // for clang parsing
// local includes
#include "Stack.h"

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
std::string getFile(CXSourceLocation location);
std::string getExtent(CXSourceRange range, CXTranslationUnit * tup);
std::string getDiagInfos(CXDiagnostic diag);
std::string getFixIts(CXDiagnostic diag);
enum CXChildVisitResult visit(CXCursor cursor, CXCursor parent,
                              CXClientData client_data);

// name of file we're currently parsing
std::string infile_str;
std::stack<CXCursor> ast_stack;
// SCB::Stack<CXCursor> ast_stack;
CXTranslationUnit * infile_ast;
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
    exit(1);
  }
  // change second arg to 1 to get diagnostics
  CXIndex index(clang_createIndex(0, 0));
  CXTranslationUnit tu = clang_parseTranslationUnit(
   index, infile, clangArgs, numArgs, nullptr, 0, CXTranslationUnit_None);
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
  // TODO: figure out whether or not diagnostics showed errors; if so, quit
  prev_cursor = prev_parent = clang_getTranslationUnitCursor(tu);
  ast_stack.push(prev_cursor);
  infile_ast = &tu;
  std::cout << "'("
            << "root";
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
  out << "file: " << getClangFileName(file) << ", line: " << line
      << ", col: " << col << ", offset: " << offset;
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
  if (1 >= argc) {
    throw ArgumentError("No arguments passed to program!");
  } else {
    std::regex allowedFileTypes("\\.(c|cpp|h|(c|h)xx|cc|C)$");
    std::string infile((std::string(argv[1])));
    if (std::regex_search(infile, allowedFileTypes)) {
      char ** ret_argv(nullptr);
      int ret_argc(0);
      if (2 < argc) {
        ret_argv = argv + 2;
        ret_argc = argc - 2;
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
  return out.str();
}

bool cursorEquals(CXCursor a, CXCursor b) {
  return (a.data[0] == b.data[0] and a.data[1] == b.data[1] and
          a.data[2] == b.data[2]);
}

std::tuple<TreeMotion, size_t> getTypeOfTreeMotion(CXCursor parent,
                                                   CXCursor current) {
  // #ifdef DEBUG
  // std::stack<CXCursor> s(ast_stack);
  // std::cout << std::endl;
  // while (not s.empty()) {
  //   std::cout << s.top().data[0] << ",";
  //   s.pop();
  // }
  // std::cout << std::endl;
  // std::cout << "parent: " << parent.data[0] << "|" << parent.data[1] << "|"
  //           << parent.data[2] << ", child: " << current.data[0] << "|"
  //           << current.data[1] << "|" << current.data[2] << std::endl;
  // #endif
  TreeMotion retval;
  size_t numPops = 0;
  if (cursorEquals(prev_cursor, parent)) {
    retval = TreeMotion::Child;
  } else if (cursorEquals(prev_parent, parent)) {
    retval = TreeMotion::Sibling;
    if (ast_stack.empty()) {
      throw std::logic_error("should never have 0! we counted wrong.");
    }
    ast_stack.pop();
    ++numPops;
  } else {
    // size_t len = ast_stack.size();
    // std::cerr << len << std::endl;
    if (ast_stack.empty()) {
      throw std::logic_error("should never have 0, even when popping many "
                             "times! we counted wrong.");
    }
    while (not cursorEquals(parent, ast_stack.top())) {
      if (ast_stack.empty()) {
        throw std::logic_error("should never have 0, even when popping many "
                               "times! we counted wrong.");
      }
      ast_stack.pop();
      ++numPops;
    }
    retval = TreeMotion::HigherSibling;
  }
  ast_stack.push(current);
  prev_cursor = current;
  prev_parent = parent;
  return std::tuple<TreeMotion, size_t>(retval, numPops);
}

enum CXChildVisitResult visit(CXCursor cursor, CXCursor parent,
                              CXClientData client_data
                              __attribute__((unused))) {
  std::string fromFile = getFile(clang_getCursorLocation(cursor));
  // #ifdef DEBUG
  // if (fromFile == infile_str) {
  // #else
  if (true) {
    // #endif
    TreeMotion typeOfMotion;
    size_t numPops;
    std::tie(typeOfMotion, numPops) = getTypeOfTreeMotion(parent, cursor);
    CXString cxcursor = clang_getCursorSpelling(cursor);
    CXString cxcursorKind =
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
    // #ifdef DEBUG
    // switch (typeOfMotion) {
    // case TreeMotion::Child:
    //   std::cout << "Descent: " << numPops << " pops!" << std::endl;
    //   break;
    // case TreeMotion::Sibling:
    //   std::cout << "Sidestep: " << numPops << " pop!" << std::endl;
    //   break;
    // case TreeMotion::HigherSibling:
    // default:
    //   std::cout << "Over and out: " << numPops << " pops!" << std::endl;
    //   break;
    // }
    // #endif
    std::cout << ":name "
              << "\"" << clang_getCString(cxcursor) << "\""
              << " :type "
              << "'" << clang_getCString(cxcursorKind) << " :file "
              << "\"" << fromFile << "\""
              << " :text \""
              << getExtent(clang_getCursorExtent(cursor), infile_ast) << "\"";
    // std::cout
    //  << "name: \"" << clang_getCString(cxcursor) << "\""
    //  << ", type: " << clang_getCString(cxcursorKind) << ", file: " <<
    //  fromFile
    //  << ", text: \"" << getExtent(clang_getCursorExtent(cursor), infile_ast)
    //  << "\""
    //  << ", location: " << getLocation(clang_getCursorLocation(cursor))
    //  << std::endl;
    // clang_disposeString(cxcursorKind);
    // clang_disposeString(cxcursor);
  }
  return CXChildVisit_Recurse;
}
