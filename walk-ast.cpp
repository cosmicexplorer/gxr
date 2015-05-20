// this is all wrenched from
// https://gist.github.com/yifu/3761845

// c++ includes
#include <iostream>  // for I/O
#include <tuple>     // for parseArgs return type
#include <regex>     // for parseArgs regex matching
#include <stdexcept> // for ArgumentError
#include <algorithm> // for for_each
// local includes
#include "cursor.hpp"       // wrapper for libclang cursors
#include "cursor-index.hpp" // wrapper for own form of ast index

namespace scb = semantic_code_browser;

class ArgumentError : public std::runtime_error {
 public:
  ArgumentError(std::string str) : std::runtime_error(str) {
  }
};

// validates input
std::tuple<const char *, char **, int> parseArgs(int argc, char ** argv);

// clang visitor functions
std::string getClangFileName(const CXFile & file);
std::string getDiagInfos(CXDiagnostic diag);
std::string getFixIts(CXDiagnostic diag);
CXChildVisitResult visit(CXCursor cursor, CXCursor parent,
                         CXClientData client_data);

// globals
// name of file we're currently parsing
std::string infile_str;
// base translation unit for the file we're compilingx
CXTranslationUnit * infile_ast;

int main(int argc, char ** argv) {
  const char * infile;
  char ** clangArgs;
  int numArgs;
  try {
    std::tie(infile, clangArgs, numArgs) = parseArgs(argc, argv);
    infile_str = std::string(infile);
  } catch (ArgumentError & e) {
    std::cerr << e.what() << std::endl;
    std::cerr << "Usage: walk-ast INFILE [ARGS...]" << std::endl;
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
    clang_disposeDiagnostic(diag);
  }
  infile_ast = &tu;
  clang_visitChildren(clang_getTranslationUnitCursor(tu), visit, nullptr);
  clang_disposeTranslationUnit(tu);
  clang_disposeIndex(index);
}

std::string getLocation(CXSourceLocation loc) {
  std::stringstream out;
  CXFile file;
  unsigned int line(0), col(0), offset(0);
  clang_getSpellingLocation(loc, &file, &line, &col, &offset);
  out << "(:file " << getClangFileName(file) << " :line " << line << " :col "
      << col << " :offset " << offset << ")";
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

// dumps out each element of the tree
CXChildVisitResult visit(CXCursor cursor,
                         CXCursor parent __attribute__((unused)),
                         CXClientData client_data __attribute__((unused))) {
  scb::Cursor * c = scb::Cursor::MakeCursor(cursor, *infile_ast);
  delete c;
  return CXChildVisit_Recurse;
}
