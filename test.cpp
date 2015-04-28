#include <iostream>        // for I/O
#include <tuple>           // for parseArgs return type
#include <regex>           // for parseArgs regex matching
#include <stdexcept>       // for ArgumentError
#include <clang-c/Index.h> // for clang parsing

class ArgumentError : public std::runtime_error {
 public:
  ArgumentError(std::string str) : std::runtime_error(str) {
  }
};

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
  if (clang_getCString(cxfilename) != 0)
    filename = clang_getCString(cxfilename);
  clang_disposeString(cxfilename);
  return "\"" + filename + "\"";
}

void printLocation(CXSourceLocation location) {
  CXFile file;
  unsigned int line = 0, col = 0, offset = 0;
  clang_getSpellingLocation(location, &file, &line, &col, &offset);
  std::cout << line << ", " << col << ", " << offset << ", "
            << getClangFileName(file);
}

void printExtent(CXSourceRange range, CXTranslationUnit * tup) {
  /*
    std::cout << "Extent begin at location [";
    printLocation(clang_getRangeStart(range));
    std::cout << "]. \n";
    std::cout << "Extent finish at location [";
    printLocation(clang_getRangeEnd(range));
    std::cout << "]. \n";
  */
  CXToken * tokens = 0;
  unsigned int nTokens = 0;
  clang_tokenize(*tup, range, &tokens, &nTokens);
  for (size_t i = 0; i < nTokens; ++i) {
    CXString cxtoken = clang_getTokenSpelling(*tup, tokens[i]);
    std::cout << clang_getCString(cxtoken) << " ";
    clang_disposeString(cxtoken);
  }
  clang_disposeTokens(*tup, tokens, nTokens);
}

// TODO: *do* use the parent attribute to determine nesting!
enum CXChildVisitResult visit(CXCursor cursor,
                              CXCursor parent __attribute__((unused)),
                              CXClientData client_data) {
  CXString cxcursor = clang_getCursorSpelling(cursor);
  CXString cxcursorKind =
   clang_getCursorKindSpelling(clang_getCursorKind(cursor));
  std::cout << "Cursor name is \"" << clang_getCString(cxcursor) << "\", "
            << "type is (" << clang_getCString(cxcursorKind) << "). ";
  clang_disposeString(cxcursorKind);
  clang_disposeString(cxcursor);

  std::cout << "Location is ";
  printLocation(clang_getCursorLocation(cursor));
  std::cout << ". ";

  std::cout << "||";
  printExtent(clang_getCursorExtent(cursor),
              static_cast<CXTranslationUnit *>(client_data));
  std::cout << "||";

  std::cout << "\n";
  return CXChildVisit_Recurse;
}

int main(int argc, char ** argv) {
  const char * infile;
  char ** clangArgs;
  int numArgs;
  try {
    std::tie(infile, clangArgs, numArgs) = parseArgs(argc, argv);
  } catch (ArgumentError & e) {
    std::cerr << e.what() << std::endl;
    exit(1);
  }
  // change second arg to 1 to get diagnostics
  CXIndex index(clang_createIndex(0, 0));
  CXTranslationUnit tu = clang_parseTranslationUnit(
   index, infile, clangArgs, numArgs, nullptr, 0, CXTranslationUnit_None);
  clang_visitChildren(clang_getTranslationUnitCursor(tu), visit, &tu);
}
