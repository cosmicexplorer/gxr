#include <iostream>
#include <string>
#include <clang-c/Index.h>

bool isFileExt(std::string s, std::string ext) {
  if (s.size() > ext.size()) {
    bool fails(true);
    for (size_t i = 0; i < ext.size(); ++i) {
      fails = s[s.size() - i - 1] == ext[ext.size() - i - 1] and fails;
    }
    fails = '.' == s[s.size() - ext.size() - 1] and fails;
    return fails;
  } else {
    return false;
  }
}

std::string getClangFileName(const CXFile &file) {
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

void printExtent(CXSourceRange range, CXTranslationUnit *tup) {
  /*
    std::cout << "Extent begin at location [";
    printLocation(clang_getRangeStart(range));
    std::cout << "]. \n";
    std::cout << "Extent finish at location [";
    printLocation(clang_getRangeEnd(range));
    std::cout << "]. \n";
  */
  CXToken *tokens = 0;
  unsigned int nTokens = 0;
  clang_tokenize(*tup, range, &tokens, &nTokens);
  for (int i = 0; i < nTokens; ++i) {
    CXString cxtoken = clang_getTokenSpelling(*tup, tokens[i]);
    std::cout << clang_getCString(cxtoken) << " ";
    clang_disposeString(cxtoken);
  }
  clang_disposeTokens(*tup, tokens, nTokens);
}

enum CXChildVisitResult visit(CXCursor cursor, CXCursor parent,
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

int a = 3;

int main(int argc, char **argv) {
  if (argc == 1 or (not isFileExt(std::string(argv[1]), "cpp") and
                    not isFileExt(std::string(argv[1]), "c"))) {
    std::cerr << "Gimme a C/C++ file to parse!" << std::endl;
  }
  // change second arg to 1 for diagnostics
  CXIndex index(clang_createIndex(0, 0));
  const char *args[] = {"-std=c++11", "-I/usr/include", "-I.",
                        "-I/usr/lib/clang/3.6.0/include"};
  int numArgs = sizeof(args) / sizeof(*args);
  CXTranslationUnit tu = clang_parseTranslationUnit(
      index, argv[1], args, numArgs, nullptr, 0, CXTranslationUnit_None);
  clang_visitChildren(clang_getTranslationUnitCursor(tu), visit, &tu);
}
