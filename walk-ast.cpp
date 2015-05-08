// heavily inspired by https://gist.github.com/yifu/3761845

// standard lib includes
#include <regex>     // for parseArgs regex matching
#include <cstring>
// libclang includes
#include <clang-c/Index.h> // for clang parsing

// clang visitor functions
std::string getClangFileName(const CXFile & file);
std::string getDiagInfos(CXDiagnostic diag);
std::string getFixIts(CXDiagnostic diag);

// globals
// name of file we're currently parsing
std::string infile_str;
// base translation unit for the file we're compilingx
CXTranslationUnit * infile_ast;
// used in traversal
CXCursor prev_cursor;
CXCursor prev_parent;

extern "C" {
int visit_ast(char * infile) {
  return strlen(infile);
}
// int visit_ast(char * infile, char ** clangArgs, int numArgs,
//               enum CXChildVisitResult (*visit)(CXCursor cursor, CXCursor parent,
//                                                CXClientData client_data
//                                                __attribute__((unused)))) {
//   CXIndex index(clang_createIndex(0, 0));
//   CXTranslationUnit tu =
//    clang_parseTranslationUnit(index, infile, clangArgs, numArgs, nullptr, 0,
//                               CXTranslationUnit_DetailedPreprocessingRecord);
//   int n = clang_getNumDiagnostics(tu);
//   for (int i = 0; i < n; ++i) {
//     CXDiagnostic diag(clang_getDiagnostic(tu, i));
//     CXString diag_str(
//      clang_formatDiagnostic(diag, clang_defaultDiagnosticDisplayOptions()));
//     std::cerr << clang_getCString(diag_str) << std::endl;
//     clang_disposeString(diag_str);
//     std::cerr << getDiagInfos(diag) << std::endl;
//     std::cerr << getFixIts(diag) << std::endl;
//   }
//   prev_cursor = prev_parent = clang_getTranslationUnitCursor(tu);
//   infile_ast = &tu;
//   clang_visitChildren(clang_getTranslationUnitCursor(tu), visit, nullptr);
//   clang_disposeTranslationUnit(tu);
//   clang_disposeIndex(index);
// }
}

// std::string getLocation(CXSourceLocation loc) {
//   std::stringstream out;
//   CXFile file;
//   unsigned int line(0), col(0), offset(0);
//   clang_getSpellingLocation(loc, &file, &line, &col, &offset);
//   out << "(:line " << line << " :col " << col << " :offset " << offset <<
//   ")";
//   return out.str();
// }

// std::string getDiagInfos(CXDiagnostic diag) {
//   std::stringstream out;
//   out << "severity: " << clang_getDiagnosticSeverity(diag);
//   out << ", location: " << getLocation(clang_getDiagnosticLocation(diag));
//   CXString diagSpellingStr(clang_getDiagnosticSpelling(diag));
//   out << ", spelling: " << clang_getCString(diagSpellingStr);
//   clang_disposeString(diagSpellingStr);
//   return out.str();
// }

// std::string getFixIts(CXDiagnostic diag) {
//   std::stringstream out;
//   int numFixIts(clang_getDiagnosticNumFixIts(diag));
//   for (int i = 0; i < numFixIts; ++i) {
//     CXSourceRange srcRange;
//     CXString fixItStr(clang_getDiagnosticFixIt(diag, i, &srcRange));
//     out << clang_getCString(fixItStr) << ",";
//   }
//   return out.str();
// }

// std::string getClangFileName(const CXFile & file) {
//   std::string filename;
//   CXString cxfilename = clang_getFileName(file);
//   if (clang_getCString(cxfilename) != 0) {
//     filename = clang_getCString(cxfilename);
//   }
//   clang_disposeString(cxfilename);
//   return filename;
// }

// std::string getFile(CXSourceLocation location) {
//   CXFile file;
//   unsigned int line = 0, col = 0, offset = 0;
//   clang_getSpellingLocation(location, &file, &line, &col, &offset);
//   return getClangFileName(file);
// }

// std::string unquoteForeignString(std::string s) {
//   return std::regex_replace(std::regex_replace(s, std::regex("\\\\"),
//   "\\\\"),
//                             std::regex("\""), "\\\"");
// }

// std::string getExtent(CXSourceRange range, CXTranslationUnit * tup) {
//   CXToken * tokens = 0;
//   unsigned int nTokens = 0;
//   clang_tokenize(*tup, range, &tokens, &nTokens);
//   std::stringstream out;
//   for (size_t i = 0; i < nTokens; ++i) {
//     CXString cxtoken = clang_getTokenSpelling(*tup, tokens[i]);
//     out << clang_getCString(cxtoken) << " ";
//     clang_disposeString(cxtoken);
//   }
//   clang_disposeTokens(*tup, tokens, nTokens);
//   // sanitize quotes (i hate the std::regex quoting rules)
//   return "\"" + unquoteForeignString(out.str()) + "\"";
// }

// bool cursorEquals(CXCursor a, CXCursor b) {
//   return (a.data[1] == b.data[1] and a.data[2] == b.data[2]);
// }

// std::string getCursorPointers(CXCursor cursor) {
//   std::stringstream s;
//   s << cursor.data[0] << "," << cursor.data[1] << "," << cursor.data[2];
//   return s.str();
// }

// std::tuple<TreeMotion, size_t> getTypeOfTreeMotion(CXCursor parent,
//                                                    CXCursor current) {
//   TreeMotion retval;
//   size_t numPops = 0;
//   if (cursorEquals(prev_cursor, parent)) {
//     retval = TreeMotion::Child;
//   } else if (cursorEquals(prev_parent, parent)) {
//     retval = TreeMotion::Sibling;
//     ast_stack.pop();
//     ++numPops;
//   } else {
//     while (not cursorEquals(parent, ast_stack.top())) {
//       ast_stack.pop();
//       ++numPops;
//     }
//     retval = TreeMotion::HigherSibling;
//   }
//   ast_stack.push(current);
//   prev_cursor = current;
//   prev_parent = parent;
//   return std::tuple<TreeMotion, size_t>(retval, numPops);
// }

// // dumps out each element of the tree
// enum CXChildVisitResult visit(CXCursor cursor, CXCursor parent,
//                               CXClientData client_data
//                               __attribute__((unused))) {
//   std::string fromFile = getFile(clang_getCursorLocation(cursor));
//   if (do_parse_other_files or fromFile == infile_str) {
//     TreeMotion typeOfMotion;
//     size_t numPops;
//     std::tie(typeOfMotion, numPops) = getTypeOfTreeMotion(parent, cursor);
//     CXString cxcursorStr = clang_getCursorSpelling(cursor);
//     CXString cxcursorKindStr =
//      clang_getCursorKindSpelling(clang_getCursorKind(cursor));
//     if (TreeMotion::Child == typeOfMotion) {
//       std::cout << " :children" << std::endl;
//       for (size_t i = 0; i < ast_stack.size() - 1; ++i) {
//         std::cout << " ";
//       }
//       std::cout << "((";
//     } else if (TreeMotion::Sibling == typeOfMotion) {
//       std::cout << ")" << std::endl;
//       for (size_t i = 0; i < ast_stack.size() - 1; ++i) {
//         std::cout << " ";
//       }
//       std::cout << "(";
//     } else if (TreeMotion::HigherSibling == typeOfMotion) {
//       for (size_t i = 0; i < CLOSE_DELIMS(numPops); ++i) {
//         std::cout << ")";
//       }
//       std::cout << std::endl;
//       for (size_t i = 0; i < ast_stack.size() - 1; ++i) {
//         std::cout << " ";
//       }
//       std::cout << "(";
//     }
//     CXSourceRange extent = clang_getCursorExtent(cursor);
//     std::cout << ":|name| "
//               << "\"" << unquoteForeignString(clang_getCString(cxcursorStr))
//               << "\""
//               << " :|type| "
//               // || pipes are because types can have spaces
//               << "'|" << clang_getCString(cxcursorKindStr) << "| :|file| "
//               << "\"" << fromFile << "\""
//               << " :|text| " << getExtent(extent, infile_ast)
//               << " :|extent| (:|range-start| "
//               << getLocation(clang_getRangeStart(extent)) << " :|range-end| "
//               << getLocation(clang_getRangeEnd(extent)) << ")";
//   }
//   return CXChildVisit_Recurse;
// }
