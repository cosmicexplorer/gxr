// http://www.ibm.com/developerworks/library/os-createcompilerllvm2/
#include <iostream>

// #include <clang-c/Index.h>
#include <llvm/Option/Arg.h>
#include <llvm/Option/ArgList.h>
#include <clang/Frontend/CompilerInstance.h>
#include <clang/Lex/Preprocessor.h>
#include <clang/Basic/TargetInfo.h>

int main(int argc, char ** argv) {
  if (argc < 2) {
    std::cerr << "lol provide an input file" << std::endl;
    exit(1);
  }
  clang::CompilerInstance ci;
  ci.createDiagnostics(
   new clang::ForwardingDiagnosticConsumer(ci.getDiagnosticClient()), true);
  ci.createFileManager();
  ci.createSourceManager(ci.getFileManager());
  ci.createPreprocessor(clang::TU_Complete);
  const clang::FileEntry * pFile = ci.getFileManager().getFile(argv[1]);
  ci.getSourceManager().createFileID(pFile, clang::SourceLocation(),
                                     clang::SrcMgr::C_User);
  ci.getPreprocessor().EnterMainSourceFile();
  ci.getDiagnosticClient().BeginSourceFile(ci.getLangOpts(),
                                           &ci.getPreprocessor());
  clang::Token tok;
  do {
    ci.getPreprocessor().Lex(tok);
    if (ci.getDiagnostics().hasErrorOccurred())
      break;
    ci.getPreprocessor().DumpToken(tok);
    std::cerr << std::endl;
  } while (tok.isNot(clang::tok::eof));
  ci.getDiagnosticClient().EndSourceFile();
}
