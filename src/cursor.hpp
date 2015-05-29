#ifndef CURSOR_HPP
#define CURSOR_HPP

// std includes
#include <list>
#include <string>
// external includes
#include <clang-c/Index.h>
// local includes
namespace semantic_code_browser {

namespace frontend {

namespace libclang_utils {

// disposes of input CXString; don't attempt to use or dispose the input after
// calling this
std::string GetStringAndDispose(CXString);
} /* libclang_utils */

namespace generic_utils {
// std::find yells at me about template arguments, so whatever
template <typename T, template <typename...> class Container>
bool is_in_container(Container<T> c, T val) {
  for (const auto & el : c) {
    if (el == val) {
      return true;
    }
  }
  return false;
}
}

namespace cursor_traits {

/*
  identifiers (in c and c++) are formatted according to the following regex:

  [a-zA-Z_][a-zA-Z_0-9]*
*/
extern const std::string IdentifierRegexString;

extern const std::regex IdentifierRegex;

bool IsValidIdentifier(std::string);

extern const std::list<std::string> CursorTypes;

extern const std::list<std::string> EntitySpecifiers;

extern const std::list<CXCursorKind> ScopeKinds;

// returns false if there are null characters
bool IsValidFilename(std::string);

/*
  scopes are formatted according to the following regex:

  (<filename>)?::(identifier::|identifier@)*

  where "filename" is a string identifying the file where the entity is located,
  if the entity has file-local (static) linkage. valid filenames are specified
  as above.

  "identifier" is the regex spelled out above. "::" denotes an enclosing
  namespace or class, while "@" denotes an enclosing function.
*/
extern const std::regex ScopeRegex;

bool GetValidScope(std::string);

} /* cursor_traits */

/*
 this class is not type-safe; it is meant to be easily serializable and
 parseable through boost::spirit. a checker function is provided; use it when
 required to ensure invalid values don't sneak into the strings used for
 individual data members.
*/
struct cursor {
 private:
  static std::tuple<std::string, unsigned int, unsigned int, unsigned int,
                    std::string, unsigned int, unsigned int, unsigned int>
   setup_locations(CXCursor);
  static std::string setup_cursor_type(CXCursor);
  static std::string setup_entity_spec(CXCursor);
  static std::string setup_type(CXCursor);
  static std::string setup_name(CXCursor);
  static std::string setup_scope(CXCursor);

 public:
  // index contents
  std::string begin_file;
  // libclang uses unsigned int for these; if it's good enough for them, it's
  // good enough for us
  unsigned int begin_offset;
  unsigned int begin_line;
  unsigned int begin_col;
  std::string end_file;
  unsigned int end_offset;
  unsigned int end_line;
  unsigned int end_col;
  std::string cursorType;
  std::string entitySpec;
  std::string type;
  std::string name;
  std::string scope;

  cursor(CXCursor);
  // not sure if this will end up being a good idea. it was confusing to have
  // copy constructors; i accidentally copy-constructed instead of constructing
  // from a CXCursor, for example. but it may be useful to have this. time will
  // tell.
  cursor(cursor &) = delete;

  /*
   if any of the strings don't evaluate to a valid choice for that particular
   element, this returns false. users of this code should probably throw an
   exception, because it means something probably went screwy during parsing
   (or there's a bug in this code; hopefully not)
  */
  bool isValid();

  // serializes to a line of csv (strings are unquoted because tokens cannot
  // have quotes in most languages)
  std::string toString();
}; /* cursor */
} /* frontend */
} /* semantic_code_browser */
#endif /* CURSOR_HPP */
