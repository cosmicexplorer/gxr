#ifndef UTILITIES_HPP
#define UTILITIES_HPP

#include <ostream>
#include <string>
#include <map>
#include <boost/algorithm/string.hpp>
#include <cassert>

namespace semantic_code_browser {
namespace utilities {
struct unparsed_enum_exception : std::runtime_error {
  unparsed_enum_exception(std::string s) : std::runtime_error(s) {
  }
};

std::string & RemoveOneLeadingWhitespace(std::string & s) {
  if (!s.empty() and s[0] == ' ') {
    s = s.substr(1);
  }
  return s;
}

template <typename T, typename... Args>
std::map<T, std::string> * ParseEnumMap(std::string token_str, Args... enums) {
  std::vector<std::string> str_vec;
  std::vector<T> enum_vec = {enums...};
  // TODO: check if the token string has spaces
  boost::split(str_vec, token_str, boost::is_any_of(","));
  assert(str_vec.size() == enum_vec.size());
  size_t num_enums = enum_vec.size();
  auto * m = new std::map<T, std::string>();
  for (size_t ind = 0; ind < num_enums; ++ind) {
    m->insert(
     std::make_pair(enum_vec[ind], RemoveOneLeadingWhitespace(str_vec[ind])));
  }
  return m;
}

template <typename T>
T MakeFromCursor(CXCursor) {
  static_assert(false);
}
}
}

// ~~the most unhygienic of macros~~
// no do-while because this needs to be in the same scope (so you probably
// shouldn't be trying to use this with c++11 inline structs...but the function
// Destroy() is there in case you still want to)
#define MAKE_ENUM_STRUCT(NAME, VALS...)                                      \
  enum NAME##_enum{VALS};                                                    \
  struct NAME {                                                              \
   private:                                                                  \
    static std::map<NAME##_enum, std::string> * enum_map;                    \
    static std::string convert_##NAME(NAME##_enum arg) {                     \
      return enum_map->at(arg);                                              \
    }                                                                        \
    static NAME##_enum parse_from_string(std::string s) {                    \
      for (auto & entry : *enum_map) {                                       \
        if (entry.second == s) {                                             \
          return entry.first;                                                \
        }                                                                    \
      }                                                                      \
      throw new semantic_code_browser::utilities::unparsed_enum_exception(   \
       "enum  " #NAME " was not parsed correctly from the string \"" + s +   \
       "\"");                                                                \
    }                                                                        \
    static NAME##_enum parse_from_cursor(CXCursor);                          \
    NAME##_enum mEnum;                                                       \
    std::string mString;                                                     \
                                                                             \
   public:                                                                   \
    static void Destroy() {                                                  \
      delete enum_map;                                                       \
    }                                                                        \
    NAME() : NAME(NAME##_enum()) {                                           \
    }                                                                        \
    NAME(NAME##_enum e) : mEnum(e), mString(convert_##NAME(mEnum)) {         \
    }                                                                        \
    NAME(std::string s) : NAME(parse_from_string(s)) {                       \
    }                                                                        \
    NAME(CXCursor c) : NAME(parse_from_cursor(c)) {                          \
    }                                                                        \
    std::string toString() const {                                           \
      return mString;                                                        \
    }                                                                        \
    bool operator==(NAME##_enum arg) const {                                 \
      return mEnum == arg;                                                   \
    }                                                                        \
    bool operator!=(NAME##_enum arg) const {                                 \
      return not(*this == arg);                                              \
    }                                                                        \
    bool operator==(const NAME & rhs) const {                                \
      return mEnum == rhs.mEnum;                                             \
    }                                                                        \
    bool operator!=(const NAME & rhs) const {                                \
      return not(*this == rhs);                                              \
    }                                                                        \
    NAME & operator=(const NAME & rhs) {                                     \
      mEnum = rhs.mEnum;                                                     \
      mString = rhs.mString;                                                 \
      return *this;                                                          \
    }                                                                        \
    NAME & operator=(NAME##_enum arg) {                                      \
      mEnum = arg;                                                           \
      mString = convert_##NAME(arg);                                         \
      return *this;                                                          \
    }                                                                        \
  };                                                                         \
  std::map<NAME##_enum, std::string> * NAME::enum_map =                      \
   semantic_code_browser::utilities::ParseEnumMap<NAME##_enum>(#VALS, VALS); \
  std::ostream & operator<<(std::ostream & stream, const NAME & obj) {       \
    stream << obj.toString();                                                \
    return stream;                                                           \
  }

#endif /* UTILITIES_HPP */
