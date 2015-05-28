#include <iostream>
#include <string>
#include <map>
#include <cassert>

struct test {
  int _i;
  test() : test(int()) {
  }
  test(int i) : _i(i) {
  }
};

#include <boost/algorithm/string.hpp>

enum _a { var, fun };

std::string & remove_leading_whitespace(std::string & s) {
  if (!s.empty() and s[0] == ' ') {
    s = s.substr(1);
  }
  return s;
}

template <typename a, typename... Args>
std::map<a, std::string> * parse_map(std::string s, Args... args) {
  std::vector<std::string> v;
  std::vector<_a> v2 = {args...};
  boost::split(v, s, boost::is_any_of(","));
  assert(v.size() == v2.size());
  size_t vs = v.size();
  auto * m = new std::map<a, std::string>();
  for (size_t s = 0; s < vs; ++s) {
    m->insert(std::make_pair(v2[s], remove_leading_whitespace(v[s])));
  }
  return m;
}

struct a {
 private:
  static std::map<_a, std::string> * mAMap;
  static std::string convert__a(_a arg) {
    return mAMap->at(arg);
  }

 public:
  std::string mA;
  _a m_A;
  a(_a arg) : mA(convert__a(arg)), m_A(arg) {
  }
  std::string toString() {
    return mA;
  }
  bool operator==(_a arg) {
    return m_A == arg;
  }
};

std::map<_a, std::string> * a::mAMap = parse_map<_a>("var, fun", var, fun);

#define test(X...) #X

#include "utilities.hpp"

MAKE_ENUM_STRUCT(wreckage, bad, horrible, awful);
// expansion of above:

int main() {
  a b(fun);
  wreckage w(horrible);
  std::cout << w << std::endl;
  std::cout << b.toString() << std::endl;
  std::cout << test(a, s, d, f) << std::endl;
}
