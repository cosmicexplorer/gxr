#ifndef UTILITIES_HPP
#define UTILITIES_HPP

#include <algorithm>
#include <functional>

/*
  templated functions to do stuff with containers (stuff that should already
  exist, but i digress)

  performance could probably be increased by making everything immutable like in
  clojure, both for compiler reasons and because in lisp everything is a
  pointer, so you don't have to keep reallocating everything constantly. but
  that's not super important.
*/

namespace semantic_code_browser {

namespace utilities {

template <typename T>
struct is_equal {
  T first;
  is_equal() = delete;
  is_equal(T input) : first(input) {
  }
  bool operator()(T second) {
    return std::equal_to<T>()(first, second);
  }
};

template <typename T, template <typename...> class Container,
          typename ContainerType, typename... ContainerArgs>
bool is_in_container(T val, Container<ContainerType, ContainerArgs...> c) {
  return std::any_of(std::begin(c), std::end(c), is_equal<ContainerType>(val));
}

template <template <typename...> class ReturnContainerType,
          typename... ReturnContainerArgs, typename T,
          template <typename...> class InputContainerType,
          typename... InputContainerArgs>
static ReturnContainerType<T, ReturnContainerArgs...>
 transform_container_given_input(
  InputContainerType<T, InputContainerArgs...> c,
  ReturnContainerType<T, ReturnContainerArgs...> out) {
  std::copy(std::begin(c), std::end(c), std::back_inserter(out));
  return out;
}

template <class ReturnContainerType,
          template <typename...> class InputContainerType, typename... InArgs,
          typename Func>
ReturnContainerType map_given_input(InputContainerType<InArgs...> c, Func f,
                                    ReturnContainerType out) {
  std::transform(std::begin(c), std::end(c), std::back_inserter(out), f);
  return out;
}

/*
  allow more performant overloads of templated functions, if the capabilities
  are available.
*/
typedef bool use_performant;

template <use_performant>
struct transformer;

template <>
struct transformer<false> {
  template <template <typename...> class ReturnContainerType,
            typename... ReturnContainerArgs, typename T,
            template <typename...> class InputContainerType,
            typename... InputContainerArgs>
  static ReturnContainerType<T, ReturnContainerArgs...>
   transform_container(InputContainerType<T, InputContainerArgs...> c) {
    return transform_container_given_input(
     c, ReturnContainerType<T, ReturnContainerArgs...>());
  }
  template <class ReturnContainerType,
            template <typename...> class InputContainerType, typename... InArgs,
            typename Func>
  static ReturnContainerType map(InputContainerType<InArgs...> c, Func f) {
    return map_given_input<ReturnContainerType>(c, f, ReturnContainerType());
  }
};

template <>
struct transformer<true> {
  template <template <typename...> class ReturnContainerType,
            typename... ReturnContainerArgs, typename T,
            template <typename...> class InputContainerType,
            typename... InputContainerArgs>
  static ReturnContainerType<T, ReturnContainerArgs...>
   transform_container(InputContainerType<T, InputContainerArgs...> c) {
    ReturnContainerType<T, ReturnContainerArgs...> ret;
    ret.reserve(c.size());
    return transform_container_given_input(c, ret);
  }
  template <class ReturnContainerType,
            template <typename...> class InputContainerType, typename... InArgs,
            typename Func>
  static ReturnContainerType map(InputContainerType<InArgs...> c, Func f) {
    ReturnContainerType ret;
    ret.reserve(c.size());
    return map_given_input<ReturnContainerType>(c, f, ret);
  }
};

/* TODO: provide alias to each function so that user doesn't have to type out
   transformer<true>:: each time */
} /* utilities */
} /* semantic_code_browser */

#endif /* UTILITIES_HPP */
