#ifndef STACK_H
#define STACK_H

#include <stdexcept>

namespace SCB {

template <typename T>
class Stack {
  struct Node {
    T data;
    Node * prev;
    Node * next;
    Node(T item, Node * p, Node * n) : data(item), prev(p), next(n) {
    }
  };

  Node * tail;
  size_t length;

 public:
  Stack() : tail(nullptr), length(0) {
  }
  ~Stack() {
    while (!empty()) {
      pop();
    }
  }

  void push(T item) {
    if (tail) {
      tail->next = new Node(item, tail, nullptr);
      tail = tail->next;
    } else {
      tail = new Node(item, nullptr, nullptr);
    }
    ++length;
  }

  T top() {
    if (empty()) {
      throw std::logic_error("stack size is 0");
    }
    return tail->data;
  }

  void pop() {
    if (empty()) {
      throw std::logic_error("stack size is 0");
    }
    if (tail->prev) {
      tail = tail->prev;
      delete tail->next;
      tail->next = nullptr;
    } else {
      delete tail;
      tail = nullptr;
    }
    --length;
  }

  size_t size() {
    return length;
  }

  bool empty() {
    return length == 0;
  }
};
}

#endif /* STACK_H */
