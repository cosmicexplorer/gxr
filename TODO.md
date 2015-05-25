TODO
====

(in topological order)

1. do types and variables in c with multiple files
2. add static variables
3. make preprocessor macros work (be able to read their expansion as well)
4. add struct/class member decls/refs (treat each one like it's its own variable?? maybe)
5. add conditional compilation support (have additional scope: within given #if)
6. add aliases (typedefs and pointer/reference (the pointer/reference thing could be super cool!!!!!))
7. add labelstmt and labelref to decls/refs
  - don't think there's an alias analogy; unless multiple labels go to the same place (lol)
8. extend to c++ (templates, namespaces, virtual functions!)
9. add client/server architecture (or something similarly easy to integrate with makefiles)
  - remove requirement to include clang headers, or at len  ast automate it
10. integrate with doxygen comments
11. produce groff or something that emacs can browse super easy (and give it links to the source so it can be used (kinda) like gtags)
12. extend to other languages?
  - python, ruby, java, lisp...

# indexing

- use sql or other database to store results from files, then read from db

## now

- file
- offset
- line
- col
- whether is decl/ref/defn/call
  - defn is decl
    - unless we count assignment as a definition (which i like)
  - call is ref
  - these two distinctions can be worked out by the backend which queries the db
- whether is variable/function/scope/label/type
- if is variable/function, then type
  - this can be extended later to types, by saying whether they are a simple type, or whether they are a class/struct
  - labels and scopes would not use this column
- language
- name (can be anon)
- semantic scope

## later

- linkage (extern/static/normal)
  - c++ implicitly treats const namespace-scope variables as having internal (static) linkage, unlike c
  - this shouldn't be a problem, because libclang can get linkage super easily anyway
- #if scope
- namespaces
- structs/classes
- aliases
