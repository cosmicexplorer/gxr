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

# indexing

we want:

- for each entity, have listings (ordered by file, then offset):
  - decls
  - refs
  - defn(s)
  - aliases

- cursors indexed by:
  - file
  - offset / line & col
  - #if scope
  - semantic scope (namespace/class/struct)
  - name (can be anon)
  - aliases
  - whether is decl/ref/defn
  - whether is type/val/scope/label
    - language-specific entities: variable/function/scope/label
  - language
  - linkage (extern/static/normal)
    - c++ implicitly treats const namespace-scope variables as having internal (static) linkage, unlike c
    - this shouldn't be a problem, because libclang can get linkage super easily anyway
