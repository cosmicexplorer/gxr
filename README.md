semantic-code-browser
=====================

Who likes coding? ("WE DO!!!" the crowd screams.)
Who likes writing programs that do lots of cool things? ("WE DO!!!" The throng of coders intensifies.)
Now who likes reading through complex code that does a lot of things? (Crowd quiets down.)
Who likes jumping through three different levels of preprocessor macros before you get to the actual function call? (Murmurs among the crowd.)
Who likes finding out the global variable you thought was protected by a mutex is actually modified unprotected in a temporary background thread? (The crowd starts jeering. A lone college student yells out "NOBODY!!!")

You're all completely right. Nobody likes any of that. But for some reason we continue to write code anyway, and if we're going to do it, we might as well have some tools to avoid all that nonsense.

# INTRO

semantic-code-browser (the name is boring, I'll fix that at some point) is a utility to generate static HTML documentation from source files. It parses source files (currently using [libclang](http://clang.llvm.org/doxygen/group__CINDEX.html)) and produces an index of declarations, definitions, and reference to all entities (functions, variable, types, etc.) in the set of files (with some restrictions; see [instructions](#INSTRUCTIONS)), a search utility, and a file browser very much like [lxr](http://lxr.free-electrons.com/). It's currently limited to C, although I plan to expand that soon.

I wanted to write this because while lxr is cool, it gets a few things wrong (try searching for the definition of [sk_buff](http://lxr.free-electrons.com/ident?i=sk_buff), for example), and while I haven't checked out the source too much (mostly because it's not up to date and there's very little documentation), I suspect it's not really parsing the C code, mostly using regular expressions or some other not-quite-good-enough construct. Thankfully, since lxr was written, libclang has come along, and while its documentation is also poor, there's at least enough doxygen in the source to get by. libclang also has the benefit of being able to parse C++ (and Objective-C, lol). That leads to the possibility of creating a language-agnostic code browsing solution, using the best of today's parsing tools.

Hopefully you'll enjoy using it as much as I did writing it.

# PROGRESS

I would say "pre-alpha," except there's literally no functionality yet, so I can't even say that. Check out our [TODO](TODO.md) to see where we are and where we're going (phew, that's a lot of wheres).

# INSTRUCTIONS

`make` and `make check` do what you'd expect. The interface is still getting hammered out, so check back later for more information.
