# remu-scope
A framework to do name analysis for programming languages. 

Focus on this sort of scopes:

- new names can only be introduced by constructs like `let` and function arguments.
- a name can only be binded once(note that still work fine for `ref` type)

A language with this sort of scopes suffices all of the mutating semantics(e.g., `ref` by ML languages),
while keeping the scope in the most simple level.

As for this package, if a language's scopes suffice above features, it works,
and also can be used for closure conversions.

The awareness of differences between scopes by `let` and scopes by functions is missing,
due to the differences are in fact produced by a concrete implementation(`let` can be implemented with either immediate lambda or name mangling/gensym).
