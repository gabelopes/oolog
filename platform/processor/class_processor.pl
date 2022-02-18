:- module(class_processor, [
  find_constructor/3,
  find_constructor/4
]).

:- use_module(library(unison)).
:- use_module('../processor/arity_processor').

find_constructor(ClassDefinition, Arity, Constructor) :-
  find_constructor(ClassDefinition, [public, protected, private], Arity, Constructor).

find_constructor(ClassDefinition, Visibilities, Arity, Constructor) :-
  class{ constructors: Constructors } :< ClassDefinition, !,
  member(Constructor, Constructors),
  method{
    name: constructor,
    arity: ConstructorArity,
    modifiers: modifiers{
      visibility: Visibility
    }
  } :<< Constructor,
  member(Visibility, Visibilities),
  Arity >@< ConstructorArity.
