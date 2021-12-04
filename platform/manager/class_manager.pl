:- module(class_manager, [
  get_class/2,
  register_class/1,
  register_class/2,
  update_class/1
]).

:- use_module('../operators/reference_operators').
:- use_module('../io/console').
:- use_module(reference_manager).

:- dynamic class/2.

get_class(&ClassReference, ClassDefinition) :-
  !, class(ClassReference, ClassDefinition).

get_class(Package:Name, ClassDefinition) :-
  !, class(_, ClassDefinition),
  class{ package: Package, name: Name } :< ClassDefinition.
get_class(Name, ClassDefinition) :-
  get_class(default:Name, ClassDefinition).

register_class(ClassDefinition) :-
  register_class(ClassDefinition, _).

register_class(ClassDefinition, &ClassReference) :-
  \+ exists_class(ClassDefinition),
  create_reference(ClassReference),
  UniqueClassDefinition = ClassDefinition.put(_{
    reference: &ClassReference
  }),
  assertz(class(ClassReference, UniqueClassDefinition)).

exists_class(ClassDefinition) :-
  class{ package: Package, name: Name } :< ClassDefinition,
  get_class(Package:Name, _).

update_class(UpdatedClass) :-
  class{ reference: &ClassReference } :< UpdatedClass,
  class(ClassReference, _),
  retractall(class(ClassReference, _)),
  assertz(class(ClassReference, UpdatedClass)).
update_class(UpdatedClass) :-
  class{ package: Package, name: Name } :< UpdatedClass,
  write_warning("Could not update reference for class ~w:~w. No reference found.~n", [Package, Name]).
