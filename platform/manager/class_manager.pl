:- module(class_manager, [
  get_class/2,
  register_class/1,
  register_class/2,
  update_class/1
]).

:- use_module('../operators/reference_operators').
:- use_module('../io/console').

:- dynamic class/2.

get_class(&Reference, ClassDefinition) :-
  class(Reference, ClassDefinition).

get_class(Package:Name, ClassDefinition) :-
  !, class(_, ClassDefinition),
  class{ package: Package, name: Name } :< ClassDefinition.
get_class(Name, ClassDefinition) :-
  get_class(default:Name, ClassDefinition).

register_class(ClassDefinition) :-
  register_class(ClassDefinition, _).

register_class(ClassDefinition, &Reference) :-
  \+ exists_class(ClassDefinition),
  uuid(Reference),
  UniqueClassDefinition = ClassDefinition.put(_{
    reference: &Reference
  }),
  assertz(class(Reference, UniqueClassDefinition)).

exists_class(ClassDefinition) :-
  class{ package: Package, name: Name } :< ClassDefinition,
  get_class(Package:Name, _).

update_class(UpdatedClass) :-
  class{ reference: &Reference } :< UpdatedClass,
  class(Reference, _),
  retractall(class(Reference, _)),
  assertz(class(Reference, UpdatedClass)).
update_class(UpdatedClass) :-
  class{ package: Package, name: Name } :< UpdatedClass,
  write_warning("Could not update reference for class ~w:~w. No reference found.~n", [Package, Name]).

% Must be declared by Platform
:- register_class(class{
  package: default,
  name: object,
  attributes: [],
  methods: []
}).
