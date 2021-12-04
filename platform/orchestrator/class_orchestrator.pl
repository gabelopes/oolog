:- module(class_orchestrator, [
  class/1
]).

:- reexport('../operators/class_operators').
:- reexport('../operators/modifier_operators').
:- reexport('../operators/method_operators').

:- use_module('../factory/class_factory').
:- use_module('../manager/class_manager').

class(Header extends (SuperClass implements Methods)) :-
  compound_name_arguments(Header, Name, Attributes),
  get_class(SuperClass, SuperClassDefinition),
  load_class(default, Name, Attributes, SuperClassDefinition, Methods), !. % TODO Add package declarations
class(Header implements Methods) :-
  class Header extends object implements Methods, !.
class(Header) :-
  class Header implements [], !.
