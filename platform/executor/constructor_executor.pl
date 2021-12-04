:- module(constructor_executor, [
  invoke_constructor/2
]).

:- use_module(library(unison)).
:- use_module('../operators/reference_operators').
:- use_module('../factory/context_factory').
:- use_module('../manager/reference_manager').
:- use_module('../lifecycle/exception').
:- use_module('../processor/accessor_processor').

invoke_constructor(InstanceReference, Arguments) :-
  validate_instance(InstanceReference),
  setup_call_cleanup(
    load_context(InstanceReference, [constructors, accessors, methods], ContextReference),
    (
      execute_constructor(ContextReference, Arguments), !,
      freeze_attributes(InstanceReference)
    ),
    unload_context(ContextReference)
  ).

validate_instance(Reference) :-
  get_object(Reference, Object),
  instance{} :< Object.
validate_instance(&Reference) :-
  raise_exception("Cannot invoke constructor on a non-instance object. (Reference: ~w)~n", [Reference]).

execute_constructor(&ContextReference, Arguments) :-
  Head =.. [constructor|Arguments],
  Constructor =.. [':', ContextReference, Head], !,
  call(Constructor), !.

freeze_attributes(InstanceReference) :-
  get_object(InstanceReference, Instance),
  instance{ data: data{ attributes: Attributes }} :<< Instance,
  dict_keys(Attributes, Names),
  freeze_attributes(InstanceReference, Names, Attributes).

freeze_attributes(_, [], _).
freeze_attributes(InstanceReference, [Name|Names], Attributes) :-
  get_dict(Name, Attributes, unset/allocate),
  access_attribute(InstanceReference, Name, none),
  freeze_attributes(InstanceReference, Names, Attributes).
freeze_attributes(InstanceReference, [_|Names], Attributes) :-
  freeze_attributes(InstanceReference, Names, Attributes).
