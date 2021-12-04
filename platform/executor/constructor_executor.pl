:- module(constructor_executor, [
  invoke_constructor/2
]).

:- use_module(library(unison)).
:- use_module('../operators/reference_operators').
:- use_module('../factory/context_factory').
:- use_module('../manager/reference_manager').
:- use_module('../manager/class_manager').
:- use_module('../lifecycle/exception').
:- use_module('../processor/accessor_processor').
:- use_module('../processor/attributes_processor').
:- use_module('../structure/header').

invoke_constructor(InstanceReference, Arguments) :-
  validate_instance(InstanceReference),
  setup_call_cleanup(
    load_context(InstanceReference, [constructors, accessors, methods], ContextReference),
    (
      execute_constructor(ContextReference, Arguments), !,
      resolve_attributes(InstanceReference)
    ),
    unload_context(ContextReference)
  ).

validate_instance(Reference) :-
  get_object(Reference, Object),
  instance{} :< Object.
validate_instance(&Reference) :-
  raise_exception("Cannot invoke constructor on a non-instance object. (Reference: ~w)", [Reference]).

execute_constructor(&ContextReference, Arguments) :-
  header(Constructor, ContextReference:constructor, Arguments), !,
  call(Constructor), !.

resolve_attributes(InstanceReference) :-
  get_object(InstanceReference, Instance),
  instance{ class: ClassReference, data: data{ attributes: Attributes }} :<< Instance,
  get_class(ClassReference, ClassDefinition),
  class{ attributes: AttributesDefinitions } :< ClassDefinition,
  dict_keys(Attributes, Names),
  resolve_attributes(AttributesDefinitions, InstanceReference, Names, Attributes).

resolve_attributes(_, _, [], _).
resolve_attributes(AttributesDefinitions, InstanceReference, [Name|Names], Attributes) :-
  get_dict(Name, Attributes, allocate(InstanceReference)),
  find_attribute_by_name(AttributesDefinitions, Name, Attribute),
  attribute{ initial_value: InitialValue } :< Attribute,
  access_attribute(InstanceReference, Name, InitialValue),
  resolve_attributes(AttributesDefinitions, InstanceReference, Names, Attributes).
resolve_attributes(AttributesDefinitions, InstanceReference, [_|Names], Attributes) :-
  resolve_attributes(AttributesDefinitions, InstanceReference, Names, Attributes).
