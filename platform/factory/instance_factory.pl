:- module(instance_factory, [
  instance_class/3
]).

:- use_module(library(unison)).
:- use_module('../processor/class_processor').
:- use_module('../processor/attributes_processor').
:- use_module('../manager/class_manager').
:- use_module('../manager/instance_manager').
:- use_module('../factory/accessor_factory').
:- use_module('../executor/constructor_executor').

instance_class(Name, ConstructorArguments, InstanceReference) :-
  get_class(Name, Class),
  create_instance(Class, InstanceReference, Instance),
  register_instance(Instance, InstanceReference), !,
  invoke_constructor(InstanceReference, ConstructorArguments), !.

create_instance(Class, InstanceReference, instance{
  class: ClassReference,
  accessors: Accessors,
  data: data{
    attributes: Attributes
  }
}) :-
  class{ reference: ClassReference, attributes: AttributeDefinitions } :< Class,
  find_attributes_by_scope(AttributeDefinitions, [prototype], PrototypeAttributes),
  allocate_attributes(InstanceReference, PrototypeAttributes, Attributes),
  create_accessors(InstanceReference, PrototypeAttributes, Accessors).
