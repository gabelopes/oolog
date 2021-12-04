:- module(reference_manager, [
  get_object/2,
  update_object/1
]).

:- use_module(class_manager).
:- use_module(instance_manager).

get_object(Reference, Object) :-
  get_class(Reference, Object).
get_object(Reference, Object) :-
  get_instance(Reference, Object).

update_object(Object) :-
  Type{} :< Object,
  update_object(Type, Object).

update_object(class, UpdatedClass) :-
  update_class(UpdatedClass).
update_object(instance, UpdatedInstance) :-
  update_instance(UpdatedInstance).
