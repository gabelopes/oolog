:- module(reference_manager, [
  create_reference/1,
  get_object/2,
  update_object/1
]).

:- use_module('../../platform/operators/reference_operators').
:- use_module('../../platform/orchestrator/method_orchestrator').
:- use_module('../io/console').
:- use_module(class_manager).
:- use_module(instance_manager).
:- use_module(context_manager).

create_reference(Reference) :-
  uuid(Reference).

get_object(Reference, Object) :-
  get_class(Reference, Object).
get_object(Reference, Object) :-
  get_instance(Reference, Object).
get_object(Reference, Object) :-
  get_instance(Reference, Object).

update_object(Object) :-
  Type{} :< Object,
  update_object(Type, Object).

update_object(class, UpdatedClass) :-
  update_class(UpdatedClass).
update_object(instance, UpdatedInstance) :-
  update_instance(UpdatedInstance).
update_object(Type, _) :-
  write_warning("Cannot update '~w' references.~n", [Type]).

% Reference Portrayal
user:portray(&Reference) :-
  get_object(&Reference, Object),
  Type{} :< Object,
  portray_object(Type, &Reference, Object, Portrayal),
  print(Portrayal).

portray_object(instance, &Reference, Instance, Portrayal) :-
  instance{ class: ClassReference } :< Instance,
  get_class(ClassReference, Class),
  class{ name: ClassName } :< Class,
  format(atom(Portrayal), "~w@~w", [ClassName, Reference]).
portray_object(class, _, Class, class(Name)) :-
  class{ name: Name } :< Class.
portray_object(context, _, Context, context(BindingPortrayal)) :-
  context{ binding: Binding } :< Context,
  get_object(Binding, Object),
  Type{ reference: Reference } :< Object,
  portray_object(Type, Reference, Object, BindingPortrayal).
