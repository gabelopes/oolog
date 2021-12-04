:- module(instance_manager, [
  get_instance/2,
  register_instance/2,
  update_instance/1
]).

:- use_module('../operators/reference_operators').
:- use_module('../io/console').

:- dynamic instance/2.

get_instance(&Reference, Instance) :-
  instance(Reference, Instance).

register_instance(Instance, &Reference) :-
  uuid(Reference),
  UniqueInstance = Instance.put(_{
    reference: &Reference
  }),
  assertz(instance(Reference, UniqueInstance)).

update_instance(UpdatedInstance) :-
  instance{ reference: &Reference } :< UpdatedInstance,
  instance(Reference, _),
  retractall(instance(Reference, _)),
  assertz(instance(Reference, UpdatedInstance)).
update_instance(_) :-
  write_warning("Could not update reference for instance. No reference found.").
