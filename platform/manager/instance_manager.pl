:- module(instance_manager, [
  get_instance/2,
  register_instance/2,
  update_instance/1
]).

:- use_module('../operators/reference_operators').
:- use_module('../io/console').
:- use_module(reference_manager).

:- dynamic instance/2.

get_instance(&InstanceReference, Instance) :-
  instance(InstanceReference, Instance).

register_instance(Instance, &InstanceReference) :-
  create_reference(InstanceReference),
  UniqueInstance = Instance.put(_{
    reference: &InstanceReference
  }),
  assertz(instance(InstanceReference, UniqueInstance)).

update_instance(UpdatedInstance) :-
  instance{ reference: &InstanceReference } :< UpdatedInstance,
  instance(InstanceReference, _),
  retractall(instance(InstanceReference, _)),
  assertz(instance(InstanceReference, UpdatedInstance)).
update_instance(_) :-
  write_warning("Could not update reference for instance. No reference found.").
