:- module(instance_orchestrator, [
  (:=)/2,
  op(500, xfy, (:=))
]).

:- use_module('../factory/instance_factory').

':='(Reference, Expression) :-
  compound_name_arguments(Expression, ClassName, Arguments),
  instance_class(ClassName, Arguments, Reference).
