:- module(instance_orchestrator, [
  (:=)/2,
  op(500, xfy, (:=))
]).

:- use_module('../factory/instance_factory').
:- use_module('../structure/header').

':='(InstanceReference, Expression) :-
  header(Expression, ClassName, Arguments), !,
  instance_class(ClassName, Arguments, InstanceReference), !.
