:- module(method_orchestrator, [
  (::)/2,
  op(500, xfy, (::))
]).

:- use_module('../executor/method_executor').

'::'(Reference, Expression) :-
  compound_name_arguments(Expression, MethodName, Arguments),
  invoke_method(Reference, MethodName, Arguments).
