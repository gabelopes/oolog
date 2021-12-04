:- module(method_orchestrator, [
  (::)/2,
  op(500, xfy, (::))
]).

:- use_module('../../platform/operators/reference_operators').
:- use_module('../manager/class_manager').
:- use_module('../executor/method_executor').
:- use_module('../lifecycle/exception').
:- use_module('../structure/header').

% TODO With new header structure, analyze allowing null arity methods to be called without ().

'::'(&Reference, Expression) :-
  !,
  (
    compound(Expression) ->
      header(Expression, MethodName, Arguments);
      raise_exception("Expression '~w' is not an invocation. Use '~w()' for null arity.", [Expression, Expression])
  ), !,
  invoke_method(&Reference, MethodName, Arguments), !.
'::'(ClassName, Expression) :-
  get_class(ClassName, ClassDefinition), !,
  class{ reference: ClassReference } :< ClassDefinition,
  '::'(ClassReference, Expression).
'::'(Object, Expression) :-
  raise_exception("Cannot invoke ~w over ~w. Object is not a reference or class.", [Expression, Object]).
