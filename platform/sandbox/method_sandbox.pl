:- module(method_sandbox, [
  sandbox_method/2,
  unify_arguments/2,
  store_arguments/2,
  retrieve_arguments/2
]).

:- dynamic execution/2.

sandbox_method(Module, Method) :-
  method{ name: Name, arguments: Arguments, arity: Arity, body: Body } :< Method,
  setup_arguments_proxy(Arity, ArgumentsProxy),
  Head =.. [Name|ArgumentsProxy],
  setup_body_proxy(Head, Arguments, Body, BodyProxy),
  MethodProxy =.. [':-', Module:Head, Module:BodyProxy],
  assertz(MethodProxy).

setup_arguments_proxy(Arity, ArgumentsProxy) :-
  setup_arguments_proxy(Arity, 0, ArgumentsProxy).

setup_arguments_proxy(Arity, Arity, []).
setup_arguments_proxy(Arity, Index, [_|ArgumentsProxy]) :-
  NextIndex is Index + 1,
  setup_arguments_proxy(Arity, NextIndex, ArgumentsProxy).

setup_body_proxy(Head, Arguments, Body, Proxy) :-
  Proxy = (
    uuid(ExecutionReference),
    \+ (
      (method_sandbox:unify_arguments(Head, Arguments), !),
      (Body, !),
      (method_sandbox:store_arguments(Head, ExecutionReference), !),
      fail
    ),
    method_sandbox:retrieve_arguments(ExecutionReference, Head)
  ).

unify_arguments(Head, Arguments) :-
  compound_name_arguments(Head, _, Arguments).

store_arguments(Head, ExecutionReference) :-
  compound_name_arguments(Head, _, Arguments),
  assertz(execution(ExecutionReference, Arguments)).

retrieve_arguments(ExecutionReference, Head) :-
  compound_name_arguments(Head, _, Arguments),
  execution(ExecutionReference, Arguments),
  retractall(execution(ExecutionReference, _)).
