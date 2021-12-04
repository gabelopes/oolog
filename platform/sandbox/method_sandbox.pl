:- module(method_sandbox, [
  sandbox_method/2,
  unify_arguments/2,
  store_arguments/2,
  retrieve_arguments/2
]).

:- use_module('../manager/reference_manager').
:- use_module('../structure/header').

:- dynamic execution/2.

sandbox_method(Module, Method) :-
  method{ name: Name, arguments: Arguments, arity: Arity, body: Body } :< Method,
  setup_arguments_proxy(Arity, ArgumentsProxy),
  header(Header, Name, ArgumentsProxy),
  setup_body_proxy(Header, Arguments, Body, BodyProxy),
  MethodProxy =.. [':-', Module:Header, Module:BodyProxy],
  assertz(MethodProxy), !.

setup_arguments_proxy(Arity, ArgumentsProxy) :-
  setup_arguments_proxy(Arity, 0, ArgumentsProxy).

setup_arguments_proxy(Arity, Arity, []).
setup_arguments_proxy(Arity, Index, [_|ArgumentsProxy]) :-
  NextIndex is Index + 1,
  setup_arguments_proxy(Arity, NextIndex, ArgumentsProxy).

% TODO For null arity methods, proxy does not need to create execution and retrieve.
% TODO Find a better way to store Arguments, than with dynamic.

setup_body_proxy(Header, Arguments, Body, Proxy) :-
  Proxy = (
    reference_manager:create_reference(ExecutionReference),
    \+ (
      (method_sandbox:unify_arguments(Header, Arguments), !),
      (Body, !),
      (method_sandbox:store_arguments(Header, ExecutionReference), !),
      fail
    ),
    method_sandbox:retrieve_arguments(ExecutionReference, Header)
  ).

unify_arguments(Header, Arguments) :-
  header(Header, _, Arguments).

store_arguments(Header, ExecutionReference) :-
  header(Header, _, Arguments),
  assertz(execution(ExecutionReference, Arguments)).

retrieve_arguments(ExecutionReference, Header) :-
  header(Header, _, Arguments),
  execution(ExecutionReference, Arguments),
  retractall(execution(ExecutionReference, _)).
