:- module(executor, [
  execute_method/4
]).

execute_method(ClassMethods, Instance, MethodName, Arguments) :-
  Instance =.. [_, _, InstanceAttributes],
  setup_methods(ClassMethods),
  setup_context(InstanceAttributes),
  get_method(ClassMethods, MethodName, Arguments, Method), !,
  call_method(Method, Arguments),
  teardown_context(InstanceAttributes),
  teardown_methods(ClassMethods).

setup_methods([]).
setup_methods([Method|Methods]) :-
  Method =.. [':-', Header, _],
  Header =.. [Name|Arguments],
  length(Arguments, Arity),
  dynamic(Name/Arity),
  assertz(Method),
  setup_methods(Methods).

setup_context([]).
setup_context([Parameter|Context]) :-
  Parameter =.. [Name, _],
  dynamic(Name/1),
  assertz(Parameter),
  setup_context(Context).

get_method(Methods, Name, Arguments, Method) :-
  length(Arguments, Arity),
  get_method(Methods, Name/Arity, Method).

get_method([], Name/Arity, _) :-
  format("Could not find method ~w/~w in provided class.", [Name, Arity]),
  fail.
get_method([Method|_], Name/Arity, Method) :-
  Method =.. [':-', Header, _],
  Header =.. [Name|Arguments],
  length(Arguments, Arity).  
get_method([_|Methods], MethodName, Method) :-
  get_method(Methods, MethodName, Method).

call_method(Method, Arguments) :-
  Method =.. [':-', Header, _],
  Header =.. [Name|_],
  CallableMethod =.. [Name|Arguments],
  call(CallableMethod).

teardown_methods([]).
teardown_methods([Method|Methods]) :-
  Method =.. [':-', Header, _],
  Header =.. [Name|Arguments],
  length(Arguments, Arity),
  abolish(Name/Arity),
  teardown_methods(Methods).

teardown_context([]).
teardown_context([Parameter|Context]) :-
  Parameter =.. [Name, _],
  abolish(Name/1),
  teardown_context(Context).
