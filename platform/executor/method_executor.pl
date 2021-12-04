:- module(method_executor, [
  invoke_method/3
]).

:- use_module('../operators/reference_operators').
:- use_module('../factory/context_factory').

invoke_method(ObjectReference, Name, Arguments) :-
  setup_call_cleanup(
    load_context(ObjectReference, ContextReference),
    execute_method(ContextReference, Name, Arguments),
    unload_context(ContextReference)
  ).

execute_method(&ContextReference, Name, Arguments) :-
  Head =.. [Name|Arguments],
  Method =.. [':', ContextReference, Head], !,
  call(Method), !.
