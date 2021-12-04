:- module(context_manager, [
  get_context/2,
  register_context/2,
  deregister_context/1
]).

:- use_module('../operators/reference_operators').
:- use_module(reference_manager).

:- dynamic context/2.

get_context(&ContextReference, Context) :-
  context(ContextReference, Context).

register_context(Context, &ContextReference) :-
  create_reference(ContextReference),
  UniqueContext = Context.put(_{
    reference: &ContextReference
  }),
  assertz(context(ContextReference, UniqueContext)).

deregister_context(&ContextReference) :-
  retractall(context(ContextReference, _)).
