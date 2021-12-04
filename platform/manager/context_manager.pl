:- module(context_manager, [
  get_context/2,
  register_context/2,
  deregister_context/1
]).

:- use_module('../operators/reference_operators').

:- dynamic context/2.

get_context(&Reference, Context) :-
  context(Reference, Context).

register_context(Context, &Reference) :-
  uuid(Reference),
  UniqueContext = Context.put(_{
    reference: &Reference
  }),
  assertz(context(Reference, UniqueContext)).

deregister_context(&Reference) :-
  retractall(context(Reference, _)).
