:- module(sandbox, [
  setup_sandbox/1,
  teardown_sandbox/1
]).

:- use_module('../operators/reference_operators').
:- use_module('../manager/context_manager').
:- use_module(method_sandbox).

setup_sandbox(ContextReference) :-
  get_context(ContextReference, Context),
  context{
    reference: &Module,
    methods: Methods,
    constants: Constants,
    exports: Exports
  } :< Context,
  setup_constants(Module, Constants),
  setup_methods(Module, Methods),
  setup_exports(Module, Exports), !.

setup_constants(_, []).
setup_constants(Module, [Constant|Constants]) :-
  assertz(Module:Constant),
  setup_constants(Module, Constants).

setup_methods(_, []).
setup_methods(Module, [Method|Methods]) :-
  sandbox_method(Module, Method),
  setup_methods(Module, Methods).

setup_exports(_, []).
setup_exports(Module, [Export|Exports]) :-
  call(Module:export(Export)),
  setup_exports(Module, Exports).

teardown_sandbox(ContextReference) :-
  get_context(ContextReference, Context),
  context{
    reference: &Module,
    methods: Methods,
    constants: Constants
  } :< Context,
  teardown_constants(Module, Constants),
  teardown_methods(Module, Methods),
  abolish_module_tables(Module), !.

teardown_constants(_, []).
teardown_constants(Module, [Constant|Constants]) :-
  Constant =.. [Name|_],
  retractall(Module:Name),
  teardown_constants(Module, Constants).

teardown_methods(_, []).
teardown_methods(Module, [Method|Methods]) :-
  method{ name: Name } :< Method,
  retractall(Module:Name),
  teardown_methods(Module, Methods).
