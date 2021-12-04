:- module(context, [
  load_context/2,
  load_context/3,
  unload_context/1
]).

:- use_module('../structure/set').
:- use_module('../operators/reference_operators').
:- use_module('../manager/reference_manager').
:- use_module('../manager/class_manager').
:- use_module('../manager/context_manager').
:- use_module('../processor/methods_processor').
:- use_module('../processor/constants_processor').
:- use_module('../sandbox/context_sandbox').

load_context(ObjectReference, ContextReference) :-
  load_context(ObjectReference, [accessors, methods], ContextReference).

load_context(ObjectReference, Includes, ContextReference) :-
  create_context(ObjectReference, Includes, Context),
  register_context(Context, ContextReference),
  setup_sandbox(ContextReference).

unload_context(ContextReference) :-
  teardown_sandbox(ContextReference),
  deregister_context(ContextReference).

create_context(ObjectReference, Includes, Context) :-
  create_context(
    context{
      methods: [],
      constants: [],
      exports: [],
      imports: []
    },
    ObjectReference,
    Includes,
    Context
  ).

create_context(ParentContext, ObjectReference, Includes, Context) :-
  get_object(ObjectReference, Object),
  Type{} :< Object,
  create_object_context(Type, Object, Includes, ObjectContext),
  merge_contexts([ObjectContext, ParentContext], Context).

create_object_context(class, Class, Includes, context{
  methods: Methods,
  constants: [],
  exports: Exports,
  imports: []
}) :-
  class{
    methods: ClassMethods,
    accessors: Accessors
  } :< Class,
  find_methods(ClassMethods, [static], StaticMethods),
  select_methods(
    includes{
      accessors: Accessors,
      methods: StaticMethods
    },
    Includes,
    Methods
  ),
  find_methods_by_visibility(Methods, [public], PublicStaticMethods),
  create_exports(PublicStaticMethods, Exports).

create_object_context(instance, Instance, Includes, context{
  methods: Methods,
  constants: [],
  exports: Exports,
  imports: []
}) :-
  instance{
    class: ClassReference,
    accessors: Accessors
  } :< Instance,
  get_class(ClassReference, Class),
  class{
    constructors: Constructors,
    methods: ClassMethods
  } :< Class,
  find_methods_by_scope(ClassMethods, [prototype], PrototypeMethods),
  select_methods(
    options{
      constructors: Constructors,
      accessors: Accessors,
      methods: PrototypeMethods
    },
    Includes,
    Methods
  ),
  find_methods_by_visibility(Methods, [public], PublicPrototypeMethods),
  create_exports(PublicPrototypeMethods, Exports).

select_methods(_, [], []).
select_methods(AvailableMethods, [Include|Includes], Methods) :-
  select_methods(AvailableMethods, Includes, PartialMethods),
  get_dict(Include, AvailableMethods, CurrentMethods),
  append(CurrentMethods, PartialMethods, Methods).
select_methods(AvailableMethods, [_|Includes], Methods) :-
  select_methods(AvailableMethods, Includes, Methods).

create_exports([], []).
create_exports([Method|Methods], [Name/Arity|Exports]) :-
  method{ name: Name, arity: Arity } :< Method,
  create_exports(Methods, Exports).

merge_contexts([Context|Contexts], MergedContext) :-
  context{ methods: Methods, constants: Constants, exports: Exports, imports: Imports } :< Context,
  create_set(Methods, methods_processor:get_method_signature, MethodsSet),
  create_set(Constants, constants_processor:get_constant_signature, ConstantsSet),
  create_set(Exports, set:identity, ExportsSet),
  create_set(Imports, set:identity, ImportsSet),
  merge_contexts(Contexts, context_sets{
    methods: MethodsSet,
    constants: ConstantsSet,
    exports: ExportsSet,
    imports: ImportsSet
  }, MergedContext).

merge_contexts([], CurrentContext, context{
  methods: Methods,
  constants: Constants,
  exports: Exports,
  imports: Imports
}) :-
  context_sets{
    methods: set(_, Methods),
    constants: set(_, Constants),
    exports: set(_, Exports),
    imports: set(_, Imports)
  } :< CurrentContext.
merge_contexts([Context|Contexts], CurrentContext, MergedContext) :-
  context{ methods: Methods, constants: Constants, exports: Exports, imports: Imports } :< Context,
  context_sets{ methods: MethodsSet, constants: ConstantsSet, exports: ExportsSet, imports: ImportsSet } :< CurrentContext,
  merge_set(MethodsSet, Methods, MergedMethodsSet),
  merge_set(ConstantsSet, Constants, MergedConstantsSet),
  merge_set(ExportsSet, Exports, MergedExportsSet),
  merge_set(ImportsSet, Imports, MergedImportsSet),
  merge_contexts(Contexts, context_sets{
    methods: MergedMethodsSet,
    constants: MergedConstantsSet,
    exports: MergedExportsSet,
    imports: MergedImportsSet
  }, MergedContext).
