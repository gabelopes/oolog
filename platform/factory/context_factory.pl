:- module(context, [
  load_context/2,
  load_context/3,
  unload_context/1
]).

:- use_module('../operators/reference_operators').
:- use_module('../manager/reference_manager').
:- use_module('../manager/class_manager').
:- use_module('../manager/context_manager').
:- use_module('../processor/methods_processor').
:- use_module('../sandbox/context_sandbox').

load_context(ObjectReference, ContextReference) :-
  load_context(ObjectReference, [accessors, methods], ContextReference), !.

load_context(ObjectReference, Includes, ContextReference) :-
  create_context(ObjectReference, Includes, Context),
  register_context(Context, ContextReference),
  setup_sandbox(ContextReference), !.

unload_context(ContextReference) :-
  teardown_sandbox(ContextReference),
  deregister_context(ContextReference), !.

create_context(ObjectReference, Includes, Context) :-
  get_object(ObjectReference, Object),
  Type{} :< Object,
  create_object_context(ObjectReference, Type, Object, Includes, Context).

create_object_context(ClassReference, class, Class, Includes, context{
  binding: ClassReference,
  methods: Methods,
  constants: [],
  exports: Exports,
  imports: []
}) :-
  class{
    methods: ClassMethods,
    accessors: Accessors
  } :< Class,
  find_methods_by_scope(ClassMethods, [static], StaticMethods),
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

create_object_context(InstanceReference, instance, Instance, Includes, context{
  binding: InstanceReference,
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
