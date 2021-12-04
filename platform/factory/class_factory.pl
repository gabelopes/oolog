:- module(class_factory, [
  load_class/5
]).

:- use_module(library(unison)).
:- use_module('../operators/method_operators').
:- use_module('../processor/methods_processor').
:- use_module('../processor/modifiers_processor').
:- use_module('../processor/attributes_processor').
:- use_module('../manager/class_manager').
:- use_module(attribute_factory).
:- use_module(method_factory).
:- use_module(accessor_factory).

load_class(Package, Name, Attributes, SuperClassDefinition, Methods) :-
  create_class(Package, Name, Attributes, SuperClassDefinition, Methods, ClassReference, ClassDefinition),
  register_class(ClassDefinition, ClassReference), !.

create_class(Package, Name, AttributeDeclarations, SuperClassDefinition, MethodDeclarations, ClassReference, class{
  package: Package,
  name: Name,
  super_class: SuperClassReference,
  attributes: Attributes,
  constructors: Constructors,
  methods: Methods,
  accessors: Accessors,
  data: data{
    attributes: AttributesData
  }
}) :-
  class{
    reference: SuperClassReference,
    attributes: SuperClassAttributes,
    methods: SuperClassMethods
  } :< SuperClassDefinition,
  create_attributes(AttributeDeclarations, SuperClassAttributes, Attributes),
  create_methods(MethodDeclarations, SuperClassMethods, CombinedMethods),
  validate_overlapping_attributes(Attributes, SuperClassMethods),
  validate_overlapping_methods(CombinedMethods, Attributes),
  select_constructors(CombinedMethods, DeclaredConstructors, Methods),
  add_default_constructor(DeclaredConstructors, Constructors),
  find_attributes_by_scope(Attributes, [static], ClassAttributes),
  initialize_attributes(ClassAttributes, AttributesData),
  create_accessors(ClassReference, ClassAttributes, Accessors).

select_constructors([], [], []).
select_constructors([Method|CombinedMethods], [Method|Constructors], Methods) :-
  method{ name: constructor } :< Method,
  select_constructors(CombinedMethods, Constructors, Methods).
select_constructors([Method|CombinedMethods], Constructors, [Method|Methods]) :-
  select_constructors(CombinedMethods, Constructors, Methods).

add_default_constructor([], [
  method{
    name: constructor,
    arity: 0,
    arguments: [],
    body: true,
    modifiers: modifiers{
      scope: prototype,
      visibility: public
    }
  }
]).
add_default_constructor(Constructors, Constructors).
