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
  create_class(Package, Name, Attributes, SuperClassDefinition, Methods, Reference, ClassDefinition),
  register_class(ClassDefinition, Reference).

create_class(Package, Name, AttributeDeclarations, SuperClassDefinition, MethodDeclarations, Reference, class{
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
  validate_overlapping_methods(CombinedMethods, Attributes),
  separate_methods(CombinedMethods, Constructors, Methods),
  find_attributes(Attributes, [static], ClassAttributes),
  initialize_attributes(ClassAttributes, AttributesData),
  create_accessors(Reference, ClassAttributes, Accessors).

separate_methods([], [], []).
separate_methods([Method|CombinedMethods], [Method|Constructors], Methods) :-
  method{ name: constructor } :< Method,
  separate_methods(CombinedMethods, Constructors, Methods).
separate_methods([Method|CombinedMethods], Constructors, [Method|Methods]) :-
  separate_methods(CombinedMethods, Constructors, Methods).
