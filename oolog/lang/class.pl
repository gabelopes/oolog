:- module(class, []).

:- use_module('../../platform/orchestrator/class_orchestrator').

:- class class(private definition) implements [
  public constructor(Definition) => (
    definition(Definition)
  ),

  public get_attributes(Attributes) => (
    definition(Definition),
    class{ attributes: AttributesDefinitions } :< Definition,
    'class':transform_attributes(AttributesDefinitions, Attributes)
  ),

  public to_string(String) => (
    definition(Definition),
    class{ package: Package, name: Name } :< Definition,
    format(string(String), "~w:~w", [Package, Name])
  )
].

transform_attributes([], []).
transform_attributes([AttributeDefinition|AttributesDefinitions], [attribute(Name, Modifiers)|Attributes]) :-
  attribute{ name: Name, modifiers: ModifiersDefinitions } :< AttributeDefinition,
  dict_keys(ModifiersDefinitions, ModifiersTypes),
  transform_modifiers(ModifiersTypes, ModifiersDefinitions, Modifiers),
  transform_attributes(AttributesDefinitions, Attributes).

transform_modifiers([], _, []).
transform_modifiers([Type|Types], ModifiersDefinitions, [Type=Modifier|Modifiers]) :-
  get_dict(Type, ModifiersDefinitions, Modifier),
  transform_modifiers(Types, ModifiersDefinitions, Modifiers).
