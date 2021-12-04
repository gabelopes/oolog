:- module(attribute_factory, [
  create_attributes/3
]).

:- use_module(library(unison)).
:- use_module('../operators/modifier_operators').
:- use_module('../processor/modifiers_processor').
:- use_module('../processor/attributes_processor').
:- use_module('../lifecycle/exception').
:- use_module('../structure/set').

create_attributes(AttributeDeclarations, SuperClassAttributes, Attributes) :-
  define_attributes(AttributeDeclarations, ClassAttributes),
  overwrite_attributes(ClassAttributes, SuperClassAttributes, ExtendedAttributes),
  merge_attributes(ExtendedAttributes, SuperClassAttributes, Attributes).

define_attributes([], []).
define_attributes([AttributeExpression|AttributeExpressions], [attribute{
  modifiers: Modifiers,
  name: Name,
  initial_value: InitialValue
}|AttributeDefinitions]) :-
  extract_modifiers(attribute, AttributeExpression, declaration(Modifiers, AttributeDeclaration)),
  extract_attribute(AttributeDeclaration, Name, InitialValue),
  define_attributes(AttributeExpressions, AttributeDefinitions).

extract_attribute(Name=InitialValue, Name, InitialValue).
extract_attribute(Name, Name, none).

overwrite_attributes(Attributes, [], Attributes).
overwrite_attributes([], _, []).
overwrite_attributes([Attribute|Attributes], BaseAttributes, [ExtendedAttribute|ExtendedAttributes]) :-
  overwrite_attribute(Attribute, BaseAttributes, ExtendedAttribute),
  overwrite_attributes(Attributes, BaseAttributes, ExtendedAttributes).

overwrite_attribute(Attribute, BaseAttributes, Attribute) :-
  find_attribute(Attribute, BaseAttributes, BaseAttribute),
  validate_overwrite(Attribute, BaseAttribute).
overwrite_attribute(Attribute, _, Attribute).

find_attribute(Attribute, BaseAttributes, FoundAttribute) :-
  member(FoundAttribute, BaseAttributes),
  _{ name: Name, modifiers: _{ scope: Scope }} :<< Attribute,
  _{ name: Name, modifiers: _{ scope: Scope }} :<< FoundAttribute.

validate_overwrite(Attribute, BaseAttribute) :-
  _{ modifiers: _{ visibility: AttributeVisibility }} :<< Attribute,
  _{ modifiers: _{ visibility: BaseAttributeVisibility }} :<< BaseAttribute,
  can_overwrite(attribute, AttributeVisibility, BaseAttributeVisibility).
validate_overwrite(Attribute, BaseAttribute) :-
  _{ modifiers: _{ visibility: AttributeVisibility }} :<< Attribute,
  _{ name: BaseAttributeName, modifiers: _{ visibility: BaseAttributeVisibility }} :<< BaseAttribute,
  raise_exception(
    "Unable to overwrite attribute '~w', visibility cannot be increased from ~w to ~w.",
    [BaseAttributeName, BaseAttributeVisibility, AttributeVisibility]
  ).

merge_attributes(Attributes, BaseAttributes, MergedAttributes) :-
  create_set(Attributes, attributes_processor:get_attribute_signature, AttributeSet, DifferentialSet),
  validate_duplicate_attributes(Attributes, DifferentialSet),
  merge_set(AttributeSet, BaseAttributes, set(_, MergedAttributes)).
