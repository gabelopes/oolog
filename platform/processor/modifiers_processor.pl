:- module(modifier_processor, [
  extract_modifiers/3,
  can_overwrite/3
]).

:- use_module('../operators/modifier_operators').

modifiers(attribute, _{
  visibility: [public, protected, private],
  scope: [static]
}, modifiers{
  visibility: public,
  scope: prototype
}).

modifiers(method, _{
  visibility: [public, protected, private],
  scope: [static]
}, modifiers{
  visibility: public,
  scope: prototype
}).

extract_modifiers(ElementType, Expression, declaration(Modifiers, Declaration)) :-
  modifiers(ElementType, ElementModifiers, DefaultElementModifiers),
  extract_modifiers(ElementModifiers, Expression, modifiers{}, ExtractedModifiers, Declaration),
  Modifiers = DefaultElementModifiers.put(ExtractedModifiers).

extract_modifiers(TypeModifiers, Expression, CurrentModifiers, Modifiers, Declaration) :-
  Expression =.. [Modifier, PartialExpression],
  get_modifier_type(TypeModifiers, Modifier, Type),
  validate_modifier(CurrentModifiers, Type, Modifier),
  PartialModifiers = CurrentModifiers.put([Type=Modifier]),
  extract_modifiers(TypeModifiers, PartialExpression, PartialModifiers, Modifiers, Declaration).
extract_modifiers(_, Declaration, Modifiers, Modifiers, Declaration).

validate_modifier(CurrentModifiers, Type, Modifier) :-
  get_dict(Type, CurrentModifiers, DeclaredModifier),
  format(string(Exception), "Modifier '~w' conflicts with '~w'.", [Modifier, DeclaredModifier]), 
  throw(Exception).
validate_modifier(_, _, _).

get_modifier_type(TypeModifiers, Modifier, Type) :-
  get_dict(Type, TypeModifiers, Modifiers),
  member(Modifier, Modifiers).

can_overwrite(ElementType, OverwritingModifier, Modifier) :-
  modifiers(ElementType, ElementModifiers, _),
  get_modifier_type(ElementModifiers, OverwritingModifier, Type),
  get_modifier_type(ElementModifiers, Modifier, Type),
  get_dict(Type, ElementModifiers, Modifiers),
  nth0(OverwritingModifierIndex, Modifiers, OverwritingModifier),
  nth0(ModifierIndex, Modifiers, Modifier),
  OverwritingModifierIndex >= ModifierIndex.
