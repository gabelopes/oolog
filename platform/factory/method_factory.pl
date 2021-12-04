:- module(method_factory, [
  create_methods/3
]).

:- use_module(library(unison)).
:- use_module('../operators/method_operators').
:- use_module('../processor/methods_processor').
:- use_module('../operators/modifier_operators').
:- use_module('../processor/modifiers_processor').
:- use_module('../lifecycle/exception').
:- use_module('../structure/set').
:- use_module('../structure/header').

create_methods(MethodDeclarations, SuperClassMethods, Methods) :-
  define_methods(MethodDeclarations, ClassMethods),
  overwrite_methods(ClassMethods, SuperClassMethods, ExtendedMethods),
  merge_methods(ExtendedMethods, SuperClassMethods, Methods).

define_methods([], []).
define_methods([MethodExpression|MethodExpressions], [method{
  modifiers: Modifiers,
  name: Name,
  arity: Arity,
  arguments: Arguments,
  body: Body
}|MethodDefinitions]) :-
  extract_modifiers(method, MethodExpression, declaration(Modifiers, MethodDeclaration)),
  extract_method(MethodDeclaration, Name, Arguments, Body),
  length(Arguments, Arity),
  define_methods(MethodExpressions, MethodDefinitions).

extract_method(Header => Body, Name, Arguments, Body) :-
  header(Header, Name, Arguments).

overwrite_methods(Methods, [], Methods).
overwrite_methods([], _, []).
overwrite_methods([Method|Methods], BaseMethods, [ExtendedMethod|ExtendedMethods]) :-
  overwrite_method(Method, BaseMethods, ExtendedMethod),
  overwrite_methods(Methods, BaseMethods, ExtendedMethods).

overwrite_method(Method, BaseMethods, Method) :-
  find_method(Method, BaseMethods, BaseMethod),
  validate_overwrite(Method, BaseMethod).
overwrite_method(Method, _, Method).

find_method(Method, BaseMethods, FoundMethod) :-
  member(FoundMethod, BaseMethods),
  _{ name: Name, arity: Arity, modifiers: _{ scope: Scope }} :<< Method,
  _{ name: Name, arity: Arity, modifiers: _{ scope: Scope }} :<< FoundMethod.

validate_overwrite(Method, BaseMethod) :-
  _{ modifiers: _{ visibility: MethodVisibility }} :<< Method,
  _{ modifiers: _{ visibility: BaseMethodVisibility }} :<< BaseMethod,
  can_overwrite(method, MethodVisibility, BaseMethodVisibility).
validate_overwrite(Method, BaseMethod) :-
  _{ modifiers: _{ visibility: MethodVisibility }} :<< Method,
  _{ name: BaseMethodName, modifiers: _{ visibility: BaseMethodVisibility }} :<< BaseMethod,
  raise_exception(
    "Unable to overwrite method '~w', visibility cannot be increased from ~w to ~w.",
    [BaseMethodName, BaseMethodVisibility, MethodVisibility]
  ).

merge_methods(Methods, BaseMethods, MergedMethods) :-
  create_set(Methods, methods_processor:get_method_signature, MethodSet, DifferentialSet),
  validate_duplicate_methods(Methods, DifferentialSet),
  merge_set(MethodSet, BaseMethods, set(_, MergedMethods)).
