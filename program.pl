:- use_module('oolog/oolog').

:- class vector(private coordinates=[0]) implements [
  public constructor() => true,

  public constructor(Coordinates) => (
    coordinates(Coordinates)
  ),

  public length(Length) => (
    coordinates(Coordinates),
    foldl(sum_squared, Coordinates, 0, SquaredCoordinates),
    Length is sqrt(SquaredCoordinates)
  ),

  private sum_squared(Coordinate, CurrentTotal, Total) => (
    Total is CurrentTotal + Coordinate^2
  )
].

start :-
  Vector := vector([15]),
  Vector::length(Length),
  writeln(Length).
