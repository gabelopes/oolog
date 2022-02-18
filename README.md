# Oolog

# Constructor and Attribute Setting Mechanism

Attributes in Oolog are constants, that means that they cannot be changed in the course of execution, however, there is one time they are allowed to be changed, and that's when a constructor is called. Once a class is instatiated, a constructor matching the arity provided is called, and inside that, by using an accessor, if the attribute does not already have an initial value, a constructor can set one. And only then it is allowed. Once the constructor finishes executing, all attributes are frozen, and cannot be changed anymore.

Remember though, that static attributes cannot be initialized via constructor, this way, they must always have an initial value!
