# A Game of Drones
My entry for Ludum Dare 32 - An Unconventional Weapon

Current game idea:
Program a drone to defend against the evil attackers. There's no direct control, only the forward-thinking and logic you feed into your program - *this* is your real weapon.

Each line is an action which your drone will do if the check after the "when" passes. The first line to pass it's check has it's action executed. Each time your drone wants to do something, it will consult your program, and do nothing if none of your rules apply.

The current checks are:
```always``` which always applies
```isWithin``` which takes a type of object, distance and an optional direction and checks to see if an object of a given type is within the given distance (and maybe also in the given direction)

The actions are:
```fire``` which takes a direction
```move``` which takes a direction
```wait``` which takes no parameters

Example program:

```
fire north when enemy isWithin 25 north
fire south when enemy isWithin 25 south
fire east when enemy isWithin 25 east
fire west when enemy isWithin 25
move south when edge isWithin 2 north
move north when edge isWithin 2 south
move east when edge isWithin 2 east
move west when edge isWithin 2 west
move south when always
```

The game is written in [Elm](http://elm-lang.org), using Vim, Elm Reactor and Safari.
