# Languae (TODO: name to be defined)

## Overview
This is the implementation of a simple imperative programming language that supports Algebraic Effects'ish control flow mechanisms.

## Syntax

### Declaractions:
Declarations can be defined with the following syntax:
<Declaration> = <Name> ':' <Type>?
<Assignment> = <Declaration> (':'|'=') <Value>

- Variables:
```
id := 0
index : i32 = 3
```

- Constants
```
id :: 0
index : i32 : 2
```

### Functions(likelly to change soon):

#### Function
Functions can be defined with the syntax:
<Arguments> = ((<Bind> ',')* <Bind>)*
<Function> = '('<Arguments>')' ('->' <Type>)? '{' <Statements> '}'

and need to be associated with a variable
```
add :: (x:i32, y:i32) -> i32 {
	return x + y;
}
```

#### Closures(likelly to be removed)
Closures can be declared with the syntax:

<ClosureArguments> = ((<ClosureArguments> ';')* <ClosureArguments>)*
<Closure> = '('<ClosureArguments>')' ('->' <Type>)? '{' <Statements> '}'

```
add :: (x:i32; y:i32) -> i32 {
	return x + y;
}

f :: () {
	x: i32 = add(3; 4);
	add_three: i32 -> i32 = add(3);
	z: i32 = add_three(4);
}
```

### Calls
Calls are made like normal function calls in any imperative language

<CallArguments> = ((<Value> ',')* <Value>)*

<Call> = <name>'(' <CallArguments> ')'

### Effect:
TODO: write syntax rules and examples for effects

### Handler:
TODO: write syntax rules and examples for handlers

### Effect Calls:
Effects are called like normal functions but with a '!' appended at the end of the function name:

<EffectCall> = <name>''!'(' <CallArguments> ')'

```
main :: () {
	print!("Hello world");
}
```
