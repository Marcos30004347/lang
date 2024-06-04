# Compiler Project

## Overview
This is the implementation of a simple programming language that supports Algebraic Effects'ish control flow mechanisms.

## Dependencies

- GNU make
- CMake
- C++ compiler

## Setup and Run
To setup the project, from the root call the following instructions from your terminal:
```
$ make
```

Now to compile the project run:
```
$ cd build
$ make
```

This commands is going to build the code and all the tests, right now the compiler does not have an executable program just yet, if you want to see what the compiler is currently doing select a test under tests/ and run it, for example:
```
./build/tests/CPSClosureConversionTests
```
This is going to run the test under /tests/continuations/closures.cpp

If you want to see what executable each tests creates take a look at the tests/CMakeLists.txt from the root of the repository.

## Syntax

### Declaractions:
Declarations can be defined with the following syntax:

\<Declaration\> = \<Name\> ':' \<Type\>?

\<Assignment\> = \<Declaration\> (':'|'=') \<Value\>

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

#### Function
Functions can be defined with the syntax:

\<Arguments\> = ((\<Bind\> ',')* \<Bind\>)*

\<Function\> = '('\<Arguments\>')' ('->' \<Type\>)? '{' \<Statements\> '}'

and need to be associated with a variable
```
add :: (x:i32, y:i32) -> i32 {
	return x + y;
}
```

Any function declaration needs to be assigned to a constant.

#### Closures
Closures can be declared with the syntax:

\<ClosureArguments\> = ((\<ClosureArguments\> ';')* \<ClosureArguments\>)*

\<Closure\> = '('\<ClosureArguments\>')' ('->' \<Type\>)? '{' \<Statements\> '}'

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

Closures declared inside function cant be returned, the closure environment can only live as long as the variables that it captures:
```

f :: () {
	y : i32 = 3;

	g :: (x:i32) {
		return x + y;
	}
	
	x : i32 : g(3); // Ok

	return g; // Not ok
}
```

### Calls
Calls are made like normal function calls in any imperative language

\<CallArguments\> = ((\<Value\> ',')* \<Value\>)*

\<Call\> = \<Name\>'(' \<CallArguments\> ')'

### Effect Calls:
Effects are called like normal functions but with a '!' appended at the end of the function name:

\<EffectCall\> = \<Name\>''!'(' \<CallArguments\> ')'

```
main :: () {
	print!("Hello world");
}
```


## Algebraic Effect'ish:
Some of the semantics of algebraic effects are not planned to be suported, sinse we aim to provide pointers support at some point, for performance reasons, a multi shot continuation may return different values if called multiple times with the same input.

## Compilation pipeline:

Parsing -> Continuation Capturing -> Continuation Closure Conversion -> Generalized Evidence Passing -> Bubbling Yields -> Transpilation to C
