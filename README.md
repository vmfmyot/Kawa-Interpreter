
# Kawa Interpreter

This project provides an OCaml interpreter for 'Kawa', a small object-oriented language.\
Developed in 4 weeks for a 3rd-year university compilation course, it covers both syntax and semantics.
## Authors

This project was made by [Isabel Fabrega](https://github.com/im-f) and [Victoria Myot](https://github.com/vmfmyot), computer science students at Université Paris-Saclay.

## Prerequisites

As this project is written in OCaml, we recommend you have opam installed on your machine. Here's the [link](https://opam.ocaml.org/) to the installation page.

## Documentation

Kawa is a small object-oriented language inspired by Java. However, we adapted some of the features for an easier implementation.\
Kawa files have the `.kwa` extension. You can find some examples in the tests folder.\
\
To compile and execute a file, use the following command line :\
`./kawai.exe <name_of_file>`\
**Variables** need to be declared at the beginning of a file. They can either be declared one by one, or one after the other if they are of the same type. **Classes** are declared below variables. The **main code** is executed in a `main { }` function (without the () !)\
Comments can be detected with `//`.
\
You will find down below the different features available.
 


### _Basic types_
This Kawa interpreter supports the following types : integer with the keyword `int`, strings with either `string` or `char` and booleans with `bool`.\
\
**Sidenote on strings :**\
Strings can be concatenated with the binary operator `@`.\
Strings are treated as arrays of single characters : the length of a string is returned by the `.length` function, and `val[index]` returns the character at the given index (as long as val is a string).


### _Binary operators_
The usual binary operators are implemented :
- Computational operators: `= + - / %`
- Comparison operators: `< <= > >= == !=`
- Boolean operators: `&& ||`
We also added structural equality, which check if 2 objects are structurally equal. The binary operators for this are `===` and `=/=`.\
Structural equality is tested in the `eqstruct.kwa` file.


### _Print function_
Integers and strings can be printed with the `print(parameters)` function.

### _Inheritance_
To let a class inherit from another, the keyword `extends` can be used the exact same way as in Java.\
Constructors can be made with the keyword `super` to call on the constructor of the parent class the following way : `super.constructor(parameters here)`.\
Inheritance is tested in the `extend.kwa` file.

### _Instanceof and typecast_
Keyword `instanceof` and typecasting are just like in Java, and tested in the `extrafonctions.kwa` file.


### _Loops_
This interpreter supports if, while and for loops.
- IF : `if( condition ){ instructions }`
- WHILE : `while( condition ){ instructions }`
- FOR : `for( type variable ; condition ; variation ){ instructions }`


**while** and **if** loops are tested in the `instr.kwa` file, and **for** loops in `loops.kwa`.\
\
There are also **foreach loops**, currently only usable on arrays as they're the only iterable structure :\
`for( array_type value : name ){ instructions }`\
**foreach** loops are tested in `tab.kwa`.


### _Override_
Overriding functions is allowed, and overrides are tested in `extrafonctions.kwa`.


### _Arrays_
Simple arrays are supported, with the [ ] token.\
\
**Declaration :** `type_of_array[] name_of_array`\
\
**Initialization :** there are 2 possible ways of initializing an array :
```
arr1 = new type[]
arr2 = [element1, element2, element3]
```
Arrays also have the following methods :
- `.length` : returns the length of an array
- `.hd` : returns the 1st element of an array
- `.tl` : returns the last element and removes it out of the array
- `.rem(index)` : removes the element of the given index
- `.mem(value)` : returns true if the value is in the array, else returns false
- `.copy` : returns a copy of the array

Arrays are tested in the `tab.kwa` file.

