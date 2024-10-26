# eiv
Implementation of a POC language where everything is a variable

To compile and run, install rust and do:
``cargo run``

With this, you'll have access to the basic cli of the interpreter.

To run individual files, you can just pass them as a parameter.

A few examples can be seen [here](src/tests/)

Syntax:
```
var_declaration = 5; // ; is optional
fn_declaration = (){
    //body
bicho }
struct_declaration = {
    var = 5;
    method = (){
        //...
    }
}
```

Basic implemented types are numbers, bools, chars and arrays.

There are no 'strings', a "string" is simply syntax sugar for `['s','t','r','i','n','g']`

The implementation of a string that i use internally is [here](src/tests/string.eiv)

`_type_`, `_add_`, `_eq_`, `_display_` are all methods that overload characteristics of the language
