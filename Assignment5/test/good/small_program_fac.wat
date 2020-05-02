(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $main (result i32))
 (func $fac (param $ia$0 i32) (result i32) (local $ir$0 i32) (local $in$0 i32))
 (export "main" (func $main))
)