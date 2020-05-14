(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $main (result i32) (call $foo) (i32.const 0) return)
 (func $foo (i32.const 1) (call $printInt))
 (export "main" (func $main))
)