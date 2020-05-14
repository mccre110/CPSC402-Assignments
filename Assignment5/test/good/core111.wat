(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $main (result i32) (call $readInt) (i32.const 1) i32.sub (call $printInt) (i32.const 0) return)
 (export "main" (func $main))
)