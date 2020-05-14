(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $g
  (result i32)
  (i32.const 1)
  (if (result i32) (then (i32.const 12) return) (else (i32.const 11) return))
  (i32.const 10)
  return
 )
 (func $main (result i32) (call $g) (call $printInt) (i32.const 0) return)
 (export "main" (func $main))
)