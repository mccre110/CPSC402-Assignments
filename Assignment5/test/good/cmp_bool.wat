(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $printBool
  (param $bb$0 i32)
  (local.get $bb$0)
  (if (then (i32.const 1) (call $printInt)) (else (i32.const 0) (call $printInt)))
 )
 (func
  $main
  (result i32)
  (i32.const 1)
  (i32.const 1)
  i32.eq
  (call $printBool)
  (i32.const 1)
  (i32.const 0)
  i32.eq
  (call $printBool)
  (i32.const 1)
  (i32.const 0)
  i32.ne
  (call $printBool)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)