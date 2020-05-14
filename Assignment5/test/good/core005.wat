(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ix$0 i32)
  (local $iy$0 i32)
  (i32.const 56)
  (local.set $iy$0)
  (local.get $iy$0)
  (i32.const 45)
  i32.add
  (i32.const 2)
  i32.le_s
  (if (then (i32.const 1) (local.set $ix$0)) (else (i32.const 2) (local.set $ix$0)))
  (local.get $ix$0)
  (call $printInt)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)