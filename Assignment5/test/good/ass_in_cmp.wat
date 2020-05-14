(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ix$0 i32)
  (i32.const 5)
  (local.set $ix$0)
  (local.get $ix$0)
  (i32.const 1)
  i32.add
  (local.tee $ix$0)
  (local.get $ix$0)
  (local.get $ix$0)
  (i32.const 1)
  i32.add
  (local.set $ix$0)
  i32.eq
  (if (then (local.get $ix$0) (call $printInt)) (else (i32.const 0) (call $printInt)))
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)