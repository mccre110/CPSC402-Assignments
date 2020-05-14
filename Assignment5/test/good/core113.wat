(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (i32.const 45)
  (call $f)
  (call $printInt)
  (i32.const 450)
  (call $f)
  (call $printInt)
  (i32.const 0)
  return
 )
 (func
  $f
  (param $ix$0 i32)
  (result i32)
  (local $iy$0 i32)
  (local $ix$2 i32)
  (local.get $ix$0)
  (i32.const 100)
  i32.lt_s
  (if
   (then (i32.const 91) (local.set $ix$2) (local.get $ix$2) (local.set $iy$0))
   (else (local.get $ix$0) (local.set $iy$0))
  )
  (local.get $iy$0)
  return
 )
 (export "main" (func $main))
)