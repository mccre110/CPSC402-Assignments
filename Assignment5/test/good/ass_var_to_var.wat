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
  (i32.const 0)
  (local.set $ix$0)
  (local.get $ix$0)
  (local.set $iy$0)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)