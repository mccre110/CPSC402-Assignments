(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $snd (param $ix$0 i32) (param $iy$0 i32) (result i32) (local.get $iy$0) return)
 (func
  $main
  (result i32)
  (local $ix$0 i32)
  (local $ir$0 i32)
  (i32.const 0)
  (local.set $ix$0)
  (i32.const 1)
  (local.get $ix$0)
  (call $snd)
  (local.set $ir$0)
  (local.get $ir$0)
  (call $printInt)
  (local.get $ir$0)
  return
 )
 (export "main" (func $main))
)