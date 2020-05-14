(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ii$0 i32)
  (call $readInt)
  (local.set $ii$0)
  (local.get $ii$0)
  (call $printInt)
  (local.get $ii$0)
  (local.get $ii$0)
  (i32.const 1)
  i32.add
  (local.set $ii$0)
  (call $printInt)
  (local.get $ii$0)
  (call $printInt)
  (local.get $ii$0)
  (i32.const 1)
  i32.add
  (local.tee $ii$0)
  (call $printInt)
  (local.get $ii$0)
  (call $printInt)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)