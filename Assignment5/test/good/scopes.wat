(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $f
  (result i32)
  (local $in$0 i32)
  (local $in$2 i32)
  (i32.const 2)
  (local.set $in$0)
  (local.get $in$0)
  (i32.const 3)
  i32.lt_s
  (if (then (i32.const 3) (local.set $in$2) (local.get $in$2) return) (else))
  (local.get $in$0)
  return
 )
 (func
  $main
  (result i32)
  (local $in$0 i32)
  (i32.const 1)
  (local.set $in$0)
  (local.get $in$0)
  (call $printInt)
  (call $f)
  (call $printInt)
  (local.get $in$0)
  (call $printInt)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)