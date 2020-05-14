(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $in$0 i32)
  (local $im$1 i32)
  (local $im$0 i32)
  (i32.const 0)
  (local.set $in$0)
  (block
   (loop
    (local.get $in$0)
    (local.get $in$0)
    (i32.const 1)
    i32.add
    (local.set $in$0)
    (i32.const 10)
    i32.lt_s
    i32.eqz
    (br_if 1)
    (i32.const 100)
    (local.set $im$1)
    (br 0)
   )
  )
  (local.get $in$0)
  (local.set $im$0)
  (local.get $im$0)
  (call $printInt)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)