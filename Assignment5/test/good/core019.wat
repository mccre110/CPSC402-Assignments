(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ii$0 i32)
  (local $ii$1 i32)
  (local $ii$2 i32)
  (i32.const 78)
  (local.set $ii$0)
  (i32.const 1)
  (local.set $ii$1)
  (local.get $ii$1)
  (call $printInt)
  (local.get $ii$0)
  (call $printInt)
  (block
   (loop
    (local.get $ii$0)
    (i32.const 76)
    i32.gt_s
    i32.eqz
    (br_if 1)
    (local.get $ii$0)
    (local.get $ii$0)
    (i32.const 1)
    i32.sub
    (local.set $ii$0)
    drop
    (local.get $ii$0)
    (call $printInt)
    (i32.const 7)
    (local.set $ii$2)
    (local.get $ii$2)
    (call $printInt)
    (br 0)
   )
  )
  (local.get $ii$0)
  (call $printInt)
  (local.get $ii$0)
  (i32.const 4)
  i32.gt_s
  (if (then (i32.const 4) (local.set $ii$2) (local.get $ii$2) (call $printInt)) (else))
  (local.get $ii$0)
  (call $printInt)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)