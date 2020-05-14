(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $main (result i32) (i32.const 17) (call $ev) (call $printInt) (i32.const 0) return)
 (func
  $ev
  (param $iy$0 i32)
  (result i32)
  (local $ie$0 i32)
  (local.get $iy$0)
  (i32.const 0)
  i32.gt_s
  (if
   (then (local.get $iy$0) (i32.const 2) i32.sub (call $ev) (local.set $ie$0))
   (else
    (local.get $iy$0)
    (i32.const 0)
    i32.lt_s
    (if (then (i32.const 0) (local.set $ie$0)) (else (i32.const 1) (local.set $ie$0)))
   )
  )
  (local.get $ie$0)
  return
 )
 (export "main" (func $main))
)