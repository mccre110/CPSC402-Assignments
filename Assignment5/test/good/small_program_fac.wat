(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $main (result i32) (i32.const 5) (call $fac) (call $printInt) (i32.const 0) return)
 (func
  $fac
  (param $ia$0 i32)
  (result i32)
  (local $ir$0 i32)
  (local $in$0 i32)
  (i32.const 1)
  (local.set $ir$0)
  (local.get $ia$0)
  (local.set $in$0)
  (block
   (loop
    (local.get $in$0)
    (i32.const 0)
    i32.gt_s
    i32.eqz
    (br_if 1)
    (local.get $ir$0)
    (local.get $in$0)
    i32.mul
    (local.set $ir$0)
    (local.get $in$0)
    (i32.const 1)
    i32.sub
    (local.set $in$0)
    (br 0)
   )
  )
  (local.get $ir$0)
  return
 )
 (export "main" (func $main))
)