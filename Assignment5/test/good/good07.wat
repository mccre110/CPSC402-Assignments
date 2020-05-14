(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ix$0 i32)
  (local $id$0 i32)
  (call $readInt)
  (local.set $ix$0)
  (local.get $ix$0)
  (i32.const 2)
  i32.div_s
  (local.set $id$0)
  (block
   (loop
    (local.get $id$0)
    (i32.const 1)
    i32.gt_s
    i32.eqz
    (br_if 1)
    (local.get $id$0)
    (local.get $ix$0)
    (local.get $id$0)
    i32.div_s
    i32.mul
    (local.get $ix$0)
    i32.eq
    (if (then (local.get $id$0) (call $printInt)) (else))
    (local.get $id$0)
    (local.get $id$0)
    (i32.const 1)
    i32.sub
    (local.set $id$0)
    drop
    (br 0)
   )
  )
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)