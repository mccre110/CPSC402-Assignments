(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $iarg$0 i32)
  (local $iret$0 i32)
  (local $ii$0 i32)
  (call $readInt)
  (local.set $iarg$0)
  (i32.const 1)
  (local.set $iret$0)
  (i32.const 1)
  (local.set $ii$0)
  (block
   (loop
    (local.get $ii$0)
    (local.get $iarg$0)
    (i32.const 1)
    i32.add
    i32.lt_s
    i32.eqz
    (br_if 1)
    (local.get $ii$0)
    (local.get $iret$0)
    i32.mul
    (local.set $iret$0)
    (local.get $ii$0)
    (i32.const 1)
    i32.add
    (local.tee $ii$0)
    (br 0)
   )
  )
  (local.get $iret$0)
  (call $printInt)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)