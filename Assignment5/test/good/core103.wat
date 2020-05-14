(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ij$0 i32)
  (local $ii$2 i32)
  (i32.const 4)
  (local.set $ij$0)
  (block
   (loop
    (local.get $ij$0)
    (i32.const 6)
    i32.lt_s
    i32.eqz
    (br_if 1)
    (i32.const 0)
    (local.set $ii$2)
    (local.get $ii$2)
    (local.get $ii$2)
    (i32.const 1)
    i32.add
    (local.set $ii$2)
    drop
    (local.get $ii$2)
    (call $printInt)
    (local.get $ij$0)
    (local.get $ij$0)
    (i32.const 1)
    i32.add
    (local.set $ij$0)
    drop
    (br 0)
   )
  )
  (local.get $ij$0)
  (i32.const 7)
  i32.lt_s
  (if
   (then (local.get $ij$0) (local.get $ij$0) (i32.const 1) i32.add (local.set $ij$0) drop)
   (else (local.get $ij$0) (local.get $ij$0) (i32.const 1) i32.sub (local.set $ij$0) drop)
  )
  (local.get $ij$0)
  (call $printInt)
  (local.get $ij$0)
  return
 )
 (export "main" (func $main))
)