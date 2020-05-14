(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ij$0 i32)
  (i32.const 4)
  (local.set $ij$0)
  (local.get $ij$0)
  (i32.const 6)
  i32.lt_s
  (if (then (local.get $ij$0) (call $printInt)) (else))
  (local.get $ij$0)
  return
 )
 (export "main" (func $main))
)