(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $dz$0 f64)
  (local $dw$0 f64)
  (f64.const 9.3)
  (local.set $dz$0)
  (f64.const 5.1)
  (local.set $dw$0)
  (local.get $dz$0)
  (local.get $dw$0)
  f64.add
  (local.get $dz$0)
  (local.get $dw$0)
  f64.sub
  f64.gt
  (call $printBool)
  (local.get $dz$0)
  (local.get $dw$0)
  f64.div
  (local.get $dz$0)
  (local.get $dw$0)
  f64.mul
  f64.le
  (call $printBool)
  (i32.const 0)
  return
 )
 (func
  $printBool
  (param $bb$0 i32)
  (local.get $bb$0)
  (if (then (i32.const 1) (call $printInt)) (else (i32.const 0) (call $printInt)))
 )
 (export "main" (func $main))
)