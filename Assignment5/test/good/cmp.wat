(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $printBool
  (param $bb$0 i32)
  (local.get $bb$0)
  (if (then (i32.const 1) (call $printInt)) (else (i32.const 0) (call $printInt)))
 )
 (func
  $main
  (result i32)
  (i32.const 30)
  (i32.const 40)
  i32.eq
  (call $printBool)
  (i32.const 30)
  (i32.const 30)
  i32.eq
  (call $printBool)
  (i32.const 30)
  (i32.const 30)
  i32.ne
  (call $printBool)
  (i32.const 40)
  (i32.const 40)
  i32.ne
  (call $printBool)
  (i32.const 3)
  (i32.const 4)
  i32.gt_s
  (call $printBool)
  (i32.const 3)
  (i32.const 3)
  i32.gt_s
  (call $printBool)
  (i32.const 4)
  (i32.const 3)
  i32.lt_s
  (call $printBool)
  (i32.const 4)
  (i32.const 4)
  i32.lt_s
  (call $printBool)
  (i32.const 400)
  (i32.const 300)
  i32.ge_s
  (call $printBool)
  (i32.const 400)
  (i32.const 400)
  i32.ge_s
  (call $printBool)
  (i32.const 400)
  (i32.const 300)
  i32.le_s
  (call $printBool)
  (i32.const 400)
  (i32.const 400)
  i32.le_s
  (call $printBool)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)