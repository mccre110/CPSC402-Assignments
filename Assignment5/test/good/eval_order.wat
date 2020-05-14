(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $order (param $ia$0 i32) (param $ib$0 i32) (result i32) (i32.const 0) return)
 (func
  $printIntInt
  (param $ii$0 i32)
  (result i32)
  (local.get $ii$0)
  (call $printInt)
  (local.get $ii$0)
  return
 )
 (func $printIntBool (param $ii$0 i32) (result i32) (local.get $ii$0) (call $printInt) (i32.const 1) return)
 (func
  $main
  (result i32)
  (i32.const 0)
  (call $printIntInt)
  (i32.const 1)
  (call $printIntInt)
  (call $order)
  drop
  (i32.const 2)
  (call $printIntInt)
  (i32.const 3)
  (call $printIntInt)
  i32.add
  (i32.const 4)
  (call $printIntInt)
  (i32.const 5)
  (call $printIntInt)
  i32.eq
  (i32.const 6)
  (call $printIntInt)
  (i32.const 7)
  (call $printIntInt)
  i32.mul
  (i32.const 8)
  (call $printIntInt)
  (i32.const 9)
  (call $printIntInt)
  i32.div_s
  (i32.const 10)
  (call $printIntInt)
  (i32.const 11)
  (call $printIntInt)
  i32.sub
  (i32.const 12)
  (call $printIntInt)
  (i32.const 13)
  (call $printIntInt)
  i32.lt_s
  (i32.const 14)
  (call $printIntInt)
  (i32.const 15)
  (call $printIntInt)
  i32.gt_s
  (i32.const 16)
  (call $printIntInt)
  (i32.const 17)
  (call $printIntInt)
  i32.le_s
  (i32.const 18)
  (call $printIntInt)
  (i32.const 19)
  (call $printIntInt)
  i32.ge_s
  (i32.const 20)
  (call $printIntInt)
  (i32.const 21)
  (call $printIntInt)
  i32.ne
  (i32.const 22)
  (call $printIntBool)
  (if
   (result i32)
   (then (i32.const 23) (call $printIntBool) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
   (else (i32.const 0))
  )
  (i32.const 24)
  (call $printIntBool)
  (if
   (result i32)
   (then (i32.const 1))
   (else (i32.const 25) (call $printIntBool) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
  )
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)