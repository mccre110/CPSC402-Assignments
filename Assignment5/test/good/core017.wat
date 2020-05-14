(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ix$0 i32)
  (i32.const 4)
  (local.set $ix$0)
  (i32.const 3)
  (local.get $ix$0)
  i32.le_s
  (if
   (result i32)
   (then (i32.const 4) (i32.const 2) i32.ne (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
   (else (i32.const 0))
  )
  (if
   (result i32)
   (then (i32.const 1) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
   (else (i32.const 0))
  )
  (if (then (i32.const 1) (call $printBool)) (else))
  (i32.const 1)
  (i32.const 1)
  (call $eq_bool)
  (if
   (result i32)
   (then (i32.const 1))
   (else (i32.const 1) (call $dontCallMe) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
  )
  (call $printBool)
  (i32.const 4)
  (i32.const 50)
  i32.gt_s
  (if
   (result i32)
   (then (i32.const 2) (call $dontCallMe) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
   (else (i32.const 0))
  )
  (call $printBool)
  (i32.const 4)
  (local.get $ix$0)
  i32.eq
  (if
   (result i32)
   (then
    (i32.const 1)
    (i32.const 0)
    (call $eq_bool)
    (if (result i32) (then (i32.const 1)) (else (i32.const 0)))
   )
   (else (i32.const 0))
  )
  (if
   (result i32)
   (then (i32.const 1) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
   (else (i32.const 0))
  )
  (call $printBool)
  (i32.const 0)
  (i32.const 0)
  (call $implies)
  (call $printBool)
  (i32.const 0)
  (i32.const 1)
  (call $implies)
  (call $printBool)
  (i32.const 1)
  (i32.const 0)
  (call $implies)
  (call $printBool)
  (i32.const 1)
  (i32.const 1)
  (call $implies)
  (call $printBool)
  (i32.const 0)
  return
 )
 (func $dontCallMe (param $ix$0 i32) (result i32) (local.get $ix$0) (call $printInt) (i32.const 1) return)
 (func
  $printBool
  (param $bb$0 i32)
  (local.get $bb$0)
  (if (then (i32.const 1) (call $printInt)) (else (i32.const 0) (call $printInt)))
 )
 (func
  $implies
  (param $bx$0 i32)
  (param $by$0 i32)
  (result i32)
  (local.get $bx$0)
  (call $not)
  (if
   (result i32)
   (then (i32.const 1))
   (else
    (local.get $bx$0)
    (local.get $by$0)
    (call $eq_bool)
    (if (result i32) (then (i32.const 1)) (else (i32.const 0)))
   )
  )
  return
 )
 (func
  $not
  (param $bx$0 i32)
  (result i32)
  (local $br$0 i32)
  (local.get $bx$0)
  (if (then (i32.const 0) (local.set $br$0)) (else (i32.const 1) (local.set $br$0)))
  (local.get $br$0)
  return
 )
 (func
  $eq_bool
  (param $bx$0 i32)
  (param $by$0 i32)
  (result i32)
  (local $br$0 i32)
  (local.get $bx$0)
  (if (then (local.get $by$0) (local.set $br$0)) (else (local.get $by$0) (call $not) (local.set $br$0)))
  (local.get $br$0)
  return
 )
 (export "main" (func $main))
)