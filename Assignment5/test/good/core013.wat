(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (i32.const 0)
  (i32.const 1)
  i32.sub
  (call $test)
  (if
   (result i32)
   (then (i32.const 0) (call $test) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
   (else (i32.const 0))
  )
  (call $printBool)
  (i32.const 0)
  (i32.const 2)
  i32.sub
  (call $test)
  (if
   (result i32)
   (then (i32.const 1))
   (else (i32.const 1) (call $test) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
  )
  (call $printBool)
  (i32.const 3)
  (call $test)
  (if
   (result i32)
   (then
    (i32.const 0)
    (i32.const 5)
    i32.sub
    (call $test)
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
  (i32.const 3)
  (call $test)
  (if
   (result i32)
   (then (i32.const 1))
   (else
    (i32.const 0)
    (i32.const 5)
    i32.sub
    (call $test)
    (if
     (result i32)
     (then (i32.const 1) (if (result i32) (then (i32.const 1)) (else (i32.const 0))))
     (else (i32.const 0))
    )
    (if (result i32) (then (i32.const 1)) (else (i32.const 0)))
   )
  )
  (call $printBool)
  (i32.const 1)
  (call $printBool)
  (i32.const 0)
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
 (func $test (param $ii$0 i32) (result i32) (local.get $ii$0) (i32.const 0) i32.gt_s return)
 (export "main" (func $main))
)