(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (i32.const 0)
  (i32.const 0)
  i32.ge_s
  (if (then (i32.const 0) (call $printInt)) (else (i32.const 1) (call $printInt)))
  (i32.const 1)
  (i32.const 1)
  i32.ge_s
  (if (then (i32.const 2) (call $printInt)) (else (i32.const 3) (call $printInt)))
  (i32.const 2)
  (i32.const 2)
  i32.le_s
  (if (then (i32.const 4) (call $printInt)) (else (i32.const 5) (call $printInt)))
  (i32.const 0)
  (i32.const 0)
  i32.le_s
  (if (then (i32.const 6) (call $printInt)) (else (i32.const 7) (call $printInt)))
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)