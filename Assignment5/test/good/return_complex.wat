(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $foo
  (param $ix$0 i32)
  (result i32)
  (local $bb$0 i32)
  (local.get $ix$0)
  (i32.const 0)
  i32.eq
  (if (then (i32.const 0) return) (else))
  (i32.const 1)
  (local.set $bb$0)
  (local.get $ix$0)
  (call $printInt)
  (local.get $bb$0)
  return
 )
 (func
  $printBool
  (param $bb$0 i32)
  (i32.const 0)
  (if
   (then)
   (else
    (local.get $bb$0)
    (if (then (i32.const 0) (call $printInt)) (else (i32.const 1) (call $printInt)))
   )
  )
 )
 (func
  $main
  (result i32)
  (i32.const 42)
  (call $foo)
  (call $printBool)
  (i32.const 0)
  (call $foo)
  (call $printBool)
  (i32.const 0)
  return
  (i32.const 1)
  (call $printBool)
 )
 (export "main" (func $main))
)