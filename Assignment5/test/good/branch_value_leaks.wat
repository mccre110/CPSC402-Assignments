(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $ix0$0 i32)
  (local $bx0$1 i32)
  (local $ix1$0 i32)
  (i32.const 0)
  (local.set $ix0$0)
  (i32.const 0)
  (if (then) (else (i32.const 1) (local.set $bx0$1)))
  (local.get $ix0$0)
  (local.get $ix0$0)
  (i32.const 1)
  i32.sub
  (local.set $ix0$0)
  (local.set $ix1$0)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)