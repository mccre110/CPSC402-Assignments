(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (block
   (loop
    (i32.const 2)
    (i32.const 5)
    (i32.const 6)
    i32.mul
    (i32.const 5)
    i32.div_s
    i32.add
    (i32.const 67)
    i32.sub
    (i32.const 5)
    i32.gt_s
    i32.eqz
    (br_if 1)
    (br 0)
   )
  )
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)