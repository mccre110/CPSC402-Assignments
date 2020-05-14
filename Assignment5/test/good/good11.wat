(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (local $isum$0 i32)
  (local $inum$0 i32)
  (local $ix$0 i32)
  (i32.const 0)
  (local.set $isum$0)
  (i32.const 0)
  (local.set $inum$0)
  (block
   (loop
    (call $readInt)
    (local.tee $ix$0)
    (i32.const 0)
    i32.ne
    i32.eqz
    (br_if 1)
    (local.get $isum$0)
    (local.get $ix$0)
    i32.add
    (local.set $isum$0)
    (local.get $inum$0)
    (local.get $inum$0)
    (i32.const 1)
    i32.add
    (local.set $inum$0)
    drop
    (br 0)
   )
  )
  (local.get $isum$0)
  (local.get $inum$0)
  i32.div_s
  (call $printInt)
  (i32.const 0)
  return
 )
 (export "main" (func $main))
)