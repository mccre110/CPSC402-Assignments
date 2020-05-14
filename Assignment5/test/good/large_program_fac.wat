(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func
  $main
  (result i32)
  (i32.const 10)
  (call $fac)
  (call $printInt)
  (i32.const 10)
  (call $rfac)
  (call $printInt)
  (i32.const 10)
  (call $mfac)
  (call $printInt)
  (i32.const 10)
  (call $ifac)
  (call $printInt)
  (i32.const 0)
  return
 )
 (func
  $fac
  (param $ia$0 i32)
  (result i32)
  (local $ir$0 i32)
  (local $in$0 i32)
  (i32.const 1)
  (local.set $ir$0)
  (local.get $ia$0)
  (local.set $in$0)
  (block
   (loop
    (local.get $in$0)
    (i32.const 0)
    i32.gt_s
    i32.eqz
    (br_if 1)
    (local.get $ir$0)
    (local.get $in$0)
    i32.mul
    (local.set $ir$0)
    (local.get $in$0)
    (i32.const 1)
    i32.sub
    (local.set $in$0)
    (br 0)
   )
  )
  (local.get $ir$0)
  return
 )
 (func
  $rfac
  (param $in$0 i32)
  (result i32)
  (local $if$0 i32)
  (local.get $in$0)
  (i32.const 0)
  i32.eq
  (if
   (then (i32.const 1) (local.set $if$0))
   (else (local.get $in$0) (local.get $in$0) (i32.const 1) i32.sub (call $rfac) i32.mul (local.set $if$0))
  )
  (local.get $if$0)
  return
 )
 (func
  $mfac
  (param $in$0 i32)
  (result i32)
  (local $if$0 i32)
  (local.get $in$0)
  (i32.const 0)
  i32.eq
  (if
   (then (i32.const 1) (local.set $if$0))
   (else (local.get $in$0) (local.get $in$0) (i32.const 1) i32.sub (call $nfac) i32.mul (local.set $if$0))
  )
  (local.get $if$0)
  return
 )
 (func
  $nfac
  (param $in$0 i32)
  (result i32)
  (local $if$0 i32)
  (local.get $in$0)
  (i32.const 0)
  i32.ne
  (if
   (then (local.get $in$0) (i32.const 1) i32.sub (call $mfac) (local.get $in$0) i32.mul (local.set $if$0))
   (else (i32.const 1) (local.set $if$0))
  )
  (local.get $if$0)
  return
 )
 (func $ifac (param $in$0 i32) (result i32) (i32.const 1) (local.get $in$0) (call $ifac2f) return)
 (func
  $ifac2f
  (param $il$0 i32)
  (param $ih$0 i32)
  (result i32)
  (local $if$0 i32)
  (local $im$3 i32)
  (local.get $il$0)
  (local.get $ih$0)
  i32.eq
  (if
   (then (local.get $il$0) (local.set $if$0))
   (else
    (local.get $il$0)
    (local.get $ih$0)
    i32.gt_s
    (if
     (then (i32.const 1) (local.set $if$0))
     (else
      (local.get $il$0)
      (local.get $ih$0)
      i32.add
      (i32.const 2)
      i32.div_s
      (local.set $im$3)
      (local.get $il$0)
      (local.get $im$3)
      (call $ifac2f)
      (local.get $im$3)
      (i32.const 1)
      i32.add
      (local.get $ih$0)
      (call $ifac2f)
      i32.mul
      (local.set $if$0)
     )
    )
   )
  )
  (local.get $if$0)
  return
 )
 (export "main" (func $main))
)