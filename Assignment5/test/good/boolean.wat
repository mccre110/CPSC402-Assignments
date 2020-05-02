(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $ff (result i32))
 (func $tt (result i32))
 (func
  $main
  (result i32)
  (local $bt$0 i32)
  (local $bf$0 i32)
  (local $bsilent1$0 i32)
  (local $bsilent2$0 i32)
  (local $bnoisy1$0 i32)
  (local $bnoisy2$0 i32)
 )
 (export "main" (func $main))
)