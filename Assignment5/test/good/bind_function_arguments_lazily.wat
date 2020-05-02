(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $snd (param $ix$0 i32) (param $iy$0 i32) (result i32))
 (func $main (result i32) (local $ix$0 i32) (local $ir$0 i32))
 (export "main" (func $main))
)