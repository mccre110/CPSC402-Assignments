(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $main (result i32) (local $dr$0 f64) (local $in$1 i32) (local $ir$1 i32))
 (func $dfac (param $dn$0 f64) (result f64) (local $df$0 f64))
 (export "main" (func $main))
)