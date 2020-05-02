(module
 (import "env" "readInt" (func $readInt (result i32)))
 (import "env" "readDouble" (func $readDouble (result f64)))
 (import "env" "printInt" (func $printInt (param i32)))
 (import "env" "printDouble" (func $printDouble (param f64)))
 (func $main (result i32) (local $bvar$0 i32) (local $ivar$1 i32) (local $bvar1$0 i32) (local $bvar2$0 i32))
 (export "main" (func $main))
)