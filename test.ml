module Compiler = Matcher.Compiler();;
let s0 = Compiler.state();;
let sa = Compiler.state();;
Compiler.transition s0 'a' 'a' (Compiler.Goto sa);;
Compiler.transition s0 '\000' (Char.chr (Char.code 'a' - 1)) (Compiler.Goto s0);;
Compiler.transition s0 'b' '\xFF' (Compiler.Goto s0);;
let table, index = Compiler.compile();;
let r = ref 0;;
let str = String.make 10_000_000 'b';;
Matcher.follow table (index s0) str ~offset:r ~len:(String.length str);;
print_int !r;;
