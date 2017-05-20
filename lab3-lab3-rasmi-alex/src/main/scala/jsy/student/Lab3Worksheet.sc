/*
 * CSCI 3155: Lab 3 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab3.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab3.Parser.parse

// Imports the ast nodes
import jsy.lab3.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab3._

// Parse a function
parse("x => x")
parse("(x) => x")
parse("function (x) { return x }")
parse("function id(x) { return x }")
parse("x => y => x + y")
parse("x => { const z = 3; return x + z }")
parse("function (x) { const z = 3; return x + z }")


val xplus1 = parse("x + 1")
val twoplus1 = parse("2 + 1")

val constx3 = parse("const x = 3; x")
val shadowx = Binary(Plus, constx3, Var("x"))


// Ask about call function
// Ask about function
// Ask about ? symbol in the if do and search rules
// Go over equality search rules

val potato = parse("const f = function f(x) { return x === 0 ? 1 : x * f(x - 1) };")

val lettuce = parse("const x = 3;")

val tomato = parse("const x = 3; function container(x) {return(x*x)} container(x);")

val onion = parse("const b = 5;\nconst foo = function (x){\n    const a = b + 5;\n    return a;\n}\n\nconst boo = function (x){\n    const b = 2;\n    return foo(1);\n}\n\nfoo(1);\nboo(1);")



