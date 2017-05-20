/*
 * CSCI 3155: Lab 2 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab2.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab2.Parser.parse

// Imports the ast nodes
import jsy.lab2.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab2._

// Call the JavaScripty parser (from the provided library) on a string
val negFourAST = parse("-4")

// Evaluate that JavaScripty expression.
//eval(negFourAST)

// For convenience, we also have an eval function that takes a string,
// which calls the parser and then delegates to your eval function.
//eval("undefined + 1")

eval("undefined && 1")
eval("'a' && 'c'")

eval("undefined> 'NaN'")
eval("undefined< 'NaN'")
eval("undefined< 1")
eval("undefined> 1")
eval("undefined < 'a'")
eval("undefined >'a'")





eval("'x'+2")
eval("'s' * 's'")
eval("5*'h'")
eval("1/0")
eval("0/1")

eval("'x'==='x'")
eval("0*'undefined' * 0")
eval("1*undefined")

eval("'a'-'a'")
eval("0-'a'")

parse("const x = 15.1;console.log(x + 2.1);x - 2")
eval("const x = 15.1")

((1 - 1) << 1)
(1 << (1 - 1))
((1 - 1) * 2)
((1 * 2) - 1)

/*
eval(Binary(Minus,S("x"),N(1)))

eval(Binary(Plus,S("x"),S("y")))

var f=Map[String,Int]()
f+("X"->9)
f

parse("const x = 15.1;console.log(x + 2.1);x - 2")

eval("const x = 15.1;console.log(x + 2.1);x - 2")
*/