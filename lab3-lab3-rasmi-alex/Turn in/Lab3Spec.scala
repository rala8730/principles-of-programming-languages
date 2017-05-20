package jsy.student

import jsy.lab3.Lab3Like
import jsy.lab3.Parser.parse
import jsy.lab3.ast._
import jsy.tester.JavascriptyTester
import org.scalatest._

class Lab3Spec(lab3: Lab3Like) extends FlatSpec {
  import lab3._

  "eval/function" should "be considered values" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Var(x))
    val e2 = Function(Some(f), x, Var(x))
    assert(evaluate(e1) == e1)
    assert(evaluate(e2) == e2)
  }
  "Plus" should "plus two number" in{
    val e1=N(1)
    val e2=N(5)
    val e3 =evaluate(Binary(Plus,e1,e2))
    assert(e3===N(6))
  }
  it should "add  number and string " in {
    val e1 = S("x")
    val e2 = N(3)
    val e3 = evaluate(Binary(Plus, e1, e2))
    assert(e3 === S("x3"))
  }
  it should "add string and string"in {
    val e1 = S("x")
    val e2 = S("y")
    val e3 = evaluate(Binary(Plus, e1, e2))
    assert(e3 === S("xy"))
  }
  it should "add number and boolean values and return a number" in {
    val e1 = B(false)
    val e2 = N(2)
    val e3 = evaluate(Binary(Plus, e1, e2))
    assert(e3 === N(2))
  }
  "Eq" should "return true if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Eq, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if two numerical values are not the same" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = evaluate(Binary(Eq, e1, e2))
    assert(e3 === B(false))
  }

  "Ne" should "return true if two numerical values are different" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = evaluate(Binary(Ne, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if two numerical values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Ne, e1, e2))
    assert(e3 === B(false))
  }

  "Lt" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = evaluate(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }
  it should "return true if the first string is less than the second" in {
    val e1 = S("a")
    val e2 = S("b")
    val e3 = evaluate(Binary(Lt, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if the first expression is not strictly less than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = evaluate(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  it should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Lt, e1, e2))
    assert(e3 === B(false))
  }

  "Le" should "return true if the first expression is less than the second" in {
    val e1 = N(5)
    val e2 = N(7)
    val e3 = evaluate(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if the first expression is greater than the second" in {
    val e1 = N(7)
    val e2 = N(5)
    val e3 = evaluate(Binary(Le, e1, e2))
    assert(e3 === B(false))
  }

  it should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if the first string expression is less than or equal to than the second" in {
    val e1 = S("a")
    val e2 = S("b")
    val e3 = evaluate(Binary(Le, e1, e2))
    assert(e3 === B(true))
  }

  "Gt" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = evaluate(Binary(Gt, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if the first expression is not strictly greater than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = evaluate(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  }

  it should "return false if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Gt, e1, e2))
    assert(e3 === B(false))
  }

  "Ge" should "return true if the first expression is greater than the second" in {
    val e1 = N(8)
    val e2 = N(7)
    val e3 = evaluate(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  }

  it should "return false if the first expression is less than the second" in {
    val e1 = N(4)
    val e2 = N(5)
    val e3 = evaluate(Binary(Ge, e1, e2))
    assert(e3 === B(false))
  }

  it should "return true if two number values are the same" in {
    val e1 = N(5)
    val e2 = N(5)
    val e3 = evaluate(Binary(Ge, e1, e2))
    assert(e3 === B(true))
  }
  "Minus" should "subtract two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(1)
    val e3 = evaluate(Binary(Minus, e1, e2))
    assert(e3 === N(2))
  }
  it should "subtract number and boolean values and return a number" in {
    val e1 = B(true)
    val e2 = N(2)
    val e3 = evaluate(Binary(Minus, e1, e2))
    assert(e3 === N(-1))
  }

  "Times" should "multiply two number values and return a number" in {
    val e1 = N(3)
    val e2 = N(2)
    val e3 = evaluate(Binary(Times, e1, e2))
    assert(e3 === N(6))
  }

  "Div" should "divide two number values and return a number" in {
    val e1 = N(8)
    val e2 = N(5)
    val e3 = evaluate(Binary(Div, e1, e2))
    assert(e3 === N(1.6))
  }
  "eval/call" should "evaluate a function using big-step semantics" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Binary(Plus, Var(x), N(1)))
    val e2 = N(2)
    val e3 = evaluate(Call(e1, e2))
    assert(e3 === N(3))
  }

  it should "handle recursive functions using big-step semantics" in {
    val f = "f"
    val x = "x"
    val fbody = If(Binary(Eq, Var(x), N(0)), Var(x), Binary(Plus, Var(x), Call(Var(f), Binary(Minus, Var(x), N(1)))))
    val e1 = Function(Some(f), x, fbody)
    val e2 = N(3)
    val e3 = evaluate(Call(e1, e2))
    assert(e3 === N(6))
  }

  "step/call" should "evaluate a function using small-step semantics" in {
    val f = "f"
    val x = "x"
    val e1 = Function(None, x, Binary(Plus, Var(x), N(1)))
    val e2 = N(2)
    val e3 = iterateStep(Call(e1, e2))
    assert(e3 === N(3))
  }

  it should "handle recursive functions using small-step semantics" in {
    val f = "f"
    val x = "x"
    val fbody = If(Binary(Eq, Var(x), N(0)), Var(x), Binary(Plus, Var(x), Call(Var(f), Binary(Minus, Var(x), N(1)))))
    val e1 = Function(Some(f), x, fbody)
    val e2 = N(3)
    val e3 = iterateStep(Call(e1, e2))
    assert(e3 === N(6))
  }

  "substitute" should "perform syntatic substitution respecting shadowing" in {
    val xplus1 = parse("x + 1")
    val twoplus1 = parse("2 + 1")
    assert(substitute(xplus1, N(2), "x") === twoplus1)
    val constx3 = parse("const x = 3; x")
    val shadowx = Binary(Plus, constx3, Var("x"))


    assert(substitute(shadowx, N(2), "x") === Binary(Plus, constx3, N(2)))
  }

  {
    val one = parse("1")

    "iterate" should "stop if the callback body returns None" in {
      assertResult(one) {
        iterate(one) { (_, _) => None }
      }
    }

    it should "increment the loop counter on each iteration and use e if the callback body returns Some(e)" in {
      assertResult(parse("--1")) {
        iterate(one) { (e: Expr, n: Int) =>
          if (n == 2) None else Some(Unary(Neg, e))
        }
      }
    }
  }

  /* Tests based on rules */

  {
    val xval = N(2)
    val envx = extend(empty, "x", xval)
    val varx = Var("x")

    val e1 = parse("2 - 1 - 1")
    val e1p = parse("1 - 1")
    val e2 = parse("3 - 1 - 1")
    val e2p = parse("2 - 1")
    val v1 = N(0)
    val v2 = N(1)
    val eq1 = B(true)
    val eq2 = B(false)

    val vidfunction = parse("function (x) { return x }")

    "EvalVar" should "perform EvalVar" in {
      assertResult(xval) {
        eval(envx, varx)
      }
    }

    "EvalNeg" should "perform EvalNeg" in {
      val np = -toNumber(v1)
      assertResult(N(np)) {
        eval(envx, Unary(Neg, e1))
      }
    }

    "EvalTypeErrorEquality1" should "perform EvalTypeErrorEquality1" in {
      intercept[DynamicTypeError] {
        eval(envx, Binary(Eq, vidfunction, e2))
      }
    }

    "DoNeg" should "perform DoNeg" in {
      val np = -toNumber(v1)
      assertResult(N(np)) {
        step(Unary(Neg, v1))
      }
    }

    "SearchUnary" should "perform SearchUnary" in {
      assertResult(Unary(Neg, e1p)) {
        step(Unary(Neg, e1))
      }
    }

    "StepEquality" should "return true for equal things" in  {
      val x = S("string")
      val y = S("string")
      val z = S("longstring")
      val a = N(1)
      val b = N(1)
      val c = N(2)
      val d = B(true)
      val e = B(true)
      val f = B(false)

      //eq1 = true
      //eq2 = false

      //Testing Eq
      assertResult(eq1) {
        step(Binary(Eq, x,y)) //matching strings
      }
      assertResult(eq1) {
        step(Binary(Eq, a,b)) //matching ints
      }
      assertResult(eq1) {
        step(Binary(Eq,d,e)) //matching bools
      }
      assertResult(eq2) {
        step(Binary(Eq, x,z)) //mismatched strings
      }
      assertResult(eq2) {
        step(Binary(Eq,a,c)) //mismatched ints
      }
      assertResult(eq2) {
        step(Binary(Eq,d,f)) //mismatched bools
      }

      //Testing Ne
      assertResult(eq2) {
        step(Binary(Ne,x,y)) //matched bools = false
      }
    }
  }
}

// An adapter class to pass in your Lab3 object.
class Lab3SpecRunner extends Lab3Spec(Lab3)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab3.
// The test expects a corresponding .ans file with the expected result.
class Lab3JsyTests extends JavascriptyTester(None, "lab3", Lab3)

class Lab3Suite extends Suites(
  new Lab3SpecRunner,
  new Lab3JsyTests
)
