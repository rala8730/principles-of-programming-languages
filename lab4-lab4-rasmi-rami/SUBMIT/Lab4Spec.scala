package jsy.student

import jsy.lab4.Lab4Like
import jsy.lab4.ast._
import jsy.lab4.Parser.parse
import jsy.tester.JavascriptyTester
import org.scalatest._

class Lab4Spec(lab4: Lab4Like) extends FlatSpec {
  import lab4._

  /***** Higher-Function Exercises Tests *****/

  "compressRec/compressFold" should "compress List(1, 2, 2, 3, 3, 3)" in {
    val l1 = List(1, 2, 2, 3, 3, 3)
    val gold1 = List(1, 2, 3)
    assertResult(gold1) { compressRec(l1) }
    assertResult(gold1) { compressFold(l1) }
  }

  "mapFirst" should "map the first element where f returns Some" in {
    val l1 = List(1, 2, -3, 4, -5)
    val gold1 = List(1, 2, 3, 4, -5)
    assertResult(gold1) {
      mapFirst(l1) { (i: Int) => if (i < 0) Some(-i) else None }
    }
  }

  "foldLeft" should "enable implementing treeFromList and sum" in {
    assertResult(6){
      sum(treeFromList(List(1, 2, 3)))
    }
  }

  "strictlyOrdered" should "check strict ordering of a binary search tree" in {
    assert(!strictlyOrdered(treeFromList(List(1,1,2))))
    assert(strictlyOrdered(treeFromList(List(1,2))))
  }


  /***** Interpreter Tests *****/

  {
    val xtype = TNumber
    val tenvx = extend(empty, "x", xtype)

    "TypeVar" should "perform TypeVar" in {
      assertResult(xtype) {
        typeof(tenvx, Var("x"))
      }
    }

      "Typeneg" should "preforem typeNeg" in {
        //val xtype=TNumber
        assertResult(TNumber) {
          typeof(tenvx, Unary(Neg, N(3)))
        }
      }
    "plus Tnumber" should "return the type Tnumber" in {
      val e = parse("3+4")
      //typeof(empty,e)
      assertResult(typeof(empty,e)){TNumber}
    }
     it should " add string" in {
       val e = parse("'hello' + 'i'")
       assertResult(typeof(empty,e)){
         TString
       }
     }

    "EQ" should "return true for equal" in {
      val e = parse("3===5")
      //val e1 = parse("5")
      val e1 = parse("f")
      assertResult(typeof(empty, e)){ TBool}
    }

    // Probably want to write some more tests for typeInfer, substitute, and step.

    //getfield
    //step search
    "seatch- GETFIELD" should "step on e1" in {
      val map0:Map[String,Expr]=empty
      val f="hello"
      val map1:Map[String,Expr]=map0 + (f-> Binary(Plus,N(4),N(2)))
      val e1 = Obj(map1)
      val e_f = GetField(e1,f)
      val e_f_p= GetField(Obj(map0 + (f-> N(6))),f)

      assertResult(e_f_p){step(e_f)}
    }

  }

}

// An adapter class to pass in your Lab4 object.
class Lab4SpecRunner extends Lab4Spec(jsy.student.Lab4)

// The next bit of code runs a test for each .jsy file in src/test/resources/lab4.
// The test expects a corresponding .ans file with the expected result.
class Lab4JsyTests extends JavascriptyTester(None, "lab4", jsy.student.Lab4)

class Lab4Suite extends Suites(
  new Lab4SpecRunner,
  new Lab4JsyTests
)