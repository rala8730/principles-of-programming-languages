package jsy.student

import jsy.lab1._

object Lab1 extends jsy.util.JsyApplication with jsy.lab1.Lab1Like {

  import jsy.lab1.Parser
  import jsy.lab1.ast._

  /*
   * CSCI 3155: Lab 1
   * <Rasmi>
   *
   * Partner: < theodore spencer>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function. The
   * '???' expression is a Scala expression that throws a NotImplementedError
   * exception.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert.  Simply put in a
   * '???' as needed to get something that compiles without error.
   */

  /*
   * Example: Test-driven development of plus
   *
   * A convenient, quick-and-dirty way to experiment, especially with small code
   * fragments, is to use the interactive Scala interpreter. The simplest way
   * to use the interactive Scala interpreter in IntelliJ is through a worksheet,
   * such as Lab1Worksheet.sc. A Scala Worksheet (e.g., Lab1Worksheet.sc) is code
   * evaluated in the context of the project with results for each expression
   * shown inline.
   *
   * Step 0: Sketch an implementation in Lab1.scala using ??? for unimmplemented things.
   * Step 1: Do some experimentation in Lab1Worksheet.sc.
   * Step 2: Write a test in Lab1Spec.scala, which should initially fail because of the ???.
   * Step 3: Fill in the ??? to finish the implementation to make your test pass.
   */

  //def plus(x: Int, y: Int): Int = ???
  def plus(x: Int, y: Int): Int = x + y

  /* Exercises */
  //takes the
  def abs(n: Double): Double = {
    if (n < 0) n * (-1) else n
  }
//returns of exclusive-or of a and b, returns true if an only if exactly one of a or b is true
  def xor(a: Boolean, b: Boolean): Boolean = {
    if (a == b) false
    else true
  }
//returns a string with n copies of s concatenated together
  def repeat(s: String, n: Int): String = n match {
    /*def repeat_one(s: String, n: Int, xxx: String): String = {
      if (n < 0) throw new java.lang.IllegalArgumentException
      if (n == 0) xxx
      else repeat_one(s, n - 1, xxx + s)//tail recursion
      //prints xxx and 3 n-1 times
    }
    repeat_one(s, n, "")*/
    //prints s n timess

    case 0 => ""
    case n if(n>0)=> repeat(s,n-1)+s
  }
/*
  def height(t: SearchTree): Int = t match {
    case  Empty=>0
    case Node(l,d,r) => max(height(l),height(r))+1

  }
  */
//implementing a square root function  by making two different fuctions inside functions which is f(xn) and f'(xn)
  //takes one steps of approximation
  def sqrtStep(c: Double, xn: Double): Double = {
    def xof_n(c: Double, xn: Double): Double = {
      return xn * xn - c
    }
    def x_prime(xn: Double): Double = {
      return 2 * xn
    }
    return xn - (xof_n(c, xn) / x_prime(xn))
  }
//implememnted square root of function in  n time using square root  step
  def sqrtN(c: Double, x0: Double, n: Int): Double = {
    require(n>=0)
    if(n <1){
      return x0
    }
    else sqrtN(c, sqrtStep(c, x0), n-1)
  }
  //implementing the error for square root within epsilson
  def sqrtErr(c: Double, x0: Double, epsilon: Double): Double = {
    if (epsilon<=0) throw new java.lang.IllegalArgumentException
    if (abs(x0*x0-c) < epsilon){
      return x0
    }
    else sqrtErr(c, sqrtStep(c, x0), epsilon)
  }
//my other function
  def sqrt(c: Double): Double = {
    require(c >= 0)
    if (c == 0) 0 else sqrtErr(c, 1.0, 0.0001)
  }

  /* Search Tree */

  // Defined in Lab1Like.scala:
  //
  // sealed abstract class SearchTree
  // case object Empty extends SearchTree
  // case class Node(l: SearchTree, d: Int, r: SearchTree) extends SearchTree

  def repOk(t: SearchTree): Boolean = {
    def check(t: SearchTree, min: Int, max: Int): Boolean = t match {
      case Empty => true
      case Node(l, d, r) => check(l,min,d) && check(r,d,max) && (d >= min) && (d < max)
    }
    check(t, Int.MinValue, Int.MaxValue)
  }
//checking the binary search tree is a valid tree
  def insert(t: SearchTree, n: Int): SearchTree = t match {
    case Empty =>Node(Empty,n,Empty)
    case Node(l,d,r)=>{
      if(n>=d)
        //go to right
        Node(l,d,insert(r,n))
        // if less go to left
      else Node(insert(l,n),d,r)
    }
  }
//deletes the minimum node of tree==leftmost node
  def deleteMin(t: SearchTree): (SearchTree, Int) = {
    require(t != Empty)
    (t: @unchecked) match {
      case Node(Empty, d, r) => (r, d)
      case Node(l, d, r) =>
        val (l1, m) = deleteMin(l)
        (Node(l1,d,r),m)
    }
  }
//delets the nodes
  def delete(t: SearchTree, n: Int): SearchTree = t match {
    case Empty =>t
    case Node(Empty, d, Empty) => if (d == n) Empty else t
    //Base case: if empty delete the parent
    case Node(Empty,d,r)=> if(d==n) r else Node(Empty,d,delete(r,n))
    case Node(l,d,Empty)=> if(d==n) l else Node(delete(l,n),d,Empty)
    case Node(l,d,r)=>{
      if (d==n){
        val(r1,v)= deleteMin(r)
        Node(l,v,r1)
      }
      else if(d>n) Node(delete(l,n),d,r)
      else Node(l,d,delete(r,n))
    }
      //if not empty delete left recursiverly and right recursively
  }

  def sum(t:SearchTree):Int= t match {
    case Empty => 0
    case Node(l,d,r)=> sum(l)+sum(r)+d
  }

  /* JavaScripty */
//
  def eval(e: Expr): Double = e match {
    case N(n) => n
    case Unary(Neg, e1: Expr) => -eval(e1)
      //returns the opposite
    case Binary(Plus, e1: Expr, e2: Expr) => eval(e1) + eval(e2)
    case Binary(Minus, e1: Expr, e2: Expr) => eval(e1) - eval(e2)
    case Binary(Times, e1: Expr, e2: Expr) => eval(e1) * eval(e2)
    case Binary(Div, e1: Expr, e2: Expr) => eval(e1) / eval(e2)
    case _ => ???
  }

 // Interface to run your interpreter from a string.  This is convenient
 // for unit testing.
  override def eval(s: String): Double = eval(Parser.parse(s))



 /* Interface to run your interpreter from the command-line.  You can ignore the code below. */

 def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

    println(prettyNumber(v))
  }

}