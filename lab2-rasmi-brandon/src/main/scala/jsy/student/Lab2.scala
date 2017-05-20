package jsy.student

import jsy.lab2.{Lab2Like, ast}

object Lab2 extends jsy.util.JsyApplication with Lab2Like {
  import jsy.lab2.Parser
  import jsy.lab2.ast._

  /*
   * CSCI 3155: Lab 2
   * <Rasmi Lamichhane>
   * 
   * Partner: <Brandon Aguirre>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   * 
   * Replace the '???' expression with  your code in each function.
   * 
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   * 
   * Your lab will not be graded if it does not compile.
   * 
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   *
   */

  /* We represent a variable environment as a map from a string of the
   * variable name to the value to which it is bound.
   * 
   * You may use the following provided helper functions to manipulate
   * environments, which are just thin wrappers around the Map type
   * in the Scala standard library.  You can use the Scala standard
   * library directly, but these are the only interfaces that you
   * need.
   */



  /* Some useful Scala methods for working with Scala values include:
   * - Double.NaN
   * - s.toDouble (for s: String)
   * - n.isNaN (for n: Double)
   * - n.isWhole (for n: Double)
   * - s (for n: Double)
   * - s format n (for s: String [a format string like for printf], n: Double)
   *
   * You can catch an exception in Scala using:
   * try ... catch { case ... => ... }
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case S("")=>0.0 //??????
      case S(s)=> try s.toDouble catch {
        case _: Throwable => Double.NaN
      }
      case B(b) => if(b)1 else 0
      case Undefined => Double.NaN
      case _ => throw new UnsupportedOperationException
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case Undefined=>false
      case S(s)=>{ if(s=="") false else true}
      case B(b)=>b
      case N(n)=> if(n==0)false
                  else if (n.isNaN) false
                  else true
      case _ => throw new UnsupportedOperationException
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
      case N(n)=>if(n.isWhole())(n.toInt).toString else n.toString
      case B(b)=>b.toString
      case Undefined => "undefined"
      case _ => throw new UnsupportedOperationException
    }
  }

  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(n)=>N(n)
      case S(s)=>S(s)
      case B(b)=>B(b)
      case Undefined=>Undefined
      case Var(x)=>eval(env,lookup(env,x))
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

      case ConstDecl(x, e1, e2) => eval(extend(env,x,eval(env,e1)), e2)

      case If(e1, e2, e3) => {
        if (toBoolean(e1))
          eval(env, e2)
        else
          eval(env, e3)
      }
      case Unary(uop,e1)=> uop match {
        case Neg => N(-toNumber(eval(env,e1)))
        case Not => B(!toBoolean(eval(env,e1)))
        case _=>throw new UnsupportedOperationException
      }
      case Binary(bop,e1,e2) => bop match {
        case Plus => (eval(env,e1),eval(env,e2)) match {
          case (S(s),x)=>S(s+toStr(x))
          case (x,S(s))=>S(toStr(x)+s)
          case (x, y) =>N(toNumber(x)+toNumber(y))
        }
        case Minus=>(eval(env,e1),eval(env,e2)) match {
          case (N(n1), N(n2)) => N(n1 - n2)
          case (x,y) => N(toNumber(eval(env,x)) - toNumber(eval(env,y)))
          case  _=>Undefined
        }
        case Div=>(eval(env,e1),eval(env,e2))match {
          case(N(x),N(y))=>N(x/y)
          case(x,y)=>N(toNumber(x)/toNumber(y))
          case _=>Undefined
        }
        case Times=>(eval(env,e1),eval(env,e2)) match{
          case(x,y)=>N(toNumber(x)*toNumber(y))
          case _=>N(Double.NaN)
        }
        case Eq=>(eval(env,e1),eval(env,e2)) match{
          case (N(x),N(y))=>B(x==y)
          case (x,y)=>B(x==y)
          case _=>B(false)
        }
        case Ne=>(eval(env,e1),eval(env,e2)) match{
          case(N(x),N(y))=>if(N(x)==N(y)) B(false) else B(true)
          case(x,y)=>if(x==y) B(false) else B(true)
          case _=>B(false)
        }
        //return true if e1 is less than e2, B(e1<e2) returns true  so we dont need to do it again
        case Lt=>(eval(env,e1),eval(env,e2)) match {
          case (S(x),S(y))=>B(x<y)
          case (x,y)=>B(toNumber(x)<toNumber(y))
        }
        case Le=>(eval(env,e1),eval(env,e2)) match {
          case (S(x),S(y))=>B(x<=y)
          case (x,y)=>B(toNumber(x)<=toNumber(y))
        }
        case Gt=>(eval(env,e1),eval(env,e2)) match {
          case (S(x),S(y))=>B(x>y)
          case (x,y)=>B(toNumber(x)>toNumber(y))
        }
        case Ge=>(eval(env,e1),eval(env,e2)) match {
          case (S(x),S(y))=>B(x>=y)
          case (x,y)=>B(toNumber(x)>=toNumber(y))
        }
        case Seq => eval(env,e1);eval(env,e2)

        case And=>(eval(env,e1),eval(env,e2)) match {
          case (Print(_), _) => eval(env, e1)
          case (Undefined,_)|(_,Undefined)=>Undefined
          case (x,y)=>
            if (toBoolean(eval(env, x)) && toBoolean(eval(env, y))) eval(env, y)
            else if (toBoolean(eval(env, x))) eval(env, y)
            else eval(env, x)
          //if(toBoolean(eval(env,x)))eval(env,y) else eval(env,x)
        }
        case Or=>(eval(env,e1),eval(env,e2)) match {
          case (Print(_), v) => eval(env, e1); eval(env, v)
          case(v1,v2)=>
            if (toBoolean(eval(env, v1))) eval(env, v1)
            else eval(env, v2)
          //if(toBoolean(eval(env,x)))eval(env,x) else if(toBoolean(eval(env,y)))eval(env,y) else eval(env,y)
        }
        }
      case _ => throw new UnsupportedOperationException
    }
  }
  /* Interface to run your interpreter from the command-line.  You can ignore what's below. */
  def processFile(file: java.io.File) {
    if (debug) { println("Parsing ...") }

    val expr = Parser.parseFile(file)

    if (debug) {
      println("\nExpression AST:\n  " + expr)
      println("------------------------------------------------------------")
    }

    if (debug) { println("Evaluating ...") }

    val v = eval(expr)

     println(pretty(v))
  }

}
