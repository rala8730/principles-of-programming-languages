package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._

  /*
   * CSCI 3155: Lab 3
   * <Rasmi Lamichhane>
   *
   * Partner: <Alex Ring>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
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
   */

  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(false) => 0
      case B(true) => 1
      case Undefined => Double.NaN
      case S("") => 0
      case S(s) => try s.toDouble catch {case e: NumberFormatException => Double.NaN}
      case Function(_, _, _) => Double.NaN
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case B(b) => b
      case Function(_, _, _) => true
      case N(n) =>
        if(n==0) false
        else if (n.isNaN) false
        else true
      case S("") => false
      case S(_) => true
      case Undefined => false
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case S(s) => s
        // Here in toStr(Function(_, _, _)), we will deviate from Node.js that returns the concrete syntax
        // of the function (from the input program).
      case Function(_, _, _) => "function"
      case B(b) => b.toString
      case N(n) => if(n.isWhole()) n.toInt.toString else n.toString
      case Undefined => "undefined"
      case _ => throw new UnsupportedOperationException
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case _ =>
        val (n1, n2) = (toNumber(v1), toNumber(v2))
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */

  /*
   * Start by copying your code from Lab 2 here.
   */
  def eval(env: Env, e: Expr): Expr = {
    /* We now have to account for functions
    *
    */
    //println(e)
    e match {
      /* Base Cases */
      case N(n) => N(n)
      case B(b) => B(b)
      case S(s) => S(s)
      case Undefined => Undefined
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Var(x) => lookup(env,x)
      case _ if isValue(e) => e
      /* Inductive Cases */
      case Print(e1) => println(pretty(eval(env, e1))); Undefined

        // ****** Your cases here
      case ConstDecl(x,e1,e2) => eval(extend(env,x,eval(env,e1)),e2)

      case Unary(Neg, e1) => N(-toNumber(eval(env,e1)))
      case Unary(Not, e1) => B(!toBoolean(eval(env,e1)))
      case Unary(_,_) => Undefined

      case Binary(And,e1,e2) =>
        if(toBoolean(eval(env,e1)))
          eval(env,e2)
        else
          eval(env,e1)

      case Binary(Or,e1,e2) =>
        if(toBoolean(eval(env,e1)))
          eval(env,e1)
        else
          eval(env,e2)

      case Binary(Plus, e1, e2) => (eval(env,e1),eval(env,e2)) match{
        case (S(_), _) | (_, S(_)) => S(toStr(eval(env, e1)) + toStr(eval(env, e2)))
        case _ => N(toNumber(eval(env, e1)) + toNumber(eval(env, e2)))
      }

      case Binary(Minus,e1,e2) => N(toNumber(eval(env, e1)) - toNumber(eval(env, e2)))

      case Binary(Times,e1,e2) => N(toNumber(eval(env, e1)) * toNumber(eval(env, e2)))

      case Binary(Div,e1,e2) => N(toNumber(eval(env, e1)) / toNumber(eval(env, e2)))

      case Binary(Eq,e1,e2) => (e1,e2) match{
        case ((Function(_,_,_)),_) => throw new DynamicTypeError(e)
        case (_, Function(_,_,_)) => throw new DynamicTypeError(e)
        case _ => B(eval (env, e1) == eval (env, e2) )
      }
      case Binary(Ne,e1,e2) => (e1,e2) match {
        case (Function(_, _, _),_) => throw new DynamicTypeError(e)
        case (_, Function(_,_, _)) => throw new DynamicTypeError(e)
        case _ => B(eval (env, e1) != eval (env, e2) )
      }


      case Binary(Lt,e1,e2) => (eval(env,e1),eval(env,e2)) match{
        case (S(_), S(_)) => B(toStr(eval(env, e1)) < toStr(eval(env, e2)))
        case _ => B(toNumber(eval(env, e1)) < toNumber(eval(env, e2)))
      }

      case Binary(Le,e1,e2) => (eval(env,e1),eval(env,e2)) match{
        case (S(_), S(_)) => B(toStr(eval(env, e1)) <= toStr(eval(env, e2)))
        case _ => B(toNumber(eval(env, e1)) <= toNumber(eval(env, e2)))
      }

      case Binary(Gt,e1,e2) => (eval(env,e1),eval(env,e2)) match{
        case (S(_), S(_)) => B(toStr(eval(env, e1)) > toStr(eval(env, e2)))
        case _ => B(toNumber(eval(env, e1)) > toNumber(eval(env, e2)))
      }

      case Binary(Ge,e1,e2) => (eval(env,e1),eval(env,e2)) match{
        case (S(_), S(_)) => B(toStr(eval(env, e1)) >= toStr(eval(env, e2)))
        case _ => B(toNumber(eval(env, e1)) >= toNumber(eval(env, e2)))
      }


      case Binary(Seq,e1,e2) =>
        eval(env,e1)
        eval(env,e2)

      case Binary(_,_,_) => Undefined

      case If(e1,e2,e3) =>
        if(toBoolean(eval(env,e1)))
          eval(env,e2)
        else
          eval(env,e3)


      //case Print(e1) => println(pretty(eval(env,e1))); Undefined

      case Call(e1, e2) => (eval(env,e1),eval(env,e2)) match{
        case (Function(None,x,body),v2) => eval(extend(env,x,v2),body)
        case (v1 @ Function(Some(x1),x2,body),v2) => eval(extend(extend(env,x2,v2),x1,v1),body)
        case (_, _) => throw new DynamicTypeError(e)
      }
    }
  }


  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e,n) match{
      case None => e
      case Some(x) => loop(x, n+1)
    }
    loop(e0, 0)
  }

  // Immediately substitute the information that you need, one step at a time
  // Not lazy evaluation
  // We do not want to recurse the entire expression, return at each case


  //Look at the pdf. Do's on top searches on bottom
  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, v, x))
      case Unary(uop, e1) => Unary(uop,substitute(e1,v,x))
      case Binary(bop, e1, e2) => Binary(bop,substitute(e1,v,x),substitute(e2,v,x))
      case If(e1, e2, e3) => If(substitute(e1,v,x),substitute(e2,v,x),substitute(e3,v,x))
      case Call(e1, e2) => Call(substitute(e1,v,x),substitute(e2,v,x))
      case Var(y) => if(y == x) v else Var(y)
      case Function(None, y, e1) =>
        if (y==x)
          Function(None,y,e1)
        else
          Function(None, y, substitute(e1,v,x))

      case Function(Some(y1), y2, e1) =>
        if(y1 ==x || y2 == x)
          Function(Some(y1),y2,e1)
        else
          Function(Some(y1), y2, substitute(e1,v,x))

      case ConstDecl(y, e1, e2) =>
        if (x==y)
          ConstDecl(y,substitute(e1,v,x), e2)
        else
          ConstDecl(y,substitute(e1,v,x), substitute(e2,v,x))
      case _ => throw new UnsupportedOperationException
    }
  }

  // IN the PDF ' means one step, use judgement forms to write this code for you
  def step(e: Expr): Expr = {

    // println(e)

    e match {
      //Print DO RULE
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined

      // Unary DO RULES
      case Unary(Neg,e1) if isValue(e1) => N(-toNumber(e1))
      case Unary(Not,e1) if isValue(e1) => B(!toBoolean(e1))

      //And DO RULES
      case Binary(And,e1,e2) if isValue(e1) =>
        if(toBoolean(e1)) e2
        else e1

      //Or DO RULES
      case Binary(Or,e1,e2) if isValue(e1) =>
        if(toBoolean(e1)) e1
        else e2

      // Minus, times, div DO RULE
      case Binary(bop @ (Minus|Times|Div), e1, e2) if isValue(e1) && isValue(e2) => bop match {
        case Minus => N(toNumber(e1) - toNumber(e2))
        case Times => N(toNumber(e1) * toNumber(e2))
        case Div => N(toNumber(e1) / toNumber(e2))
      }

      //Plus DO RULES
      case Binary(Plus,e1,e2) if isValue(e1) && isValue(e2) => (e1,e2) match{
        case (S(s),x) => S(s+toStr(x))
        case (x,S(s)) => S(toStr(x) + s)
        case (x,y) => N(toNumber(x) + toNumber(y))
      }

      //Seq DO RULE
      case Binary(Seq,v1,e2) if isValue(v1) => e2

      //Inequality DO RULES
      case Binary(bop @(Gt|Lt|Ge|Le) ,e1,e2) if isValue(e1) && isValue(e2) => (e1,e2) match{
        case (S(x),S(y)) => bop match{
          case Ge => B(toStr(e1) >= toStr(e2))
          case Gt => B(toStr(e1) > toStr(e2))
          case Le => B(toStr(e1) <= toStr(e2))
          case Lt => B(toStr(e1) < toStr(e2))
        }
        case (x,y) => bop match{
          case Ge => B(toNumber(x) >= toNumber(y))
          case Gt => B(toNumber(x) > toNumber(y))
          case Le => B(toNumber(x) <= toNumber(y))
          case Lt => B(toNumber(x) < toNumber(y))
        }
      }

      //Equality DO RULES
      case Binary(bop @(Eq|Ne),e1,e2) if isValue(e1) && isValue(e2) => bop match{
        case Eq => (e1,e2) match{
          case (S(x),S(y)) => B(x == y)
          case (N(x), N(y)) => B(x == y)
          case (B(x), B(y)) => B(x == y)
          case (Function(_,_,_),_) => throw DynamicTypeError(e)
          case (_,Function(_,_,_)) => throw DynamicTypeError(e)
          case _ => B(false)
        }
        case Ne => (e1,e2) match{
          case (S(x),S(y)) => B(x != y)
          case (N(x), N(y)) => B(x != y)
          case (B(x), B(y)) => B(x != y)
          case (Function(_,_,_),_) => throw DynamicTypeError(e)
          case (_,Function(_,_,_)) => throw DynamicTypeError(e)
          case _ => B(true)
        }
      }

      // If DO RULE
      case If(e1,e2,e3) if isValue(e1)=> toBoolean(e1) match{
        case true => e2
        case false => e3
      }
      //Const DO RULE
      case ConstDecl(x,v1,e2) if isValue(v1) => substitute(e2,v1,x)

      // Call DO RULE
      case Call(v1,v2) if isValue(v1) && isValue(v2) => v1 match{
        case Function(None,x,e1) => substitute(e1,v2,x)
        case Function(Some(x1),x2,e1) => substitute(substitute(e1,v1,x1),v2,x2)
        case _ => throw DynamicTypeError(e)
      }

      //-------- SEARCH RULES ---------//
      //Print SEARCH RULE
      case Print(e1) => Print(step(e1))

      // Unary SEARCH RULE
      case Unary(uop, e1) => Unary(uop, step(e1)) //from recitation

      // General Binary SEARCH RULE
      case Binary(bop,e1,e2) if isValue(e1) => Binary(bop,e1,step(e2))
      case Binary(bop,e1,e2) => Binary(bop,step(e1),e2)

      // If SEARCH RULE
      case If(e1,e2,e3) => If(step(e1),e2,e3)

      //Const SEARCH RULE
      case ConstDecl(x,e1,e2) => ConstDecl(x,step(e1),e2)

      // Call SEARCH RULE
      case Call(e1,e2) => e1 match{
        case Function(_,_,_) => Call(e1,step(e2))
        case _ => Call(step(e1),e2)
      }


      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */

  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}
