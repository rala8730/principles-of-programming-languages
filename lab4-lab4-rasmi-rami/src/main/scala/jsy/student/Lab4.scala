package jsy.student

import jsy.lab4.{Lab4Like, ast}

import scala.reflect.internal.Trees

object Lab4 extends jsy.util.JsyApplication with Lab4Like {
  import jsy.lab4.ast._
  import jsy.lab4.Parser

  /*
   * CSCI 3155: Lab 4
   * <Rami AlQunaibit>
   *
   * Partner: <Rasmi Lamichhane>
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
   * '???' as needed to get something that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /* Collections and Higher-Order Functions */

  /* Lists */

  def compressRec[A](l: List[A]): List[A] = l match {
    case Nil | _ :: Nil => l
    case h1 :: (t1 @ (h2 :: _)) => if (h1 == h2) compressRec(t1) else h1 :: compressRec(t1)
  }

  def compressFold[A](l: List[A]): List[A] = l.foldRight(Nil: List[A]){
    (h, acc) => acc match {
      case Nil => h :: acc
      case h1 :: t => if (h == h1) acc else h :: acc
    }
  }

  def mapFirst[A](l: List[A])(f: A => Option[A]): List[A] = l match {
    case Nil => l
    case h :: t => f(h) match {
      case Some(x) => x :: t
      case None => h :: mapFirst(t)(f)
    }
  }

  /* Trees */

  def foldLeft[A](t: Tree)(z: A)(f: (A, Int) => A): A = {
    def loop(acc: A, t: Tree): A = t match {
      case Empty => acc
      case Node(l, d, r) => loop(f(loop(acc, l), d), r)
    }
    loop(z, t)
  }

  // An example use of foldLeft
  def sum(t: Tree): Int = foldLeft(t)(0){ (acc, d) => acc + d }

  // Create a tree from a list. An example use of the
  // List.foldLeft method.
  def treeFromList(l: List[Int]): Tree =
  l.foldLeft(Empty: Tree){ (acc, i) => acc insert i }

  def strictlyOrdered(t: Tree): Boolean = {
    val (b, _) = foldLeft(t)((true, None: Option[Int])){
      (acc, d) =>
        acc match {
          case (b1, None) => (b1, Some(d))
          case (b2, nextInt) => (nextInt.get < d && b2, Some(d))
        }
    }
    b
  }

  /* Type Inference */

  // While this helper function is completely given, this function is
  // worth studying to see how library methods are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env(x)
      case Decl(mode, x, e1, e2) => typeof(extend(env,x,typeof(env,e1)),e2)
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
      case Binary(Plus, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TString, TString) => TString
        case (TNumber,y)=> err(y,e2)
        case (TString,y)=>err(y,e2)
        case(x,y) => err(x,e1)
      }
      case Binary(Minus|Times|Div, e1, e2) => (typeof(env,e1),typeof(env,e2)) match {
        case (TNumber,TNumber) =>TNumber
        case (TNumber,y)=> err(y,e2)
        case(x,y) => err(x,e1)
      }
      case Binary(Eq|Ne, e1, e2) => (typeof(env,e1),typeof(env,e2)) match {
        case (TNumber,TNumber) =>TBool
        case (TString,TString) =>TBool
        case (TBool,TBool) =>TBool
        case (TUndefined,TUndefined)=>TBool
        case (TFunction(_, _),_) => err(typeof(env,e1),e1)
        case (TObj(_),TObj(_))=>TBool
        case (x,y)=> err(y,e2)
      }
      case Binary(Lt|Le|Gt|Ge, e1, e2) => (typeof(env,e1),typeof(env,e2)) match{
        case (TNumber,TNumber)=> TBool
        case (TString,TString) =>TBool
        case (TNumber,y)=> err(y,e2)
        case (TString,y)=>err(y,e2)
        case(x,y) => err(x,e1)
      }
      case Binary(And|Or, e1, e2) => (typeof(env,e1),typeof(env,e2)) match {
        case (TBool,TBool) =>TBool
        case (TBool,_)=>err(typeof(env,e2),e2)
        case(x,y) => err(x,e1)
      }
      case Binary(Seq, e1, e2) => typeof(env, e1); typeof(env, e2)

      case If(e1, e2, e3) => (typeof(env,e1),typeof(env,e2),typeof(env,e3)) match {
        case (TBool,x,y)=>if (x==y) x else err(x,e2)
        case _ => err(typeof(env,e1),e1)
      }
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          /***** Add cases here *****/
          case (Some(f), Some(rt)) => extend(env, f, TFunction(params, rt))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env1) {
          case (aEnv, (xi, MTyp(_, ti))) => extend(aEnv, xi, ti)
        }
        // Infer the type of the function body
        val t1 = typeof(env2, e1)
        // Check with the possibly annotated return type
        tann match {
          case None => ()
          case Some(rt) => if(t1==rt) () else err(t1, e1)
        }
        TFunction(params, t1)
      }
      case Call(e1, args) => typeof(env, e1) match {   /*We should check this one too*/
        case TFunction(params, tret) if (params.length == args.length) =>
          (params zip args).foreach {
            case ((name, MTyp(_, typ)), ex) => if(typ!=typeof(env, ex)) err(typ, ex)
          };
          tret
        case tgot => err(tgot, e1)
      }
      case Obj(fields) => TObj(fields.mapValues((exp: Expr)=>typeof(env, exp)))
      case GetField(e1, f) => typeof(env, e1) match {
        case TObj(fields) => fields get f match {
          case None => err(typeof(env, e1), e1)
          case Some(t) => t
        }
        case _ => throw StuckError(e)
      }
    }
  }


  /* small-step interpreter */

  /*
   * helper function that implements the semantics of inequality
   * operators lt, le, gt, and ge on values.
   *
   * we suggest a refactoring of code from lab 2 to be able to
   * use this helper function in eval and step.
   *
   * this should the same code as from lab 3.
   */
 /* def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityval: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityval: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    ((v1, v2): @unchecked) match {
      case (S(s1), S(s2)) =>
        (bop: @unchecked) match {
          case Lt => s1 < s2
          case Le => s1 <= s2
          case Gt => s1 > s2
          case Ge => s1 >= s2
        }
      case (N(n1), N(n2)) =>
        (bop: @unchecked) match {
          case Lt => n1 < n2
          case Le => n1 <= n2
          case Gt => n1 > n2
          case Ge => n1 >= n2
        }
    }
  }
*/
  /* this should be the same code as from lab 3 */
  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e, n) match{
      case None => e
      case Some(el) => loop(el, n+1)
    }
    loop(e0, 0)
  }

  /* capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1) => Print(substitute(e1, esub, x))
      /***** cases from lab 3 */
      case Unary(uop, e1) => Unary(uop,substitute(e1,esub,x))
      case Binary(bop, e1, e2) => Binary(bop,substitute(e1,esub,x),substitute(e2,esub,x))
      case If(e1, e2, e3) => If(substitute(e1,esub,x),substitute(e2,esub,x),substitute(e3,esub,x))
      case Var(y) => if(y==x) esub else Var(y)
      case Decl(mode, y, e1, e2) => Decl(mode, y, subst(e1), if (x == y) e2 else subst(e2))
      /***** cases needing adapting from lab 3 */
      case Function(p, params, tann, e1) => params.foldLeft(true){(b,param) => if (param._1 == x) false else b} match {
        case true => p match {
          case Some(f) => if (f != x) Function(p, params, tann, subst(e1)) else e //Potential recursive function
          case _ => Function(p, params, tann, subst(e1)) // Anonymous function
        }
        case _ => e
      }

      case Call(e1, args) => Call(substitute(e1, esub, x), args map subst)
      /***** new cases for lab 4 */
      case Obj(fields) => Obj(fields.map{ case (s,e2) => (s,subst(e2)) })
      case GetField(e1, f) => GetField(subst(e1),f)
      case _ => {
        println("The error caused by the value: " + e)
        throw StuckError(e)
      }
      case _ => throw new StuckError(e)

    }

    val fvs = freeVars(e)
    def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
    subst(e)
  }

  /* Rename bound variables in e */
  def rename(e: Expr)(fresh: String => String): Expr = {
    def ren(env: Map[String,String], e: Expr): Expr = {
      e match {
        case N(_) | B(_) | Undefined | S(_) => e
        case Print(e1) => Print(ren(env, e1))

        case Unary(uop, e1) => Unary(uop, ren(env, e1))
        case Binary(bop, e1, e2) => Binary(bop, ren(env, e1), ren(env, e2))
        case If(e1, e2, e3) => If(ren(env, e1), ren(env, e2), ren(env, e3))

        case Var(y) => Var(env.getOrElse(y, y))

        case Decl(mode, y, e1, e2) =>
          val yp = fresh(y)
          Decl(mode, yp, ren(env, e1), ren(env + (y->yp),e2))

        case Function(p, params, retty, e1) => {
          val (pp, envp): (Option[String], Map[String,String]) = p match {
            case None => (None, env)
            case Some(x) =>
              val xp = fresh(x)
              (Some(xp), env + (x -> xp))
          }
          val (paramsp, envpp) = params.foldRight( (Nil: List[(String,MTyp)], envp) ) {
            ???/*case ((envacc, renamedacc), (y, modet)) =>
              val yp = fresh(y)
              (envacc + (y -> yp), (yp, modet) :: renamedacc) */
          }
          ??? // Function(envp, envpp.reverse, retty, ren(paramsp, e1))
        }

        case Call(e1, args) => ???

        case Obj(fields) => Obj(fields map { case (f,e) => (f, ren(env, e)) })
        case GetField(e1, f) => GetField(ren(env, e1), f)
      }
    }
    ren(empty, e)
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst => if(isValue(e)) false else true
    case MName => false
  }

  def step(e: Expr): Expr = {
    require(!isValue(e), s"step: e ${e} to step is a value")
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => println(pretty(v1)); Undefined
      /***** Cases needing adapting from Lab 3. */
      case Unary(Neg, N(v1)) => N(-v1)
      /***** More cases here */
      case Unary(Not, B(b1)) => B(!b1)
      case Binary(Seq, v1, e2) if isValue(v1) => e2
      case Binary(Plus, S(s1), S(s2)) => S(s1 + s2)
      case Binary(Plus, N(n1), N(n2)) => N(n1 + n2)
      case Binary(Lt, S(s1), S(s2)) => B(s1<s2)
      case Binary(Le, S(s1), S(s2)) => B(s1<=s2)
      case Binary(Gt, S(s1), S(s2)) => B(s1>s2)
      case Binary(Ge, S(s1), S(s2)) => B(s1>=s2)
      case Binary(Lt, N(n1), N(n2)) => B(n1<n2)
      case Binary(Le, N(n1), N(n2)) => B(n1<=n2)
      case Binary(Gt, N(n1), N(n2)) => B(n1>n2)
      case Binary(Ge, N(n1), N(n2)) => B(n1<=n2)
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => B(v1 == v2)
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => B(v1 != v2)
      case Binary(And, B(b1), e2) => if (b1) e2 else B(false)
      case Binary(Or, B(b1), e2) => if (b1) B(true) else e2
      case If(B(b1), e2, e3) => if(b1) e2 else e3
      case Decl(m1,x,e1,e2) if !isRedex(m1,e1)=> substitute(e2,e1,x)
      case Decl(m1,x,e1,e2) if !isRedex(m1,e1)=> substitute(e2,e1,x)
      case Call(v1, args) if isValue(v1) =>
        v1 match {
          case Function(p, params, t, ebody) => {
            val pazip = params zip args
            if (pazip.forall{case ((_, MTyp(mi, _)), ei) => !isRedex(mi, ei)}) {
              val e1p = pazip.foldRight(ebody) {
                (e, acc) => substitute(acc, e._2, e._1._1)
              }
              p match {
                case None => e1p
                case Some(x1) => substitute(e1p, Function(p, params, t, ebody), x1)
              }
            }
            else {
              val pazipp = mapFirst(pazip) {
                case ((x, MTyp(mi, t)), ei) => {
                  if (isRedex(mi, ei)) Some((x, MTyp(mi, t)), step(ei))
                  else None
                }
              }
              val newArgs= pazipp.map({case (a, b)=> b})
              Call(v1, newArgs)
            }
          }
          case _ => throw StuckError(e)
        }

      /***** New cases for Lab 4. */

      /* Inductive Cases: Search Rules */
      case Print(e1) => Print(step(e1))
      /***** Cases from Lab 3. */
      case Unary(uop, e1) => Unary(uop, step(e1))
      /***** More cases here */
      case Binary(Minus, N(n1), N(n2)) => N(n1 - n2)
      case Binary(Times, N(n1), N(n2)) => N(n1 * n2)
      case Binary(Div, N(n1), N(n2)) => N(n1 / n2)

      case Binary(bop, v1, e2) if isValue(v1) => Binary(bop, v1, step(e2))
      case Binary(bop, e1, e2) => if (isValue(e1)) Binary(bop, e1, step(e2)) else Binary(bop, step(e1), e2)
      case If(e1, e2, e3) => If(step(e1), e2, e3)
      /***** Cases needing adapting from Lab 3 */
      case GetField(Obj(field), f) => field.get(f) match{
        case Some(v)  => v
        case None => throw new StuckError(e)
      }
      case Call(e1, args) => Call(step(e1), args)
      /***** New cases for Lab 4. */
      case Obj(a) =>{
        val objList= a.toList
        val retList= mapFirst(objList){
          case (fi, ei) => if(isValue(ei)) None else Some(fi, step(ei))
        }
        Obj(retList.toMap)
      }
      case GetField(e1,f) => GetField(step(e1),f)
      case Decl(m, x, e1, e2) => Decl(m, x, step(e1), e2)

      /* Everything else is a stuck error. Should not happen if e is well-typed.
       *
       * Tip: you might want to first develop by comment out the following line to see which
       * cases you have missing. You then uncomment this line when you are sure all the cases
       * that you have left the ones that should be stuck.
       */

      case _ => {
        println("The error caused by the expression: " + e)
        throw StuckError(e)
      }
    }
  }


  /* External Interfaces */

  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
