package jsy.student

import jsy.lab5.Lab5Like

object Lab5 extends jsy.util.JsyApplication with Lab5Like {
  import jsy.lab5.ast._
  import jsy.util.DoWith
  import jsy.util.DoWith._

  /*
   * CSCI 3155: Lab 5
   * <Rasmi LAmichhane>
   *
   * Partner: <Lauren Raddatz>
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

  /*** Exercise with DoWith ***/

  def rename[W](env: Map[String,String], e: Expr)(fresh: String => DoWith[W,String]): DoWith[W,Expr] = {
    def ren(env: Map[String,String], e: Expr): DoWith[W,Expr] = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => doreturn(e)
      case Print(e1) => ren(env,e1) map { e1p => Print(e1p) }

      case Unary(uop, e1) => ren(env, e1) map {e1p => Unary(uop, e1p)}
      case Binary(bop, e1, e2) => ren(env, e1) flatMap { e1p => ren(env, e2) map { e2p => Binary(bop, e1p, e2p) }}
      case If(e1, e2, e3) => ren(env, e1) flatMap { e1p => ren(env, e2) flatMap { e2p => ren(env, e3) map { e3p => If(e1p, e2p, e3p)}}}

      case Var(x) => doreturn(Var(env.getOrElse(x,x)))

      case Decl(m, x, e1, e2) => fresh(x) flatMap { xp =>
        ren(env, e1) flatMap { e1p => ren(extend(env,x,xp), e2) map { e2p => Decl(m, xp, e1p, e2p) }}
      }

      case Function(p, params, retty, e1) => {
        val w: DoWith[W,(Option[String], Map[String,String])] = p match {
          case None => doreturn((p, env))
          case Some(x) => fresh(x) map {
            xp => (Some(x), extend(env, x, xp))
          }
        }
        w flatMap { case (pp, envp) =>
          params.foldRight[DoWith[W,(List[(String,MTyp)],Map[String,String])]]( doreturn((Nil, envp)) ) {
            case ((x,mty), acc) => acc flatMap {
              case (list, env2) => fresh(x) map {
                xp => ((xp, mty) :: list, extend(env, x, xp))
              }
            }
          } flatMap {
            case (listp, env3) => ren(env3, e1) map {
              e1p => Function(pp, listp, retty, e1p)
            }
          }
        }
      }

      case Call(e1, args) => ren(env, e1) flatMap {
        e1p => mapWith(args){
          argsp => ren(env, argsp)
        } map { argsl => Call(e1p, argsl)}
      }

      case Obj(fields) => mapWith(fields) {
        case (f,e) => ren(env, e) map {
          (e1p) => (f, e1p)
        }
      } map { (e1pp) => Obj(e1pp)}

      case GetField(e1, f) => ren(env, e1) map { e1p => GetField(e1p, f)}

      case Assign(e1, e2) => ren(env, e1) flatMap {
        e1p => ren(env, e2) map {
          e2p => Assign(e1p, e2p)
        }
      }

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }
    ren(env, e)
  }

  def myuniquify(e: Expr): Expr = {
    val fresh: String => DoWith[Int,String] = { _ =>
      doget[Int] flatMap {
        (i) =>
          val xp = "x" + i
          doput(i+1) map { (_) => xp}
      }
    }
    val (_, r) = rename(empty, e)(fresh)(0)
    r
  }

  /*** Helper: mapFirst to DoWith ***/

  // List map with an operator returning a DoWith (( from lecture ))
  //mapwith[parameter=>state type, input type, output type]
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) {
      (h, acc) => acc flatMap(xyz => f(h) map(abc => abc :: xyz)) // acc is a doWith
    }
  }

  // Map map with an operator returning a DoWith

  /* from lecture
  def mapWith[W,A,B](l: List[A])(f: A => DoWith[W,B]): DoWith[W,List[B]] = {
    l.foldRight[DoWith[W,List[B]]]( doreturn(Nil) ) {(a,dw)=>
      f(a).flatMap((r:B)=>dw.map((b:List[B])=>r ::b))
    }
  }
 */


  // Map map with an operator returning a DoWith((from lecture))
  def mapWith[W,A,B,C,D](m: Map[A,B])(f: ((A,B)) => DoWith[W,(C,D)]): DoWith[W,Map[C,D]] = {
    m.foldRight[DoWith[W,Map[C,D]]](doreturn(Map.empty)) {(a,dw)=>
      //f(a).flatMap((r:(C,D))=>dw.map((b:Map[C,D])=> b + r))
      f(a).flatMap((r) => dw.map(_ + r))
    }
  }

  // Just like mapFirst from Lab 4 but uses a callback f that returns a DoWith in the Some case.
  //Mapfirst[typeparamater](type parameter:2nd argument)(function that takes in A and return D0with ):(final things it return id do eith with list)
  def mapFirstWith[W,A](l: List[A])(f: A => Option[DoWith[W,A]]): DoWith[W,List[A]] = l match {
    case Nil => doreturn(l)
    case h :: t => f(h) match {
      case None => mapFirstWith(t)(f) map { x => h :: x } //map{h :: _}
      case Some(h1) => h1 map { a => a :: t}
    }
  }

  // There are better ways to deal with the combination of data structures like List, Map, and
  // DoWith, but we won't tackle that in this assignment.

  /*** Casting ***/

  def castOk(t1: Typ, t2: Typ): Boolean = (t1, t2) match {
    /***** Make sure to replace the case _ => ???. */
    case (TNull, TObj(_)) => true
    case(_, _) if (t1 == t2) => true
    case (TObj(fields1), TObj(fields2)) =>
      if (fields1.size < fields2.size) {
        fields1.forall {
          case (f, exp) =>
            if (fields2.contains(f)) {
              if (fields1(f) == fields2(f)) true else false
            } else false
        }
      }
      else {
        fields2.forall {
          case (f, exp) => if(fields1.contains(f)) {
            if (fields2(f) == fields1(f)) true else false
          } else false
      }
    }

      /***** Cases for the extra credit. Do not attempt until the rest of the assignment is complete. */
    //case (TInterface(tvar, t1p), _) => ???
    //case (_, TInterface(tvar, t2p)) => ???
    /***** Otherwise, false. */
    case _ => false
  }

  /*** Type Inference ***/

  // A helper function to check whether a jsy type has a function type in it.
  // While this is completely given, this function is worth studying to see
  // how library functions are used.
  def hasFunctionTyp(t: Typ): Boolean = t match {
    case TFunction(_, _) => true
    case TObj(fields) if (fields exists { case (_, t) => hasFunctionTyp(t) }) => true
    case _ => false
  }

  def isBindex(m: Mode, e: Expr): Boolean = m match {
    case MRef => isLExpr(e)
    case _ => true
  }

  def typeof(env: TEnv, e: Expr): Typ = {
    def err[T](tgot: Typ, e1: Expr): T = throw StaticTypeError(tgot, e1, e)

    e match {
      case Print(e1) => typeof(env, e1); TUndefined
      case N(_) => TNumber
      case B(_) => TBool
      case Undefined => TUndefined
      case S(_) => TString
      case Var(x) => env(x).t
      // TypeNeg
      case Unary(Neg, e1) => typeof(env, e1) match {
        case TNumber => TNumber
        case tgot => err(tgot, e1)
      }

      /***** Cases directly from Lab 4. We will minimize the test of these cases in Lab 5. */
        // TypeSeq
      case Binary(Seq, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (t1, t2) => t2
      }
        // TypeNot
      case Unary(Not, e1) => typeof(env, e1) match {
        case TBool => TBool
        case tgot => err(tgot, e1)
      }
        // TypePluString, TypePlusNumber
      case Binary(Plus, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (TString, TString) => TString
        case (TNumber, tgot) => err(tgot, e2)
        case (TString, tgot) => err(tgot, e2)
        case (tgot, _) => err(tgot, e1)
      }
        // TypeArith
      case Binary(Minus|Times|Div, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TNumber
        case (tgot, _) if (tgot != TNumber) => err(tgot, e1)
        case (_, tgot) if (tgot != TNumber) => err(tgot, e2)
      }
        // TypeEquality
      case Binary(Eq|Ne, e1, e2) => typeof(env, e1) match {
        case t1 if !hasFunctionTyp(t1) => typeof(env, e2) match {
          case t2 if (t1 == t2) => TBool
          case tgot => err(tgot, e2)
        }
        case tgot => err(tgot, e1)
      }
        // TypeInequality
      case Binary(Lt|Le|Gt|Ge, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TNumber, TNumber) => TBool
        case (TString, TString) => TBool
        case (tgot, _) if (tgot != TNumber && tgot != TString) => err(tgot, e1)
        case (_, tgot) if (tgot != TNumber && tgot != TString) => err(tgot, e2)
      }
        // TypeAndOr
      case Binary(And|Or, e1, e2) => (typeof(env, e1), typeof(env, e2)) match {
        case (TBool, TBool) => TBool
        case (tgot, _) if (tgot != TBool) => err(tgot, e1)
        case (_, tgot) if (tgot != TBool) => err(tgot, e2)
      }
       // TypeIf
      case If(e1, e2, e3) => (typeof(env, e1), typeof(env, e2), typeof(env, e3)) match {
        case (TBool, t1, t2) => if (t1 == t2) t1 else err(t2, e3)
        case (tgot, _, _) => err(tgot, e1)
      }
        // TypeObject
      case Obj(fields) => TObj(fields map { case (f,t) => (f, typeof(env, t))})

        // TypeGetField
      case GetField(Null, _) => throw new NullDereferenceError(e)
      case GetField(e1, f) => typeof(env, e1) match {
        case TObj(tfields) if (tfields.contains(f)) => tfields(f)
        case tgot => err(tgot, e1)
      }

      /***** Cases from Lab 4 that need a small amount of adapting. */
        // TypeDecl
      case Decl(m, x, e1, e2) if isBindex(m, e1) =>
        val mt = MTyp(m, typeof(env, e1))
        typeof(env + (x -> mt), e2)

        // TypeFunctionAnn, TypeFunctionRec, TypeFunction
      case Function(p, params, tann, e1) => {
        // Bind to env1 an environment that extends env with an appropriate binding if
        // the function is potentially recursive.
        val env1 = (p, tann) match {
          case (Some(f), Some(tret)) =>
            val tprime = TFunction(params, tret)
            //extend(env, f, MTyp(MConst, tprime))
            env + (f->MTyp(MConst, tprime))
          case (None, _) => env
          case _ => err(TUndefined, e1)
        }
        // Bind to env2 an environment that extends env1 with bindings for params.
        val env2 = params.foldLeft(env) {
          (acc, par) => acc + (par._1 -> par._2) //extend string to the mtyp
        }
        // Match on whether the return type is specified.
        val body_type = typeof(env2, e1)

        tann match {
          //case Some(tret) => if (tret != body_type) err(body_type, e1) else TFunction(params, tret)
          case Some(tret) => TFunction(params, tret)
          case None => TFunction(params, typeof(env2, e1))
        }
      }
        // TypeCall
      case Call(e1, args) => typeof(env, e1) match {
        case TFunction(params, tret) if (params.length == args.length) =>
          (params, args).zipped.foreach {
            (p, a) => if (p._2.t != typeof(env, a) || !isBindex(p._2.m, a)) err(p._2.t, a) //else p._2.t
          }
          tret
        case tgot => err(tgot, e1)
      }

      /***** New cases for Lab 5. ***/
        // TypeAssignVar
//      case Assign(Var(x), e1) => lookup(env, x) match {
//        case MTyp(m, t) if (m == MVar || m == MRef) =>
//          val t1 = typeof(env, Var(x))
//          if (t == typeof(env, e1)) t else err(typeof(env, e1), e1)
//        case _ => throw StuckError(e)
//      }
        // TypeAssignVar

      case Assign(Var(x), e1) => lookup(env, x) match {
        case MTyp(m, t) if (m == MVar || m == MRef) =>
          if (t == typeof(env, e1)) t else err(typeof(env, e1), e1)
        case _ => throw new DynamicTypeError(e)
      }

        // TypeAssignGetField
      case Assign(GetField(Null, _), _) => throw new NullDereferenceError(e)
      case Assign(GetField(e1, f), e2) => typeof(env, e1) match {
        case TObj(fields) if (fields.contains(f)) => {
          val t2 = typeof(env, e2)
          if (fields(f) == t2) t2 else err(t2, e2)
        }
        case _ => err(TUndefined, e1)
      }
      case Assign(_, _) => err(TUndefined, e)

        // TypeNull
      case Null => TNull

      // TypeCast
//      case Unary(Cast(t), e1) => castOk(typeof(env, e1), t) match {
//        case true => t
//        case false => err(typeof(env, e1), e1)
//      }
      case Unary(Cast(t), e1) => typeof(env, e1) match {
        case tgot if castOk(tgot, t) => t
        case tgot=> err(tgot, e1)
      }


      /* Should not match: non-source expressions or should have been removed */
      case A(_) | Unary(Deref, _) | InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
      case _ => throw DynamicTypeError(e)
    }
  }

  /*** Small-Step Interpreter ***/

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   *
   * This should the same code as from Lab 3 and Lab 4.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1), s"inequalityVal: v1 ${v1} is not a value")
    require(isValue(v2), s"inequalityVal: v2 ${v2} is not a value")
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(s1), S(s2)) => bop match {
        case Lt => s1 < s2
        case Le => s1 <= s2
        case Gt => s1 > s2
        case Ge => s1 >= s2
      }
      case (N(n1), N(n2)) => bop match {
        case Lt => n1 < n2
        case Le => n1 <= n2
        case Gt => n1 > n2
        case Ge => n1 >= n2
      }
      case _ => throw DynamicTypeError(v1)
    }
  }

  /* Capture-avoiding substitution in e replacing variables x with esub. */
  def substitute(e: Expr, esub: Expr, x: String): Expr = {
    def subst(e: Expr): Expr = e match {
      case N(_) | B(_) | Undefined | S(_) | Null | A(_) => e
      case Print(e1) => Print(subst(e1))
      /***** Cases from Lab 3 */
      case Unary(uop, e1) => Unary(uop, subst(e1))
      case Binary(bop, e1, e2) => Binary(bop, subst(e1), subst(e2))
      case If(e1, e2, e3) => If(subst(e1), subst(e2), subst(e3))
      case Var(y) => if(y == x) esub else e
      /***** Cases need a small adaption from Lab 3 */
      case Decl(mode, y, e1, e2) => Decl(mode, y, subst(e1), if (x == y) e2 else subst(e2))
      /***** Cases needing adapting from Lab 4 */
//      case Function(p, paramse, retty, e1) => p match {
//        case Some(name) =>
//          if (paramse exists { case (y, _) => x == y }) e
//          else Function(p, paramse, retty, subst(e1))
//        case None =>
//          if (paramse exists { case (y, _) => x == y }) e
//          else Function(p, paramse, retty, subst(e1))
//      }
      case Function(p, paramse, retty, e1) =>
        if(paramse.exists((params: (String, MTyp)) => (params._1 == x || Some(x) == p))) e
        else Function(p, paramse, retty, subst(e1))

      //case Function(p, paramse, retty, e1) => if (p == Some(x) || (paramse exists { case (y, _) => x ==y })) e else Function(p, paramse, retty, subst(e1))

      //if (p == Some(x) || (paramse exists { case (y, _) => x ==y })) e else Function(p, paramse, retty, subst(e1))

      /***** Cases directly from Lab 4 */
      case Call(e1, args) => Call(subst(e1), args.map((arg) => subst(arg)))
      case Obj(fields) => Obj(fields map { case (fi, ei) => (fi, subst(ei))})
      case GetField(e1, f) => GetField(subst(e1), f)
      /***** New case for Lab 5 */
      case Assign(e1, e2) => Assign(subst(e1), subst(e2))

      /* Should not match: should have been removed */
      case InterfaceDecl(_, _, _) => throw new IllegalArgumentException("Gremlins: Encountered unexpected expression %s.".format(e))
    }

    def myrename(e: Expr): Expr = {
      val fvs = freeVars(esub)
      def fresh(x: String): String = if (fvs contains x) fresh(x + "$") else x
      rename[Unit](e)(){ x => doreturn(fresh(x)) }
    }

    subst(myrename(e))
  }

  /* Check whether or not an expression is reduced enough to be applied given a mode. */
  def isRedex(mode: Mode, e: Expr): Boolean = mode match {
    case MConst | MVar => !isValue(e) // reduceable if e is not a value
    case MRef => !isLValue(e)
    case _ => false
  }

  // gets the bound variable from the expression that is bound
  def getBinding(mode: Mode, e: Expr): DoWith[Mem,Expr] = {
    require(!isRedex(mode,e), s"expression ${e} must not reducible under mode ${mode}")
    mode match {
      case MVar => memalloc(e) map { x => Unary(Deref, x) } // e is assigned to the var, get memory for it, and deref it
      case _ => doreturn(e)
    }
  }

  /* A small-step transition. */
  def step(e: Expr): DoWith[Mem, Expr] = {
    require(!isValue(e), "stepping on a value: %s".format(e))
    e match {
      /* Base Cases: Do Rules */
      case Print(v1) if isValue(v1) => doget map { m => println(pretty(m, v1)); Undefined }
      /***** Cases needing adapting from Lab 3. */
      // DoNeg
      case Unary(Neg, N(v1)) =>  doreturn( N(-v1) )
      /***** More cases here */
      // DoNot
      case Unary(Not, B(b1)) => doreturn( B(!b1) )
      // DoPlusString
      case Binary(Plus, S(s1), S(s2)) => doreturn( S(s1 + s2) )
      // DoArith
      case Binary(Plus, N(n1), N(n2)) => doreturn( N(n1 + n2) )
      case Binary(Minus, N(n1), N(n2)) => doreturn( N(n1 - n2) )
      case Binary(Times, N(n1), N(n2)) => doreturn( N(n1 * n2) )
      case Binary(Div, N(n1), N(n2)) => doreturn( N(n1 / n2) )
      // DoSeq
      case Binary(Seq, v1, e2) if isValue(v1) => doreturn( e2 )
      // DoInequalityNumber, DoInequalityString
      case Binary(bop @ (Lt | Le | Gt | Ge), v1, v2) if isValue(v1) && isValue(v2) => doreturn( B(inequalityVal(bop, v1, v2)) )
      // DoEquality
      case Binary(Eq, v1, v2) if isValue(v1) && isValue(v2) => doreturn(B(v1 == v2))
      case Binary(Ne, v1, v2) if isValue(v1) && isValue(v2) => doreturn(B(v1 != v2))
      // DoAndTrue, DoAndFalse
      case Binary(And, B(b1), e2) => doreturn( if (b1) e2 else B(false) )
      // DoOrTrue, DoOrFalse
      case Binary(Or, B(b1), e2) => doreturn( if (b1) B(true) else e2 )
      // DoIfTrue, DoIfFalse
      case If(B(b1), e2, e3) => doreturn( if (b1) e2 else e3 )


      /***** Cases needing adapting from Lab 4. */
        // DoObject
      case Obj(fields) if (fields forall { case (_, vi) => isValue(vi)}) => memalloc(Obj(fields))

        // DoGetField
      case GetField(a @ A(_), f) => doget map { m => m.get(a) match {
        case Some(Obj(fields)) => fields.get(f) match {
          case Some(ep) => ep
          case None => throw new NullDereferenceError(e)
        }
        case _ => throw StuckError(e)
      }}
      case GetField(Null, _) => throw new NullDereferenceError(e)

       // DoDecl
      //case Decl(mode, x, v1, e2) if !isRedex(mode, v1) => getBinding(mode, v1) map { e1p => substitute(e2, e1p, x)}
      case Decl(MConst,x,v1,e2) if isValue(v1) => doreturn(substitute(e2,v1,x) )
      case Decl(MVar, x, v1, e2) if isValue(v1) => getBinding(MVar, v1) map { e1p => substitute(e2, e1p, x)}

      /***** New cases for Lab 5. */
        // DoDeref
        // from help hour
      case Unary(Deref, a @ A(_)) => doget map { state => state.get(a) match {
        case Some(ep) => ep
        case None => Undefined
      }}

        // DoCast
      case Unary(Cast(t), v) if isValue(v) => v match {
        case a @ A(_) => doget map { w =>
          w.get(a) match {
            case Some(Obj(fields)) => t match {
              case TObj(tfields) => {
                if (fields.forall { case (fi,ei) => tfields.get(fi) match {
                  case Some(_) => true
                  case None => false
                }}) a
                else throw StuckError(e)
              }
            }
            case None => throw StuckError(e)
          }
        }
        case Null => t match {
          case TObj(_) | TInterface(_,_) => doreturn(Null)
          case _ => throw StuckError(e)
        }
        case _ => doreturn(v)
      }

        // DoAssignVar
      case Assign(Unary(Deref, a @ A(_)), v) if isValue(v) =>
        domodify {w: Mem => w + (a -> v)} map {_ => v}
        // DoAssignGetField
      case Assign(GetField(a @ A(_), f), v) if isValue(v) => domodify[Mem] {
          (m) => m.get(a) match {
            case Some(Obj(fields)) => m + (a -> Obj(fields + (f -> v)))
            case _ => throw new DynamicTypeError(e)
          }
        } map { _ => v}

        // DoCall, DoCallRec
      case Call(v @ Function(p, params, _, e), args) => {
        val pazip = params zip args
        if (pazip forall { case (((_, MTyp(m, _)), ei)) => !isRedex(m, ei)}) {
          val dwep = pazip.foldRight( doreturn(e) : DoWith[Mem,Expr] )  {
            case (((xi, MTyp(mi, _)), ei), dwacc) => getBinding(mi, ei) flatMap { eip => dwacc map {
              params1 => substitute(params1, ei, xi)
            }}
          }
          p match {
            case None => dwep // anon -- DoCall
            case Some(x) => dwep map { ebody => substitute(ebody, v, x)} // named function - recursive
          }
        }
        else {
          val dwpazipp = mapFirstWith(pazip) {
            case (pi@(_, MTyp(m, _)), ei) if isRedex(m, ei) => Some(step(ei) map { eip => (pi, eip)})
            case _ => None
          }
          dwpazipp map { list => Call(v,list.unzip._2)} // SearchCall2

        }
      }

      /* Base Cases: Error Rules */
      /***** Replace the following case with a case to throw NullDeferenceError.  */
      //case _ => throw NullDeferenceError(e)

      /* Inductive Cases: Search Rules */
      /***** Cases needing adapting from Lab 3. Make sure to replace the case _ => ???. */
      case Print(e1) => step(e1) map { e1p => Print(e1p) }
        // SearchUnary
      case Unary(uop, e1) => step(e1) map { e1p => Unary(uop, e1p) }
       // SearchBinary2
      case Binary(bop, v1, e2) if isValue(v1) => step(e2) map { e2p => Binary(bop, v1, e2p) }
        // SearchBinary1
      case Binary(bop, e1, e2) => step(e1) map { e1p => Binary(bop, e1p, e2) }
         // SearchIf
      case If(e1, e2, e3) => step(e1) map { e1p => If(e1p, e2, e3)}
      /***** Cases needing adapting from Lab 4 */
        // SearchGetField
      case GetField(e1, f) => step(e1) map { e1p => GetField(e1p, f)}
        // SearchObject
      case Obj(fields) => fields find { case (_, e1) => !isValue(e1) } match {
        case Some((f1, e1)) => step(e1) map { e1p => Obj(fields + (f1 -> e1p))}
        case None => throw StuckError(e)
      }
        // SearchDecl
      case Decl(mode, x, e1, e2) if isRedex(mode, e1) => step(e1) map { e1p => Decl(mode, x, e1p, e2)}
        // SearchCall1
      case Call(e1, args) => step(e1) map { e1p => Call(e1p, args)}

      /***** New cases for Lab 5.  */
        // SearchAssign1
      case Assign(e1, e2) if (!isLValue(e1)) => step(e1) map { e1p => Assign(e1p, e2)}
        // SearchAssign2
      case Assign(e1, e2) => step(e2) map { e2p => Assign(e1, e2p)}

      /* Everything else is a stuck error. */
      case _ => throw StuckError(e)
    }
  }

  /*** Extra Credit: Lowering: Remove Interface Declarations ***/

  def lower(e: Expr): Expr =
  /* Do nothing by default. Change to attempt extra credit. */
    e

  /*** External Interfaces ***/

  //this.debug = true // comment this out or set to false if you don't want print debugging information
  this.maxSteps = Some(1000) // comment this out or set to None to not bound the number of steps.
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file
}
