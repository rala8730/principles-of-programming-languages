package jsy.util

/*
 * DoWith is a data structure that holds a function that returns a result of
 * type R with a input-output state of type W.
 *
 * Aside: This is also known as the State monad.
 */
/*introduce the idea of encapsulating computation with the DoWith[W,R] type
* it encapsulates a function of type W=>(W,R),
* which is a computation that returns a value of type R with an input-output state of type W.
* The doer field holds precisely a function of the type W=>(W,R)*/
//doer is the initializer
//R is the subclass of +R
sealed class DoWith[W,+R] private (doer: W => (W,R)) {
  def apply(w: W) = doer(w)

  def map[B](f: R => B): DoWith[W,B] = new DoWith[W,B]({
    (w: W) => {
      val (wp, r) = doer(w)
      (wp, f(r))
    }
  })

  def flatMap[B](f: R => DoWith[W,B]): DoWith[W,B] = new DoWith[W,B]({
    (w: W) => {
      val (wp, r) = doer(w)
      f(r)(wp) // same as f(a).apply(s)
    }
  })
}

object DoWith {
  def doget[W]: DoWith[W, W] = new DoWith[W, W]({ w => (w, w) })
  def doput[W](w: W): DoWith[W, Unit] = new DoWith[W, Unit]({ _ => (w, ()) })
  def doreturn[W, R](r: R): DoWith[W, R] = new DoWith[W, R]({ w => (w, r) })  // doget map { _ => r }
  def domodify[W](f: W => W): DoWith[W, Unit] = new DoWith[W, Unit]({ w => (f(w), ()) })  // doget flatMap { w => doput(f(w)) }
}

