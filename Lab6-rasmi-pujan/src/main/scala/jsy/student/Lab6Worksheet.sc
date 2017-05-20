import jsy.lab6.Lab6Like
import jsy.lab6.ast._
import jsy.util.DoWith
import jsy.util.DoWith.{doget, domodify, doreturn}

val l = List(1,2,3)
val y = List(1,2,0)


def summ(l: List[Int]): Int = {
  def sum(sc: Int => Int, l: List[Int]): Int = l match {
    case Nil => sc(0)
    case h :: t if (h<0) => 0
    case h :: t => sum(acc => sc(h + acc), t)
  }
  sum(acc => acc, l)

}
summ(l)

summ(y)

