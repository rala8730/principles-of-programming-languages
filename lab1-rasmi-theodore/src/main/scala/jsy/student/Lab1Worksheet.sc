import com.sun.tools.corba.se.idl.constExpr.Xor
import jsy.student.Lab1._
import jsy.lab1.Lab1Like
/*
 * CSCI 3155: Lab 1 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab1.scala.
 */

/*
 * Example: Test-driven development of plus
 */

//binary search tree
def factorialBeta (n: Int): Int = {
  assert( n >= 0 )
  n match {
    case 0 => 1
    case _ => factorialBeta(n - 1) * n
  }
}
def factorialAlpha (n: Int): Int = {
  assert( n >= 0 )
  n match {
    case _ => factorialAlpha(n - 1) * n
    case 0 => 1
  }
}

sealed abstract class SearchTree
case object Empty extends SearchTree
case class Node(l:SearchTree,
                d:Int,
                r:SearchTree)
  extends SearchTree

def sum(t:SearchTree):Int=t match {
  case Empty=> 0
  case Node(l,d,r) =>d+ sum(l) + sum(r)
}


 // Binary(Plus, Unary(Neg, N(5.0))






// Here we can write expressions to experiment with how we might implement
// something. The expression is evaluated interactively.
/*val pi = 3.14
def circumference(r: Double): Double = {
  val pi = 3.14159
    println(pi)
 20 * pi * r
}
def area(r: Double): Double = {
  pi * r * r

}
circumference(1)
area(1)

*/
def g(x: Int) = {
  val (a, b) = (1, (x, 3))
  if (x == 0) (b, 1) else (b, a + 2)
}
def g_one(x:Int) = {
  val a=1
  val b=(x,3)
 if (x==0) (b,1)
 else (b,a+2)
}



/*
val x = 4
def f(x: Int): Int = x match {
    case 0 => 0
    case x => {
      val y = x + 1
      ({
        val x = y + 1
     } * f(x - 1))11

    }
   }
 val y = x + f(x)

f(x)
*/

