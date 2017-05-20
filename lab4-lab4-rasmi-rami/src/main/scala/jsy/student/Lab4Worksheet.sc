/*
 * CSCI 3155: Lab 4 Worksheet
 *
 * This worksheet demonstrates how you could experiment
 * interactively with your implementations in Lab4.scala.
 */

// Imports the parse function from jsy.lab1.Parser
import jsy.lab4.Parser.parse

// Imports the ast nodes
import jsy.lab4.ast._

// Imports all of the functions form jsy.student.Lab2 (your implementations in Lab2.scala)
import jsy.student.Lab4._

// Try compressRec
//val cr1 = compressRec(List(1, 2, 2, 3, 3, 3))
iterateStep("const f=function(x:number){return x}")
// Parse functions with possibly multiple parameters and type annotations.
parse("function fst(x: number, y: number): number { return x }")
parse("function (x: number) { return x }")
parse("function (f: (y: number) => number, x: number) { return f(x) }")

// Parse objects
parse("{ f: 0, g: true }")
parse("x.f")

/*sealed abstract class Lst
case class Nl() extends Lst
case class Cons(hd:String, tl: Lst) extends Lst

val listExample:Lst =Cons("What", Cons("Now",Nl()))

def reverseLst(x:Lst):Lst =x match {
  case Nl()=>Nl()
  case Cons(h,t)=>Cons(h, reverseLst(t))
}

reverseLst(listExample)

def reverseLst_one(x:Lst,acc:Lst):Lst =x match {
  case Nl()=>acc
  case Cons(h,t)=>reverseLst_one(t, Cons(h, acc))
}
reverseLst_one(listExample, Nl())

val a = 1 :: 3 :: 5 :: 7 :: 11 :: Nil
val b:List[Int] = 1 :: (3::(5::(7::(11:: Nil))))

def reverseLst_two[T](x:List[T],acc:List[T]):List[T] =x match {
  case Nil=> acc
  case h :: t =>reverseLst_two(t, h::acc)
}
reverseLst_two(b,Nil)


def reverseLst_three[T](x:List[T]):List[T]=reverser (x,Nil)

def reverser[T](x:List[T],acc:List[T]):List[T] =x match {
  case Nil=> acc
 }
reverseLst_three(b)

val c:List[Int]=1 :: (3 :: (5 ::(7 :: (11 :: Nil))))
def  foldleft[A,B](x:List[A],f:(A,B)=>B,acc:B):B = x match {
  case Nil =>acc
  case h :: t => foldleft(t,f,f(h,acc))
}

foldleft(b,(h:Int, acc:List[Int]) => h :: acc, Nil)
def  foldleft_one[A,B](f:(A,B)=>B,acc:B): List[A] => B =(x:List[A]) => x match {
  case Nil => acc
  case h :: t => foldleft_one(f,f(h,acc))(t)
}
val rev = foldleft_one((h:Int, acc:List[Int]) => h :: acc, Nil)
rev(b)
val rev_one = foldleft_one((h:Int, acc:List[Int]) => h :: acc, Nil)(b)

def ComposeFun[A,B,C](f:A=>B,g:B=>C):A=>C= {
  (input:A)=>g(f(input))
}
//val f=(x:string)
 def len[A](l:List[A]):Int= l match{
   case Nil =>
   case _ :: t =>
 }

*/
parse("{f:2+3,g:true}")
parse("x.f")
parse("function g(x:number){return {f:x}}; g(2).f")
parse("function (x:number){return {f:x}}; g(2).f")
