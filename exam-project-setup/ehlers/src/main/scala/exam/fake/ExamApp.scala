package exam.fake

import adpro.monoids.Monoid
import scala.collection.mutable.ListBuffer

object ExamApp extends App {

  //Q1
  println("\nQ1-1")
  val list = List(5,3,2,1,2,3,5)
  val listBuff = ListBuffer[Int] (5,3,2,1,2,3,5)
  println(Q1.listDifferentialImp(listBuff))
  println(Q1.listDifferentialFun(list))
  println(Q1.listDifferentialFunFold(list))

  println("\nQ1-2")
  val list2 = Nil:List[Int]
  val listBuff2 = ListBuffer[Int] ()
  println(Q1.listDifferentialImp(listBuff2))
  println(Q1.listDifferentialFun(list2))
  println(Q1.listDifferentialFunFold(list2))

  //Q2
  println("\nQ2")
  println(( (Q2.onList (x => x+x+x)) ("ABC".toList) ).mkString)

  //Q3
  println("\nQ3")
  println(Q3.foldBack(List("x1","x2","x3")) (Monoid.stringMonoidZeroZParenthesis))

  //Q4
  //Nothing to test

  //Q5
  def branch:Q5.Tree[Int] = Q5.branch(branch, branch)


}
