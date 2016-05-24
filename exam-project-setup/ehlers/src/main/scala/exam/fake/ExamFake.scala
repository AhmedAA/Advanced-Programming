/**
 * Advanced Programming.  Fake exam 2016.  This set of exam questions have been
 * prepared as a demonstration before the first written exam has been used in
 * ADPRO.
 */


/**
 * The questions will be greaded  manually.
 *
 * I do not recommend solving questions in an IDE to the point when they compile
 * and work (at least not until you have drafted all the answers).
 *
 * We will  be permissive  on small  syntactic issues  (semicolons, punctuation,
 * small deviations in  function names, switching between  curried and uncurried
 * parameters, unless  the question is about  currying, etc).
 *
 * We  will also  not check  whether the  type inference  succeeded (if  a human
 * reader could infer types).
 *
 * If you insert your answers in a  separate document remember to write the task
 * number before each answer, and answer questions  in the same order as in this
 * document.
 *
 * You can use any functions from the course (textbook, exercises) in your
 * solutions.
 *
 * You can use the provided build.sbt file to compile the exam. The same file
 * will work for the exam (but you might need to include some fpinscala files in
 * src/ if you use other functions in your solutions).  In general compiling
 * during a 4 hour exam is not recommended, as dependency problems can take all
 * your time.  But you are welcomed to do it, if you already solved all the
 * questions without compiling.
 **/

package exam.fake

import adpro.monoids.Monoid
import org.scalacheck.Gen

object Q1 {

  import scala.collection.mutable.ListBuffer

  /**
   * Task 1.
   *
   * Translate the following scala function to a referentially
   * transparent version. You don't need to know what is a differential to
   * solve this task.
   */

  def listDifferentialImp (inList: ListBuffer[Int]) :ListBuffer[Int] = {

    var result = ListBuffer[Int]()
    if (inList.size > 1) {
      var prev = inList.head
      for (curr <- inList.tail) {
        result += curr - prev // '+=' adds the right hand side as the last element in the list buffer
        prev = curr
      }
    }
    return result
  }

  //Solved using helper
  def listDifferentialFun (inList :List[Int]) :List[Int] = {
    if (inList.size < 1)
      Nil
    else {
      def helper(first: Boolean, prev: Int, list: List[Int]): List[Int] = list match {
        case x :: xs => if (first) helper(false, x, xs) else x - prev :: helper(false, x, xs)
        case _ => Nil
      }

      helper(true, inList.head, inList)
    }
  }

  //Solved using foldLeft
  //Starts with empty list (Nil) and None (Option)
  //The first element will match None as the option and return Nil, Some(x) - Basically the head as prev
  //The next elements will return the accumulated list appended with the current element minus previous with Some(x)
  def listDifferentialFunFold (inList :List[Int]) :List[Int] = {
    inList.foldLeft(Nil:List[Int], None:Option[Int]) ((z, x) => z._2 match {
      case None => (Nil, Some(x))
      case Some(prev) => (z._1 :+ x-prev, Some(x))
    })._1
  }

}



object Q2 {

  /**
   * Task 2.
   *
   * Implement  function onList  that  converts any  function of  type
   * String=>String to a function  of type List[Char]=>List[Char] that
   * satisies the following property:
   *
   * For any String s, and any function f:String => String:
   *
   *  f(s) == ( (onList (f)) (s.toList) ).mkString
   *
   * where mkString is  a function that converts (implodes)  a list of
   * characters back to a String.
   */

  //Returns a function that takes a list of chars X and makes string applies f and returns to list (List[Char] => f(String) => List[Char])
  def onList (f: String => String): List[Char] => List[Char] = {
    x:List[Char] => f(x.mkString).toList //Basically mkString does the same as x.foldRight("")((cha,s) => cha+s)
  }

}



object Q3 {

  import adpro.monoids.Monoid
  import scala.language.higherKinds

  /**
   * Task 3.
   *
   * Implement a function foldBack that  folds an operator of a monoid
   * M, by traversing  through the list twice. If the  operator is "+"
   * and  the List  is  : List(x1,x2,x3),  then  your function  should
   * compute:
   *
   * (((((((z + x1) + x2) +x3) + x3) + x2) + x1) + z)
   *
   * where z = M.zero and + is M.op .
   */

  //The trick here is to identify the correct folds
  //The first fold is the l.foldLeft(M.zero)((acc, cur) => M.op(acc,cur)) which yields (((z + x1) + x2) +x3)
  //The second fold is the l.foldRight(FIRST_FOLD)((cur, acc) => M.op(acc, cur)) which yields (FIRST_FOLD + x3) + x2) + x1)
  //At last the second fold is op'ed with M.zero which yields SECOND_FOLD + z
  def foldBack[A] (l :List[A]) (implicit M :Monoid[A]) :A = {
    M.op(l.foldRight(l.foldLeft(M.zero)((acc, cur) => M.op(acc,cur))) ((cur, acc) => M.op(acc, cur)), M.zero)
  }

}



object Q4 {

  /**
   * Task 4.
   *
   * (One does not need to know probability theory to solve this task).
   *
   * Let  trait Event  be a  trait representing  random events  (as in
   * probability theory)  and P  be a probability  function, assigning
   * a  value  in  the  interval  [0;1] to  each  event  (an  instance
   * of  Event). Assume  the  declarations  below. The body  of  P  is
   * irrelevant.
   */

  trait Event
  trait Probability
  def P (e :Event) :Probability = ??? // assume that this is defined

  /**
   * The   function   conditionalP(E1,E2)    assigns   a   conditional
   * probability value  to a pair  of random  events E1 and  E2.  This
   * function  is sometimes  undefined.  Write  the type  signature of
   * conditionalP below.
   *
   * Note that  we are not asking  for a definition of  this function,
   * just for a type declaration.
   */

  //Option is because of the possibility of the function to be undefined

   def conditionalP (E1 :Event, E2 :Event): Option[Probability] = ??? // replace ..., leave ??? in place this time.

}



object Q5 {

  /**
   * Task 5.
   *
   * Consider a type of lazy binary trees:
   */

  trait Tree[+A]
  case class Branch[+A] (l:() => Tree[A], r:() => Tree[A]) extends Tree[A]
  case object Leaf extends Tree[Nothing]

  /**
   * Implement a  convenience constructor  'branch' that is  both lazy
   * but does not require using explicit delays like Branch.
   */

  def branch[A] (l : =>Tree[A], r: =>Tree[A]) :Tree[A] = {
    lazy val left = l
    lazy val right = r
    Branch(() => left, () => right)
  }

}



object Q6 {

  import monocle.Optional
  import monocle.Lens

  /**
   * Task 6.
   *
   * Formalize a lense leftFT, that allows accessing and replacing the
   * leftmost element of a deque stored in a finger tree.
   *
   * Recall the basic types from our implementation:
   */

  trait FingerTree[+A] {
    def addL[B >:A] (b: B) :FingerTree[B] = ??? // assume that this is implemented
    def addR[B >:A] (b: B) :FingerTree[B] = ??? // Implemented for task 7
  }
  case class Empty () extends FingerTree[Nothing]

  sealed trait ViewL[+A]
  case class NilTree () extends ViewR[Nothing] with ViewL[Nothing] // Implemented for task 7
  case class ConsL[A] (hd: A, tl: FingerTree[A]) extends ViewL[A]

  def viewL[A] (t: FingerTree[A]) :ViewL[A] = ??? // assume that this is defined

  /* Use the addL and viewL to create a lens that extracts and allows
   * to modify the left most element of a finger tree. Either use the Monocle
   * API or (if you are writing in free text) use the notation from the paper of
   * Foster et al.
   *
   * Include the type of the lens (partial/total), and the put and get function.
   */

  //This is the solution!!

  def getL[A] (t: FingerTree[A]) :Option[A] = viewL(t) match {
    case NilTree () => None
    case ConsL (h,tl) => Some (h)
  }

  def putL[A] (a: A) (t :FingerTree[A]) :FingerTree[A] = viewL(t) match {
    case NilTree () => Empty().addL[A] (a)
    case ConsL (h,tl) => tl.addL (a)
  }

  def leftFT[A] = Optional[FingerTree[A],A] (getL) (putL)

  /**
   *  Task 7.
   *
   *  Explain in English (or in Danish) which parts of your solution
   *  need to be updated (and how) in order to create a lense that provides the
   *  anologous functionality for the right end of the deque
   */

  //A definition for viewR needs to be defined with ConsR.
  //getR similar to getL but with ConsR. Same with putR.
  //Lastly rightFT would need to be created with simply the L replaced

  //Example implementation

  sealed trait ViewR[+A]
  case class ConsR[A] (hd: A, tr: FingerTree[A]) extends ViewR[A]

  def viewR[A] (t: FingerTree[A]) :ViewR[A] = ???

  def getR[A] (t: FingerTree[A]) :Option[A] = viewR(t) match {
    case NilTree () => None
    case ConsR (h,tl) => Some (h)
  }

  def putR[A] (a: A) (t :FingerTree[A]) :FingerTree[A] = viewR(t) match {
    case NilTree () => Empty().addR[A] (a)
    case ConsR (h,tl) => tl.addR (a)
  }

  def rightFT[A] = Optional[FingerTree[A],A] (getR) (putR)
}




object Question7 {

  /* Task 8.
   *
   * Consider the standard library function (from the List companion object).
   *
   * def fill[A](n: Int)(elem: =>A): List[A]
   *
   * What is the meaning of (=>) in the above signature, and why the designers
   * of Scala library have used this type operator there? Explain in English (or
   * Danish).
   */

  //The Symbol => A means that elem is the type of a function that takes no parameters and returns an element of type A
  //This is an Expression that calculates the element
  //I would assume this is done in order to allow the filling function to fit multiple purposes.


  import adpro.state.RNG
  import adpro.testing.{GenAdPro => Gen}

  /**
   * Task 9.
   *
   * Implement  a generator multiplesOf10 that generates
   * integer numbers that are divisible by 10.
   *
   * Assume an implementation of Gen[A] as in the text book.
   * Also assume existance of arbitraryInt (implemented)
   *
   * Provide an explicit type for multiplesOf10
   */

  val arbitraryInt :Gen[Int] = Gen(adpro.state.State(_.nextInt)) // assume that this exists.

  //Correct GEN version - The idea is that division down and up will round the integer to 10 dividable
  val multiplesOf10Gen:Gen[Int] = arbitraryInt.map(x => x / 10 * 10)

  //Incorrect Stream version
  val multiplesOf10:Long => Stream[Int] = (seed:Long) => arbitraryInt.toStream(seed).dropWhile(x => x%10 != 0)

  /**
   * Task 10.
   *
   * Implement a generator multipleOf10UpTo(m) that generaters integer
   * numbers divisible by 10, but smaller than m.
   *
   * Provide an explicit type for multiplesOf10UpTo
   */

  //Correct Gen version - Using modulo limits the value to max m
  def multiplesOf10UpTo(m:Int): Gen[Int] = multiplesOf10Gen.map(x => x % m)

  //Incorrect Stream version
  //Uses multiplesOf10 but dropsWhile x < m
  def multiplesOf10UpTo(seed:Long, m:Int): Stream[Int] = multiplesOf10(seed).dropWhile(x => x < m)

}

object Question8 {

  import adpro.state.RNG
  import adpro.testing.{GenAdPro => Gen}

  val arbitraryInt :Gen[Int] = ??? // assume that this exists.
  def listOfN[A] (n: Int, g: Gen[A]) : Gen[List[A]] = ??? // assume that this exists.

  /**
   * Task 11.
   *
   * Below you will find two expressions that (apparently) generate randomly
   * sized integer lists.
   *
   * Write explicit types for v2 and v1.
   *
   * Explain in English (or Danish) what are the types of values v1 and v2 and
   * explain the difference between the computations that produce them.  The
   * explanation should not be long (4-5 lines will suffice).
   **/

  val v1:Gen[List[Int]] = arbitraryInt.flatMap (n => listOfN(n, arbitraryInt))
  val v2:(List[Int],RNG) = arbitraryInt.flatMap (n => listOfN(n, arbitraryInt)).
           sample.run (RNG.Simple(42))

  //V1 Defines a generator of type List[Int] where the generation have not been executed
  //V2 Defines a tuple of List[Int] and RNG, that is the actual generation to be executed in V1. V2 runs the generator
  //and thus calculates the List of integers. The calculation in v2 is based on RNG.Simple(42), V1 is not calculated and
  //can therefore be based on whatever the coder decides later.

}
