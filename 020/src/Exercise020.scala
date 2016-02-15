import scala.annotation.tailrec

// Advanced Programming, Exercises by A. WÄ…sowski, IT University of Copenhagen
//
// AUTHOR1: sdeh@itu.dk
// AUTHOR2: ahaq@itu.dk
//
// Write names and ITU email addresses of both group members that contributed to
// the solution of the exercise (in alphabetical order by family name).
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file is meant to be compiled as follows:
//
// scalac Exercises.scala
//
// or
//
// fsc Exercises.scala
//
// To run the compiled file do "scala Exercises"
//
// To load the file int the REPL start the 'scala' interpreter and issue the
// command ':load Exercises.scala'. Now you can interactively experiment with
// your code.
//
// Continue solving exercises in the order presented in the PDF file. Large
// parts of the file are commented in order to make sure that the exercise
// compiles.  Uncomment and complete fragments as you proceed.  The file shall
// always compile and run after you are done with each exercise (if you do them
// in order).  Please compile and test frequently.

//Exercise 1: The answer is case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
// because x (which does not have anything to do with the "original" x, but is a "wildcard"
// y is another "wildcard", and x,y,3,4,_ matches the list. And since the case is before _, the _ does NOT win.

//For testing
object Test extends App {
  //Exercise 1
  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h
    case _ => 101
  }

  println(x)

  //Exercise 6
  println(List.init(List(1,2,3,4)))

  //Exercise 8
  println(List.length(List(1,2,3,4)))

  //Exercise 9
  println(List.foldLeft(List(1,2,3,4),0)((item,acc) => acc+1))

  //Exercise 11
  println(List(1,2,3,4))
  println(List.reverse(List(1,2,3,4)))

  //Exercise 12
  println(List.foldRight1(List(1.0,2.0,3.0,4.0),1.0)((item, acc) => item/acc))
  println(List.foldRight(List(1.0,2.0,3.0,4.0),1.0)((item, acc) => item/acc))

  //println(List.foldLeft1(List(1.0,2.0,3.0,4.0),1.0)((item, acc) => item/acc))
  //println(List.foldLeft(List(1.0,2.0,3.0,4.0),1.0)((item, acc) => item/acc))

  //Exercise 13
  println(List.concat(List(List(1,2),List(3,4), List(4,5))))

  //Exercise 14
  println(List.map(List(1,2,3,4))(x => x*2.0))

  //Exercise 16
  println(List.filter(List(1,2,3,4,3,2,1,1,2,3,4)) (x => x>2))

  //Exercise 17
  println(List.flatMap(List(1,2,3,4,5)) (x => List(x,x,x)))

  //Exercise 19
  println(List.add(List(1,2,3)) (List(4,5,6,7)))

  //Exercise 21
  println(List.hasSubsequence(List(1,2,3,4,5,6,7,8,9,10), List(1,2,3)))
  println(List.hasSubsequence(List(1,2,5,5,5,5,7,8,9,10), List(5,5,7)))
  println(List.hasSubsequence(List(1,2,3,4,5,6,7,8,9,10), List(1,3)))
  println(List.hasSubsequence(List(1,2,3,4,5,6,7,8,9,10), List(5,6,7)))
  println(List.hasSubsequence(List(1,2,3,4,5,6,7,8,9,10), List(8,9,10)))
  println(List.hasSubsequence(List(1,2,3,4,5,6,7,8,9,10), Nil))

  //Exercise 23
  println(List.pascal(1))
  println(List.pascal(2))
  println(List.pascal(3))
  println(List.pascal(4))
  println(List.pascal(5))
}


// An ADT of Lists

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {

  // override function application to provide a factory of lists (convenience)

  def apply[A](as: A*): List[A] = // Variadic function
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Exercise 2

  def tail[A] (as: List[A]) :List[A] = as match {
    case Cons(x,xs) => xs
    case _ => Nil
  }

  // Exercise 3

  def setHead[A] (as: List[A], newHead: A) : List[A] = as match {
    case Cons(x,xs) => Cons(newHead,xs)
    case _ => Nil
  }

  // Exercise 4

  def drop[A] (l: List[A], n: Int) : List[A] = l match {
    case Cons(x,xs) => {
      if (n > 0)
        drop(xs,n-1)
      else
        xs
    }
    case _ => {
      if (n > 0)
        Nil
      else
        l
    }
  }

  // Exercise 5

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x,xs) => {
      if (f(x))
        dropWhile(xs,f)
      else
        Cons(x,dropWhile(xs,f))
    }
    case _ => l
  }

  // Exercise 6
  //Linear time linear space
  def init[A](l: List[A]): List[A] = l match {
    case Cons(x,Nil) => Nil
    case Cons(x,xs) => Cons(x,init(xs))
    case _ => l
  }

  // Exercise 7 is in the bottom of the file

  // Exercise 8

  def foldRight[A,B] (as :List[A], z: B) (f : (A,B)=> B) :B = as match {
    case Nil => z
    case Cons (x,xs) => f (x, foldRight (xs,z) (f))
  }

  def length[A] (as: List[A]): Int = {
    foldRight(as,0)((item,acc) => acc+1)
  }

  // Exercise 9 NOTE: We fixed f: B, A => B to A, B => B
  @tailrec
  def foldLeft[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = as match {
    case Cons(x,xs) => foldLeft(xs, f(x, z)) (f)
    case _ => z
  }

  // Exercise 10

  def sum (as : List[Int]) : Int = foldLeft(as, 0) ((item, acc) => item + acc)
  def product (as :List[Int]) : Int = foldLeft(as, 1) ((item, acc) => item * acc)
  def length1 (as :List[Int]) : Int = foldLeft(as,0)((item,acc) => acc+1)

  // Exercise 11

  def reverse[A] (as :List[A]) :List[A] = as match {
    case Cons(x,xs) => foldLeft(xs,List(x))((item, newList) => Cons(item, newList))
    case _ => as
  }

  // Exercise 12
  def foldRight1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = foldLeft(reverse(as),z) (f)

  //NOTE: We fixed f: B, A => B to A, B => B
  def foldLeft1[A,B] (as: List[A], z: B) (f: (A, B) => B) : B = {
   throw new NotImplementedError("TODO ")  //TODO NOT WORKING ATM 12
  }

  // Exercise 13

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h,t) => Cons(h, append(t, a2))
  }

  def concat[A] (as: List[List[A]]) :List[A] = as match {
    case Cons(x, Nil) => x
    case Cons(x, xs) => append(x,concat(xs))
    case _ => Nil
  }

  // Exercise 14
  // The implementation in the lecture is using foldRight, foldRight is not tail recursive
  def map[A,B] (a :List[A]) (f :A => B) :List[B] = {
    def helpMap (in: List[A]) :List[B] = in match {
        case Cons(x,Nil) => List(f(x))
        case Cons(x,xs) => foldLeft(xs,List(f(x)))((item, acc) => Cons(f(item), acc))
        case _ => Nil
      }

    helpMap(List.reverse(a))
  }

  // Exercise 15 (no coding)
  // Because the results would be "reversed" if we had used foldLeft without reversing first.

  // Exercise 16

  def filter[A] (as: List[A]) (f: A => Boolean) : List[A] = as match {
    case Cons(x,Nil) => if (f(x)) List(x) else Nil
    case Cons(x,xs) => if (f(x)) Cons(x,filter(xs) (f)) else filter(xs) (f)
    case _ => Nil
  }

  // Exercise 17

  def flatMap[A,B](as: List[A])(f: A => List[B]) : List[B] = as match {
    case Cons(x,Nil) => f(x)
    case Cons(x,xs) => List.concat(List(f(x),flatMap(xs)(f)))
    case _ => Nil
  }

  // Exercise 18

  //TODO 18 def filter1[A] (l: List[A]) (p: A => Boolean) :List[A] =

  // Exercise 19
  def add (l: List[Int]) (r: List[Int]): List[Int] = l match {
    case Cons(l, ls) => r match {
      case Cons(r, rs) => Cons(l+r, add(ls) (rs))
      case _ => Nil
    }
    case _ => Nil
  }

  // Exercise 20

  def zipWith[A,B,C] (f : (A,B)=>C) (l: List[A], r: List[B]) : List[C] = l match {
    case Cons(l, ls) => r match {
      case Cons(r, rs) => Cons(f(l,r), zipWith(f) (ls,rs))
      case _ => Nil
    }
    case _ => Nil
  }

  // Exercise 21

  def hasSubsequence[A] (sup: List[A], sub: List[A]) :Boolean = {

    def hasSubHelper [A] (supH: List[A], subH: List[A],  startSub: List[A]) :Boolean = supH match {
      case Cons(x,xs) => subH match {
        case Cons(subx, subxs) => {
          if (subx == x)
            hasSubHelper(xs, subxs, startSub)
          else
            hasSubHelper(xs, startSub, startSub)
        }
        case _ => true
      }
      case _ => if (subH == Nil) true else false
    }

    if (hasSubHelper(sup, sub, sub))
      true
    else sup match {
      case Cons(x,xs) => hasSubsequence(xs, sub)
      case _ => false
    }
  }

  // Exercise 22
  //TODO PASCAL NOT WORKING
  def pascal (n :Int) : List[Int] = {
    def pascHelp(in: List[Int]): List[Int] = in match {
      case Cons(x1,Cons(x2,xs)) => Cons(x1+x2, pascHelp(Cons(x2,xs)))
      case Cons(x, xs) => List(x)
      case _ => Nil
    }

    def pascHelpPt2 (cur: Int, max: Int, lastList: List[Int]): List[Int] = {
      if (cur < max)
        pascHelpPt2(cur + 1, max, pascHelp(lastList))
      else
        lastList
    }

    pascHelpPt2(1, n, List(1))
  }

  // a test: pascal (4) = Cons(1,Cons(3,Cons(3,Cons(1,Nil))))

}




// Exercise 7

object Exercise7 {

  case class SalaryLine(name: String, amount: Integer)

  def maximumSalary (salaries: List[SalaryLine]) :Integer = salaries match {
    case Cons(x, Nil) => x.amount
    case Cons(x, xs) => if (x.amount > maximumSalary(xs)) x.amount
                        else maximumSalary(List.drop(salaries, 0))
    case Nil => -1
    }

  val test_case = List( SalaryLine("John",41),
                        SalaryLine("Alice", 42),
                        SalaryLine("Bob",40))

  val test_case1 = List(SalaryLine("John",1),
                        SalaryLine("Alice", 2),
                        SalaryLine("Bob",3))

  val test_case2 = List(SalaryLine("John",3),
                        SalaryLine("Alice", 2),
                        SalaryLine("Bob",1))

  val test_case3 = List(SalaryLine("John",1))

  val test_case4 = List(SalaryLine("",0))
}

