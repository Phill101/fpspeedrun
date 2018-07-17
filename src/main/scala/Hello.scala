package fpspeedrun

import scala.io.StdIn
import cats.{Eval, Foldable, Id}
import cats.syntax.semigroup._
import cats.syntax.foldable._
//import cats.instances.unit._
//import cats.instances.list._
//import cats.instances.string._
//import cats.instances.int._
import cats.instances.all._

object Hello extends App {
//  plusOneX2(Eval.always(StdIn.readLine("Input number:\n"))).value

  def plusOne(input: Eval[String]): Eval[Unit] = {
    for {
      x <- input
      _ <- Eval.always(println(s"Your input $x + 1 = ${x.toInt + 1}"))
    } yield ()
  }

  def plusOneX2(input: Eval[String]): Eval[Unit] = plusOne(input).combineN(2)


  def sumFirstN(iter: BigInt => BigInt, init: BigInt, count: Int): Eval[BigInt] = {
//    (0 until count).foldLeft((init, 0: BigInt)){ case ((v, r), _) => (iter(v), r+v) }._2
    if (count == 0) Eval.now(0)
    else for(v <- Eval.defer(sumFirstN(iter, iter(init), count-1))) yield init + v
  }

//  val r = sumFirstN(_+1, 1, 100000).value
//  println(r)


  val l = "1" :: "2" :: "3" :: Nil
//  l.foreach(x => plusOne(Eval.now(x)).value)
//  l.foldMap(x => plusOne(Eval.now(x))).value

  println(l.foldMap(identity))
  println(l.foldMap(_.toInt))


  def concat[F[_]: Foldable](x: F[String]): String = x.foldLeft(""){_ + _ + "!"}

//  println(concat(l))
//  println(concat(l.toVector))
//  println(concat(l.toStream))
//  println(concat(Option("1")))
//  println(concat((1, "5")))
//  println(concat[Id]("one"))

  val s = Stream.from(1).map(BigInt(_))
  println(s.take(100).sum)

  def sumN[F[_] : Foldable](xs: F[BigInt], n: BigInt): BigInt =
    xs.foldRight[BigInt => Eval[BigInt]](Eval.now(_ => Eval.now(0))) { (x, acc) =>
      Eval.later{k => if (k == 0) Eval.now(0) else
        for {
          f <- acc
          s <- f(k - 1)
        } yield s + x
      }
    }.flatMap(_(n)).value

  println(sumN(s, 100000))

//  def sumWhile[F[_] : Foldable](xs: F[Int], pr: Int => Boolean) =
//    xs.foldRight(Eval.now(0)) { case (x, acc) =>
//      if (pr(x)) for (sum <- acc) yield sum + x
//      else Eval.now(0)
//    }.value
//  println(sumWhile(s, _ <= 100000))


  // todo реализовать функцию foldLeft через foldRight
  // todo простой foldRight(без eval) который реализован через foldMap
}
