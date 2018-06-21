package fpspeedrun
import fpspeedrun.Iso.{Wrapper, WrapperCompanion}
import simulacrum.typeclass
import syntax.semigroup._

@typeclass
trait Monoid[A] extends Semigroup[A] with Default[A]{
  def empty: A
  override def default: A = empty
}

object Monoid extends StdMonoidInstances[Monoid] {
  implicit def optionMonoid[T: Semigroup]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def empty: Option[T] = None
    override def combine(xo: Option[T], yo: Option[T]): Option[T] =
      for (x <- xo; y <- yo) yield x |+| y
  }
}


final case class Endo[A](run: A => A) extends AnyVal

object Endo{
  implicit def endoMonoid[A]: Monoid[Endo[A]] = ???
}

final case class Sum[T](value: T) extends AnyVal with Wrapper[T]

object Sum extends WrapperCompanion[Sum] {
  implicit def sumMonoid[T: Num]: Monoid[Sum[T]] = new Monoid[Sum[T]] {
    override def empty: Sum[T] = Sum(Num[T].one)
    override def combine(x: Sum[T], y: Sum[T]): Sum[T] = Sum(Num[T].plus(x.value, y.value))
  }
}

final case class Prod[T](value: T) extends AnyVal with Wrapper[T]

object Prod extends WrapperCompanion[Prod] {
  implicit def prodMonoid[T: Num]: Monoid[Prod[T]] = new Monoid[Prod[T]] {
    override def empty: Prod[T] = Prod(Num[T].zero)
    override def combine(x: Prod[T], y: Prod[T]): Prod[T] = Prod(Num[T].times(x.value, y.value))
  }
}


trait StdMonoidInstances[TC[x] >: Monoid[x]] {
  final implicit val stringMonoid: TC[String] = new Monoid[String] {
    override def empty: String = ""
    override def combine(x: String, y: String): String = x + y
  }

  final implicit def listMonoid[A]: TC[List[A]] = new Monoid[List[A]] {
    override def empty: List[A] = List.empty
    override def combine(x: List[A], y: List[A]): List[A] = x ::: y
  }

  final implicit def vectorMonoid[A]: TC[Vector[A]] = ???
}
