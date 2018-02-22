package example

import cats.Functor

sealed trait Tree[+A]

final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

final case class Leaf[A](value: A) extends Tree[A]

object Tree {

  // TODO 04: implement Functor[Tree]
  implicit val treeFunctor = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] =
      fa match {
        case Branch(l, r) => Branch(map(l)(f), map(r)(f))
        case Leaf(a)      => Leaf(f(a))
      }
  }

}
