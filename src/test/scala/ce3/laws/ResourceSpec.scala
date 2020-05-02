package ce3
package laws

import cats.{
  ~>,
  Applicative,
  ApplicativeError,
  Eq,
  FlatMap,
  Monad,
  Monoid,
  Show
}
import cats.data.{EitherK, StateT}
import cats.free.FreeT
import cats.implicits._

import coop.ThreadT

import org.scalacheck.{Arbitrary, Cogen, Gen}, Arbitrary.arbitrary
import org.scalacheck.util.Pretty

import org.specs2.ScalaCheck
import org.specs2.matcher.Matcher
import org.specs2.mutable._

import playground._
import org.typelevel.discipline.specs2.mutable.Discipline

class ResourceSpec extends Specification with Discipline with ScalaCheck {
  import Generators._

  checkAll(
    "Resource", {

      implicit def cogenLawsCase[F[_]: Applicative, E](
          implicit base: Cogen[
            Outcome[F, E, Unit]
          ] //not too sure about this one
      ): Cogen[Outcome[F, E, _]] =
        base.contramap(_.void)

      RegionTests[
        Resource,
        PureConc[Int, *],
        Outcome[PureConc[Int, *], Int, *],
        Int
      ].region[Int, Int, Int]
    }
  )

  implicit def arbPureConc[E: Arbitrary: Cogen, A: Arbitrary: Cogen]
      : Arbitrary[PureConc[E, A]] =
    Arbitrary(genPureConc[E, A](0))

  implicit def prettyFromShow[A: Show](a: A): Pretty =
    Pretty.prettyString(a.show)

  def beEqv[A: Eq: Show](expect: A): Matcher[A] = be_===[A](expect)

  def be_===[A: Eq: Show](expect: A): Matcher[A] =
    (result: A) =>
      (
        result === expect,
        s"${result.show} === ${expect.show}",
        s"${result.show} !== ${expect.show}"
      )

  implicit def arbitraryPureConcResource[
      E: Arbitrary: Cogen,
      A: Arbitrary: Cogen
  ]: Arbitrary[PureConc[E, Resource[PureConc[E, *], A]]] =
    Arbitrary(
      Gen.delay(
        arbPureConc[E, Resource[PureConc[E, *], A]].arbitrary
      )
    )

  implicit def arbResource[F[_]: Bracket[*[_], E], E, A: Arbitrary: Cogen](
      implicit arbEffect: Arbitrary[F[Resource[F, A]]],
      arbFA: Arbitrary[F[A]],
      arbAFUnit: Arbitrary[A => F[Unit]]
  ): Arbitrary[Resource[F, A]] = Arbitrary(genResource[F, E, A])

  implicit def cogenResource[F[_]: Bracket[*[_], E], E, A](
      implicit cogenF: Cogen[F[Unit]]
  ): Cogen[Resource[F, A]] = cogenF.contramap(_.used)

  implicit def eqResource[F[_]: Bracket[*[_], E], E, A](
      implicit
      eqFUnit: Eq[F[Unit]]
      /*  exhaustiveUse: ExhaustiveCheck[A => F[B]], */
      // eqFListB: Eq[F[List[B]]]
  ): Eq[Resource[F, A]] =
    Eq.by(
      _.used
    ) // { resource => exhaustiveUse.allValues.traverse(resource.use(_)) }

}

object Demo extends App {

  def prog[F[_]: ConcurrentBracket[*[_], E], E, A, B](
      a: A,
      b: B,
      e: E
  ): F[B] = {
    val F = ConcurrentBracket[F, E]

    val R = implicitly[Region[Resource, F, E]]

    Resource
      .make(F.pure(b))(b => F.raiseError[Unit](e))
      .use(a => F.pure(a))
  }

  // Completed(Some(hello))
  // │
  // ├ Notify 0x6404F418
  // ├ Notify 0x6404F418
  // ├ Notify 0x6404F418
  // ├ Notify 0x6404F418
  // ├ Notify 0x6404F418
  // ├ Notify 0x2794EAB6
  // ╰ Pure Completed(hello)
  //todo: shouldn't this raise an exception in the background? How does PureConc handle failures in finalizers?
  println {
    prog[PureConc[Int, *], Int, Int, String](42, "hello", -5).show
  }
}
