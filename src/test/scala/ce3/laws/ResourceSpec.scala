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
      val testz: RegionTests[Resource, PureConc[Int, *], Int] = RegionTests[
        Resource,
        PureConc[Int, *],
        Outcome[PureConc[Int, *], Int, *],
        Int
      ]

      val lawz: testz.laws.type =
        testz.laws

      implicit val cogenLawsCase: Cogen[lawz.F.Case[_]] =
        cogenExitCase[PureConc[Int, *], Int, Unit]
          .asInstanceOf[Cogen[lawz.F.Case[_]]] //just to get this to compile...

      testz.region[Int, Int, Int]
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

  implicit def arbResource[F[_], E, A]: Arbitrary[Resource[F, A]] = ???
  implicit def cogenResource[F[_], E, A]: Cogen[Resource[F, A]] = ???

  implicit def eqResource[F[_]: Bracket[*[_], E], E, A](
      implicit eqFUnit: Eq[F[Unit]]
  ): Eq[Resource[F, A]] =
    Eq.by(_.used) //todo pass real function?

}
