/*
 * Copyright 2020 Daniel Spiewak
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ce3

import cats.implicits._

import playground._

import org.scalacheck.{Arbitrary, Cogen, Gen}, Arbitrary.arbitrary

object Generators {

  private[this] def F[E] = ConcurrentBracket[PureConc[E, ?], E]

  def genResource[F[_]: Bracket[*[_], E], E, A: Arbitrary: Cogen](
      implicit arbEffect: Arbitrary[F[Resource[F, A]]],
      arbFA: Arbitrary[F[A]],
      arbAFUnit: Arbitrary[A => F[Unit]]
  ): Gen[Resource[F, A]] = {
    val self = Gen.delay(genResource[F, E, A])
    Gen.frequency(
      1 -> genPureResource[F, E, A],
      1 -> genSuspendResource[F, E, A],
      1 -> genMakeResource[F, E, A],
      1 -> genFlatMapResource[F, E, A](
        self,
        Arbitrary.arbFunction1(Arbitrary(self), Cogen[A]).arbitrary
      )
    )
  }

  def genPureResource[F[_]: Bracket[*[_], E], E, A: Arbitrary]
      : Gen[Resource[F, A]] =
    Arbitrary.arbitrary[A].map(Resource.pure[F, E, A](_))

  def genSuspendResource[F[_]: Bracket[*[_], E], E, A](
      implicit arbEffect: Arbitrary[F[Resource[F, A]]]
  ): Gen[Resource[F, A]] =
    arbEffect.arbitrary.map(Resource.suspend)

  def genMakeResource[F[_]: Bracket[*[_], E], E, A](
      implicit arbFA: Arbitrary[F[A]],
      arbAFUnit: Arbitrary[A => F[Unit]]
  ): Gen[Resource[F, A]] =
    for {
      allocate <- arbFA.arbitrary
      release <- arbAFUnit.arbitrary
    } yield Resource.make(allocate)(release)

  def genFlatMapResource[F[_]: Bracket[*[_], E], E, A](
      baseGen: Gen[Resource[F, A]],
      genFunction: Gen[A => Resource[F, A]]
  ): Gen[Resource[F, A]] =
    for {
      base <- baseGen
      f <- genFunction
    } yield base.flatMap(f)

  def genPureConc[E: Arbitrary: Cogen, A: Arbitrary: Cogen](
      depth: Int
  ): Gen[PureConc[E, A]] = {
    if (depth > 10) {
      Gen.frequency(
        1 -> genPure[E, A],
        1 -> genRaiseError[E, A],
        1 -> genCanceled[E, A],
        1 -> genCede[E].flatMap(pc => arbitrary[A].map(pc.as(_))),
        1 -> Gen.delay(genNever)
      )
    } else {
      Gen.frequency(
        1 -> genPure[E, A],
        1 -> genRaiseError[E, A],
        1 -> genCanceled[E, A],
        1 -> genCede[E].flatMap(pc => arbitrary[A].map(pc.as(_))),
        1 -> Gen.delay(genBracketCase[E, A](depth)),
        1 -> Gen.delay(genUncancelable[E, A](depth)),
        1 -> Gen.delay(genHandleErrorWith[E, A](depth)),
        1 -> Gen.delay(genNever),
        1 -> Gen.delay(genRacePair[E, A](depth)),
        1 -> Gen.delay(genStart[E, A](depth)),
        1 -> Gen.delay(genJoin[E, A](depth)),
        1 -> Gen.delay(genFlatMap[E, A](depth))
      )
    }
  }

  def genPure[E, A: Arbitrary]: Gen[PureConc[E, A]] =
    arbitrary[A].map(_.pure[PureConc[E, ?]])

  def genRaiseError[E: Arbitrary, A]: Gen[PureConc[E, A]] =
    arbitrary[E].map(F[E].raiseError[A](_))

  def genHandleErrorWith[E: Arbitrary: Cogen, A: Arbitrary: Cogen](
      depth: Int
  ): Gen[PureConc[E, A]] = {
    implicit def arbPureConc[E2: Arbitrary: Cogen, A2: Arbitrary: Cogen]
        : Arbitrary[PureConc[E2, A2]] =
      Arbitrary(genPureConc[E2, A2](depth + 1))

    for {
      fa <- genPureConc[E, A](depth + 1)
      f <- arbitrary[E => PureConc[E, A]]
    } yield F[E].handleErrorWith(fa)(f)
  }

  def genBracketCase[E: Arbitrary: Cogen, A: Arbitrary: Cogen](
      depth: Int
  ): Gen[PureConc[E, A]] = {
    implicit def arbPureConc[E2: Arbitrary: Cogen, A2: Arbitrary: Cogen]
        : Arbitrary[PureConc[E2, A2]] =
      Arbitrary(genPureConc[E2, A2](depth + 1))

    for {
      acquire <- genPureConc[E, A](depth + 1)
      use <- arbitrary[A => PureConc[E, A]]
      release <- arbitrary[
        (A, Outcome[PureConc[E, ?], E, A]) => PureConc[E, Unit]
      ]
    } yield F[E].bracketCase(acquire)(use)(release)
  }

  // TODO we can't really use poll :-( since we can't Cogen FunctionK
  def genUncancelable[E: Arbitrary: Cogen, A: Arbitrary: Cogen](
      depth: Int
  ): Gen[PureConc[E, A]] =
    genPureConc[E, A](depth).map(pc => F[E].uncancelable(_ => pc))

  def genCanceled[E, A: Arbitrary]: Gen[PureConc[E, A]] =
    arbitrary[A].map(F[E].canceled.as(_))

  def genCede[E]: Gen[PureConc[E, Unit]] =
    F[E].cede

  def genNever[E, A]: Gen[PureConc[E, A]] =
    F[E].never[A]

  def genStart[E: Arbitrary: Cogen, A: Arbitrary](
      depth: Int
  ): Gen[PureConc[E, A]] =
    genPureConc[E, Unit](depth).flatMap(pc =>
      arbitrary[A].map(a => F[E].start(pc).as(a))
    )

  def genJoin[E: Arbitrary: Cogen, A: Arbitrary: Cogen](
      depth: Int
  ): Gen[PureConc[E, A]] =
    for {
      fiber <- genPureConc[E, A](depth)
      cont <- genPureConc[E, Unit](depth)
      a <- arbitrary[A]
    } yield F[E].start(fiber).flatMap(f => cont >> f.join).as(a)

  def genRacePair[E: Arbitrary: Cogen, A: Arbitrary: Cogen](
      depth: Int
  ): Gen[PureConc[E, A]] =
    for {
      fa <- genPureConc[E, A](depth + 1)
      fb <- genPureConc[E, A](depth + 1)

      cancel <- arbitrary[Boolean]

      back = F[E].racePair(fa, fb) flatMap {
        case Left((a, f)) =>
          if (cancel)
            f.cancel.as(a)
          else
            f.join.as(a)

        case Right((f, a)) =>
          if (cancel)
            f.cancel.as(a)
          else
            f.join.as(a)
      }
    } yield back

  def genFlatMap[E: Arbitrary: Cogen, A: Arbitrary: Cogen](
      depth: Int
  ): Gen[PureConc[E, A]] = {
    implicit val arbPureConc: Arbitrary[PureConc[E, A]] =
      Arbitrary(genPureConc[E, A](depth + 1))

    for {
      pc <- genPureConc[E, A](depth + 1)
      f <- arbitrary[A => PureConc[E, A]]
    } yield pc.flatMap(f)
  }

  implicit def cogenPureConc[E: Cogen, A: Cogen]: Cogen[PureConc[E, A]] =
    Cogen[Outcome[Option, E, A]].contramap(run(_))

  implicit def arbExitCase[F[_], E: Arbitrary, A](
      implicit A: Arbitrary[F[A]]
  ): Arbitrary[Outcome[F, E, A]] =
    Arbitrary(genExitCase[F, E, A])

  def genExitCase[F[_], E: Arbitrary, A](
      implicit A: Arbitrary[F[A]]
  ): Gen[Outcome[F, E, A]] =
    Gen.oneOf(
      Gen.const(Outcome.Canceled),
      Arbitrary.arbitrary[E].map(Outcome.Errored(_)),
      Arbitrary.arbitrary[F[A]].map(Outcome.Completed(_))
    )

  implicit def cogenExitCase[F[_], E: Cogen, A](
      implicit A: Cogen[F[A]]
  ): Cogen[Outcome[F, E, A]] = Cogen[Option[Either[E, F[A]]]].contramap {
    case Outcome.Canceled      => None
    case Outcome.Completed(fa) => Some(Right(fa))
    case Outcome.Errored(e)    => Some(Left(e))
  }
}
