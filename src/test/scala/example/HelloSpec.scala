package example

import cats.kernel.{Eq, Monoid, Semigroup, Semilattice}
import cats.kernel.laws.discipline._
import org.scalatest.funspec.{AnyFunSpec, AsyncFunSpec}
import cats.laws
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSpecDiscipline

import java.util.UUID

/** Must be:
  *   - A commutative Semigroup with an idempotent combine
  */
final case class MySemiLattice(seenIds: Set[UUID])

object MySemiLattice {
  implicit val eqInstance: Eq[MySemiLattice] = new Eq[MySemiLattice] {
    override def eqv(x: MySemiLattice, y: MySemiLattice): Boolean = x.seenIds == y.seenIds
  }

  implicit val semigroupInstance: Semigroup[MySemiLattice] = new Semigroup[MySemiLattice] {
    override def combine(x: MySemiLattice, y: MySemiLattice): MySemiLattice =
      MySemiLattice(x.seenIds.union(y.seenIds))
  }

  implicit val semilatticeInstance: Semilattice[MySemiLattice] = new Semilattice[MySemiLattice] {
    override def combine(x: MySemiLattice, y: MySemiLattice): MySemiLattice =
      semigroupInstance.combine(x, y)
  }

  implicit val monoidInstance: Monoid[MySemiLattice] = new Monoid[MySemiLattice] {
    override def empty: MySemiLattice = MySemiLattice(Set.empty)

    override def combine(x: MySemiLattice, y: MySemiLattice): MySemiLattice =
      semigroupInstance.combine(x, y)
  }
}

class HelloSpec extends AnyFunSpec with FunSpecDiscipline with Configuration {

  val mySemiLaticeGenInstance: Gen[MySemiLattice] = for {
    i          <- Gen.long.map(_.toInt)
    seenBefore <- Gen.listOf(Gen.uuid).map(_.toSet)
  } yield {
      MySemiLattice(seenBefore)
  }

  implicit val mySemiLaticeArbitraryInstance: Arbitrary[MySemiLattice] = Arbitrary(
    mySemiLaticeGenInstance
  )

  val catsLawsRuleSetSemigroup   =
    SemigroupTests[MySemiLattice](MySemiLattice.semigroupInstance).semigroup
  val catsLawsRuleSetMonoid      = MonoidTests[MySemiLattice].monoid
  val catsLawsRuleSetSemiLattice = SemilatticeTests[MySemiLattice].semilattice

  // [info] - semilattice.intercalateRepeat2 *** FAILED ***
  // [info] GeneratorDrivenPropertyCheckFailedException was thrown during property evaluation.
  // [info] (HelloSpec.scala:73)
  // [info] Falsified after 0 successful property evaluations.
  // [info] Location: (HelloSpec.scala:73)
  // [info] Occurred when passed generated values (
  // [info] arg0 =  MySemiLattice(-21227448,Set()),
  // [info] arg1 = MySemiLattice(-2014200145,Set())
  // [info] )
  // [info] Label of failing property:
  // [info] Expected: MySemiLattice(245339558,Set())
  // [info] Received: MySemiLattice(-2035427593,Set())
  //

//  val a          = MySemiLattice(-21227448, Set.empty)
//  val b          = MySemiLattice(-2014200145, Set.empty)
//  val withMiddle = MySemiLattice.semilatticeInstance.intercalate(b)
//  val left       = withMiddle.combineN(a, 2)
//  val rght      = withMiddle.combine(a, a)
//  val real       = MySemiLattice(245339558, Set())
//  pprint.log(left)
//  pprint.log(rght)
//  pprint.log(real)

  checkAll(name = "semigroup", ruleSet = catsLawsRuleSetSemigroup)

  checkAll(name = "monoid", ruleSet = catsLawsRuleSetMonoid)

  checkAll(name = "semilattice", ruleSet = catsLawsRuleSetSemiLattice)

}
