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
final case class MySemiLattice(i: Int, seenIds: Set[UUID])

object MySemiLattice {
  implicit val eqInstance: Eq[MySemiLattice] = new Eq[MySemiLattice] {
    override def eqv(x: MySemiLattice, y: MySemiLattice): Boolean = x.i == y.i
  }

  implicit val semigroupInstance: Semigroup[MySemiLattice] = new Semigroup[MySemiLattice] {
    override def combine(x: MySemiLattice, y: MySemiLattice): MySemiLattice =
      if (x.seenIds.isEmpty && y.seenIds.isEmpty && x.i != y.i)
        MySemiLattice(x.i + y.i, Set.empty)
      else if (x.seenIds.diff(y.seenIds).isEmpty && x.i == y.i)
        x
      else
        MySemiLattice(x.i + y.i, y.seenIds.union(y.seenIds))
  }

  implicit val semilatticeInstance: Semilattice[MySemiLattice] = new Semilattice[MySemiLattice] {
    override def combine(x: MySemiLattice, y: MySemiLattice): MySemiLattice =
      semigroupInstance.combine(x, y)
  }

  implicit val monoidInstance: Monoid[MySemiLattice] = new Monoid[MySemiLattice] {
    override def empty: MySemiLattice = MySemiLattice(0, Set.empty)

    override def combine(x: MySemiLattice, y: MySemiLattice): MySemiLattice =
      semigroupInstance.combine(x, y)
  }
}

class HelloSpec extends AnyFunSpec with FunSpecDiscipline with Configuration {
//  import org.scalacheck.ScalacheckShapeless.derivedArbitrary

  val mySemiLaticeGenInstance: Gen[MySemiLattice] = for {
    i          <- Gen.long.map(_.toInt)
    seenBefore <- Gen.listOf(Gen.uuid).map(_.toSet)
  } yield MySemiLattice(i, seenBefore)

  implicit val mySemiLaticeArbitraryInstance: Arbitrary[MySemiLattice] = Arbitrary(
    mySemiLaticeGenInstance
  )

  val catsLawsRuleSetSemigroup   =
    SemigroupTests[MySemiLattice](MySemiLattice.semigroupInstance).semigroup
  val catsLawsRuleSetMonoid      = MonoidTests[MySemiLattice].monoid
  val catsLawsRuleSetSemiLattice = SemilatticeTests[MySemiLattice].semilattice

  checkAll(name = "semigroup", ruleSet = catsLawsRuleSetSemigroup)

  checkAll(name = "monoid", ruleSet = catsLawsRuleSetMonoid)

  checkAll(name = "semilattice", ruleSet = catsLawsRuleSetSemiLattice)

}
