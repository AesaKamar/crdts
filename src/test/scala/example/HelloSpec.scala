package example

import alleycats.Empty
import cats.kernel.{CommutativeMonoid, Eq, Monoid, Semigroup, Semilattice}
import cats.kernel.laws.discipline._
import org.scalatest.funspec.{AnyFunSpec, AsyncFunSpec}
import cats.laws
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSpecDiscipline
import org.apache.commons.collections4.trie.PatriciaTrie

import java.util.UUID

/** Must be:
  *   - A commutative Semigroup with an idempotent combine
  */
final case class MySemiLattice[A: Monoid](stuff: PatriciaTrie[A])

object MySemiLattice {
  implicit def eqInstance[A]: Eq[MySemiLattice[A]] = (x: MySemiLattice[A], y: MySemiLattice[A]) =>
    x.stuff.equals(y.stuff)

  implicit def semigroupInstance[A: Monoid]: Semigroup[MySemiLattice[A]] =
    (x: MySemiLattice[A], y: MySemiLattice[A]) => {
      val newStuff =  new PatriciaTrie(x.stuff)
      newStuff.putAll(y.stuff)
      MySemiLattice(newStuff)
    }

  implicit def semilatticeInstance[A: Monoid]: Semilattice[MySemiLattice[A]] =
    (x: MySemiLattice[A], y: MySemiLattice[A]) => semigroupInstance[A].combine(x, y)

  implicit def emptyInstance[A: Monoid]: Empty[MySemiLattice[A]] = new Empty[MySemiLattice[A]] {
    override def empty: MySemiLattice[A] =
      MySemiLattice(new PatriciaTrie[A]())
  }

  implicit def monoidInstance[A: Monoid]: Monoid[MySemiLattice[A]] = new Monoid[MySemiLattice[A]] {
    override def empty: MySemiLattice[A]                                             = emptyInstance[A].empty
    override def combine(x: MySemiLattice[A], y: MySemiLattice[A]): MySemiLattice[A] =
      semigroupInstance[A].combine(x, y)
  }
}

class HelloSpec extends AnyFunSpec with FunSpecDiscipline with Configuration {
  import scala.jdk.CollectionConverters._

  def mySemiLaticeGenInstance[A: Monoid: Arbitrary: Gen]: Gen[MySemiLattice[A]] = for {
    m <- Gen.mapOf[String, A](Gen.zip(Gen.alphaStr, implicitly[Gen[A]]))
  } yield {
    val tree = new PatriciaTrie[A](m.asJava)
    MySemiLattice(tree)
  }

  implicit val genLong   = Gen.long
  implicit val genString = Gen.alphaStr

  implicit def mySemiLaticeArbitraryInstance[A: Gen: Arbitrary: Monoid]
      : Arbitrary[MySemiLattice[A]]                          =
    Arbitrary(mySemiLaticeGenInstance[A])
  implicit val arbitraryBlah: Arbitrary[MySemiLattice[Long]] =
    Arbitrary(mySemiLaticeGenInstance[Long])

  val catsLawsRuleSetSemigroup   =
    SemigroupTests[MySemiLattice[String]](MySemiLattice.semigroupInstance).semigroup
  val catsLawsRuleSetMonoid      = MonoidTests[MySemiLattice[String]].monoid
  val catsLawsRuleSetSemiLattice = SemilatticeTests[MySemiLattice[String]].semilattice

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

//  val a          = MySemiLattice[String](Set(UUID.randomUUID() -> "aaa"))
//  val b          = MySemiLattice[String](Set(UUID.randomUUID() -> "b"))
//  val withMiddle = MySemiLattice.semilatticeInstance[String].intercalate(b)
//  val left       = withMiddle.combineN(a, 2)
//  val rght       = withMiddle.combine(a, a)
//  val real       = MySemiLattice(Set.empty[(UUID, String)])
//  pprint.log(left)
//  pprint.log(rght)
//  pprint.log(real)
//
//  pprint.log(Set(1, 2, 3).diff(Set(1)))
//  pprint.log(Set(1).diff(Set(1, 2, 3)))
//  checkAll(name = "semigroup", ruleSet = catsLawsRuleSetSemigroup)

//  checkAll(name = "monoid", ruleSet = catsLawsRuleSetMonoid)

  checkAll(name = "semilattice", ruleSet = catsLawsRuleSetSemiLattice)

}
