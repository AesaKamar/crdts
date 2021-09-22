package example

import alleycats.Empty
import cats.kernel.{CommutativeMonoid, Eq, Monoid, Semigroup, Semilattice}
import cats.kernel.laws.discipline._
import org.scalatest.funspec.{AnyFunSpec, AsyncFunSpec}
import cats.{Order, laws}
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSpecDiscipline
import org.apache.commons.collections4.trie.PatriciaTrie
import org.scalacheck.Prop.exception.check
import org.scalacheck.Prop.forAll
import org.scalatestplus.scalacheck.Checkers

import java.util
import java.util.UUID
import scala.jdk.CollectionConverters._

/** Must be:
  *   - A commutative Semigroup with an idempotent combine
  */
final case class MySemiLattice[A: Monoid](stuff: PatriciaTrie[A])

object MySemiLattice {
  implicit def eqInstance[A]: Eq[MySemiLattice[A]] = (x: MySemiLattice[A], y: MySemiLattice[A]) => {
    x.stuff.equals(y.stuff)
  }

  implicit def semigroupInstance[A: CommutativeMonoid]: Semigroup[MySemiLattice[A]] =
    (x: MySemiLattice[A], y: MySemiLattice[A]) => {
      val newStuff = new PatriciaTrie(x.stuff)
      y.stuff.forEach((k, v) => newStuff.merge(k, v, CommutativeMonoid[A].combine(_, _)))
      MySemiLattice(newStuff)
    }

  implicit def semilatticeInstance[A: CommutativeMonoid]: Semilattice[MySemiLattice[A]] =
    (x: MySemiLattice[A], y: MySemiLattice[A]) => semigroupInstance[A].combine(x, y)

  implicit def emptyInstance[A: CommutativeMonoid]: Empty[MySemiLattice[A]] =
    new Empty[MySemiLattice[A]] {
      override def empty: MySemiLattice[A] =
        MySemiLattice(new PatriciaTrie[A]())
    }

  implicit def monoidInstance[A: CommutativeMonoid]: Monoid[MySemiLattice[A]] =
    new CommutativeMonoid[MySemiLattice[A]] {
      override def empty: MySemiLattice[A]                                             = emptyInstance[A].empty
      override def combine(x: MySemiLattice[A], y: MySemiLattice[A]): MySemiLattice[A] =
        semigroupInstance[A].combine(x, y)
    }
}

final case class RGBA(r: Int, g: Int, b: Int, a: Int)
object RGBA {
  implicit val commutativeMonoidInstance: CommutativeMonoid[RGBA] = new CommutativeMonoid[RGBA] {
    def empty: RGBA = RGBA(Monoid.empty, Monoid.empty, Monoid.empty, Monoid.empty)

    val cb: (Int, Int) => Int = (x, y) => math.min(x + y, 255)

    def combine(x: RGBA, y: RGBA): RGBA =
      RGBA(cb(x.r, y.r), cb(x.g, y.g), cb(x.b, y.b), cb(x.a, y.a))

  }
}

/** Test class
  *
  * THe plan is to use a patricia tree to gossip diffs about a 2d canvas that people can edit
  */
class HelloSpec extends AnyFunSpec with FunSpecDiscipline with Configuration with Checkers {
  import scala.jdk.CollectionConverters._

  def mySemiLaticeGenInstance[A: CommutativeMonoid](a: Gen[A]): Gen[MySemiLattice[A]] = for {
    m <- Gen.mapOf[String, A](Gen.zip(Gen.alphaStr, a))
  } yield {
    val tree = new PatriciaTrie[A](m.asJava)
    MySemiLattice(tree)
  }

  implicit val genLong   = Gen.long
  implicit val genString = Gen.alphaStr

  val genPatricia: Gen[PatriciaTrie[String]] = for {
    m <- Gen.mapOf[String, String](Gen.zip(Gen.numStr, Gen.alphaStr))
  } yield new PatriciaTrie[String](m.asJava)

  def mySemiLaticeOf[A: CommutativeMonoid](a: Gen[A]): Arbitrary[MySemiLattice[A]] =
    Arbitrary(mySemiLaticeGenInstance[A])

  implicit val mySemiLatticeOfString: Arbitrary[MySemiLattice[Int]] =
    mySemiLaticeOf[Int](Gen.long.map(_.toInt))

//  val catsLawsRuleSetSemigroup   =
//    SemigroupTests[MySemiLattice[]](MySemiLattice.semigroupInstance).semigroup
//  val catsLawsRuleSetMonoid      = MonoidTests[MySemiLattice[String]].monoid
//  val catsLawsRuleSetSemiLattice = SemilatticeTests[MySemiLattice[String]].semilattice

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

//  checkAll(name = "semilattice", ruleSet = catsLawsRuleSetSemiLattice)
//  it("A Patricia tree should pass the comutativity law") {
//    check(forAll(genPatricia, genPatricia) { case (x, y) =>
//      val xWithY = new PatriciaTrie[String](x)
//      xWithY.putAll(y)
//      val yWithX = new PatriciaTrie[String](y)
//      yWithX.putAll(x)
//
//      val leftSide  = xWithY.values().asScala.toSet
//      val rightSide = yWithX.values().asScala.toSet
//      if (leftSide == rightSide)
//        true
//      else {
//        pprint.pprintln(leftSide)
//        pprint.pprintln(rightSide)
//        false
//      }
//    })
//  }

  /** I learned that we must combine over the inner type on conflicts with a commutative semigroup
    * operation
    */
  it(
    "A HashMap should pass the commutativity law under map.merge and CommutativeMonoid[A].combine"
  ) {

    check(
      forAll(
        Gen.mapOf[String, String](Gen.zip(Gen.alphaStr, Gen.alphaStr)).map(_.asJava),
        Gen.mapOf[String, String](Gen.zip(Gen.alphaStr, Gen.alphaStr)).map(_.asJava)
      ) { case (x, y) =>
        val xWithY = new util.HashMap[String, String](x)
        y.forEach { case (a, b) =>
          xWithY.merge(a, b, (l, r) => Order[String].min(l, r).concat(Order[String].max(l, r)))
        }
        val yWithX = new util.HashMap[String, String](y)
        x.forEach { case (a, b) =>
          yWithX.merge(a, b, (l, r) => Order[String].min(l, r).concat(Order[String].max(l, r)))
        }

        val leftSide  = xWithY.values().asScala.toSet
        val rightSide = yWithX.values().asScala.toSet
        if (leftSide == rightSide)
          true
        else {
          false
        }
      }
    )
  }

}
