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
    x.stuff.values().asScala.toSet.equals(y.stuff.values().asScala.toSet)
  }

  implicit def semigroupInstance[A: CommutativeMonoid]: Semigroup[MySemiLattice[A]] =
    (x: MySemiLattice[A], y: MySemiLattice[A]) => {
      val newStuff = new PatriciaTrie(x.stuff)
      y.stuff.forEach((k, v) =>
        newStuff.merge(
          k,
          v,
          (oldVal, newVal) =>
            if (oldVal != newVal)
              CommutativeMonoid[A].combine(oldVal, newVal)
            else
              oldVal
        )
      )
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
  implicit val eqInstance: Eq[RGBA] = (x: RGBA, y: RGBA) => x == y

  implicit val commutativeMonoidInstance: CommutativeMonoid[RGBA] = new CommutativeMonoid[RGBA] {
    def empty: RGBA = RGBA(0, 0, 0, 0)

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
    Arbitrary(mySemiLaticeGenInstance[A](a))

  implicit val mySemiLatticeOfString: Arbitrary[MySemiLattice[Int]] =
    mySemiLaticeOf[Int](Gen.long.map(_.toInt))

  implicit val genMySemiLatticeRGBA: Gen[RGBA] =
    Gen.zip(Gen.choose(0, 255), Gen.choose(0, 255), Gen.choose(0, 255), Gen.choose(0, 255)).map {
      case (r, g, b, a) => RGBA.apply(r, g, b, a)
    }

  implicit val mySemiLatticeRGBA: Arbitrary[MySemiLattice[RGBA]] =
    mySemiLaticeOf[RGBA](genMySemiLatticeRGBA)

  implicit val arbitratyRGBA                  = Arbitrary(genMySemiLatticeRGBA)
  val catsLawsSemiLatticeForMySemiLatticeRGBA = SemilatticeTests[MySemiLattice[RGBA]].semilattice

  val catsLawsCommutativeMonoidForRGBA = CommutativeMonoidTests[RGBA].commutativeMonoid

  checkAll(name = "commutativeMonoidRGBA", ruleSet = catsLawsCommutativeMonoidForRGBA)
  checkAll(
    name = "commutativeMonoidSemiLatticeRGBA",
    ruleSet = catsLawsSemiLatticeForMySemiLatticeRGBA
  )

  def showMapCommutativity[M <: util.AbstractMap[String, String]](
      newMap: util.Map[String, String] => M,
      semigroupOp: (String, String) => String
  ) = {
    forAll(
      Gen.mapOf[String, String](Gen.zip(Gen.alphaStr, Gen.alphaStr)).map(_.asJava),
      Gen.mapOf[String, String](Gen.zip(Gen.alphaStr, Gen.alphaStr)).map(_.asJava)
    ) { case (x, y) =>
      val xWithY = newMap(x)
      y.forEach { case (a, b) =>
        xWithY.merge(a, b, (l, r) => semigroupOp(l, r))
      }
      val yWithX = newMap(y)
      x.forEach { case (a, b) =>
        yWithX.merge(a, b, (l, r) => semigroupOp(l, r))
      }

      val leftSide  = xWithY.values().asScala.toSet
      val rightSide = yWithX.values().asScala.toSet
      if (leftSide == rightSide)
        true
      else
        false
    }
  }

  it(
    "A HashMap should pass the commutativity law under map.merge and CommutativeMonoid[A].combine"
  ) {

    check(
      showMapCommutativity(
        newMap = new util.HashMap[String, String](_),
        semigroupOp = (l, r) => Order[String].min(l, r).concat(Order[String].max(l, r))
      )
    )
  }

  it(
    "A PatriciaTrie should pass the commutativity law under map.merge and CommutativeMonoid[A].combine"
  ) {

    check(
      showMapCommutativity(
        newMap = new PatriciaTrie[String](_),
        semigroupOp = (l, r) => Order[String].min(l, r).concat(Order[String].max(l, r))
      )
    )
  }

}
