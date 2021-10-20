package example

import alleycats.Empty
import cats.kernel._
import cats.kernel.laws.discipline._
import org.scalatest.funspec.AnyFunSpec
import cats.Order
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.prop.Configuration
import org.typelevel.discipline.scalatest.FunSpecDiscipline
import org.apache.commons.collections4.trie.PatriciaTrie
import org.scalacheck.Prop.forAll
import org.scalatestplus.scalacheck.Checkers

import java.util

/** Must be:
  *   - A [[CommutativeSemigroup]] with an idempotent combine AKA [[Band]]
  *     - Indexable with a unique identifier
  */
final case class MyCRDT[A: Monoid](stuff: PatriciaTrie[A])

/** Provide typeclass instances
  */
object MyCRDT {
  implicit def eqInstance[A]: Eq[MyCRDT[A]] = new Eq[MyCRDT[A]] {
    override def eqv(x: MyCRDT[A], y: MyCRDT[A]): Boolean = x.equals(y)
  }

  implicit def semigroupInstance[A: CommutativeMonoid]: Semigroup[MyCRDT[A]] =
    new Semigroup[MyCRDT[A]] {
      override def combine(x: MyCRDT[A], y: MyCRDT[A]): MyCRDT[A] = {
        val newStuff = new PatriciaTrie(x.stuff)
        y.stuff.forEach { (k, v) =>
          newStuff.merge(
            k,
            v,
            (oldVal, newVal) =>
              if (oldVal != newVal)
                CommutativeMonoid[A].combine(oldVal, newVal)
              else
                oldVal
          )
          ()
        }
        MyCRDT(newStuff)
      }
    }

  implicit def semilatticeInstance[A: CommutativeMonoid]
      : Semilattice[MyCRDT[A]] = new Semilattice[MyCRDT[A]] {
    override def combine(x: MyCRDT[A], y: MyCRDT[A]): MyCRDT[A] =
      semigroupInstance[A].combine(x, y)
  }

  implicit def emptyInstance[A: CommutativeMonoid]: Empty[MyCRDT[A]] =
    new Empty[MyCRDT[A]] {
      override def empty: MyCRDT[A] =
        MyCRDT(new PatriciaTrie[A]())
    }

  implicit def monoidInstance[A: CommutativeMonoid]: Monoid[MyCRDT[A]] =
    new CommutativeMonoid[MyCRDT[A]] {
      override def empty: MyCRDT[A]                               = emptyInstance[A].empty
      override def combine(x: MyCRDT[A], y: MyCRDT[A]): MyCRDT[A] =
        semigroupInstance[A].combine(x, y)
    }
}

/** A sample data structure which is a [[CommutativeMonoid]]
  */
final case class RGBA(r: Int, g: Int, b: Int, a: Int)
object RGBA {
  implicit val eqInstance: Eq[RGBA] = (x: RGBA, y: RGBA) => x == y

  implicit val commutativeMonoidInstance: CommutativeMonoid[RGBA] =
    new CommutativeMonoid[RGBA] {
      def empty: RGBA = RGBA(0, 0, 0, 0)

      // Use basic color addition
      val cb: (Int, Int) => Int = (x, y) => math.min(x + y, 255)

      def combine(x: RGBA, y: RGBA): RGBA =
        RGBA(cb(x.r, y.r), cb(x.g, y.g), cb(x.b, y.b), cb(x.a, y.a))

    }
}

/** Test class
  *
  * The plan is to use a patricia tree to gossip diffs about a 2d canvas that
  * people can edit
  */
class HelloSpec
    extends AnyFunSpec
    with FunSpecDiscipline
    with Configuration
    with Checkers {
  import HelloSpec._

  checkAll(
    name = "semigroup",
    ruleSet = catsLawsSemigroupForMySemiLatticeRGBA
  )
  checkAll(
    name = "commutativeMonoidRGBA",
    ruleSet = catsLawsCommutativeMonoidForRGBA
  )
  checkAll(
    name = "semiLatticeRGBA",
    ruleSet = catsLawsSemiLatticeForMySemiLatticeRGBA
  )

  it(
    "A HashMap should pass the commutativity law under map.merge and CommutativeMonoid[A].combine"
  ) {

    check(
      showMapCommutativity(
        newMap = new util.HashMap[String, String](_),
        semigroupOp =
          (l, r) => Order[String].min(l, r).concat(Order[String].max(l, r))
      )
    )
  }

  it(
    "A PatriciaTrie should pass the commutativity law under map.merge and CommutativeMonoid[A].combine"
  ) {

    check(
      showMapCommutativity(
        newMap = new PatriciaTrie[String](_),
        semigroupOp =
          (l, r) => Order[String].min(l, r).concat(Order[String].max(l, r))
      )
    )
  }

}

object HelloSpec {
  import scala.jdk.CollectionConverters._

  def showMapCommutativity[M <: util.AbstractMap[String, String]](
      newMap: util.Map[String, String] => M,
      semigroupOp: (String, String) => String
  ) = {
    forAll(
      Gen
        .mapOf[String, String](Gen.zip(Gen.alphaStr, Gen.alphaStr))
        .map(_.asJava),
      Gen
        .mapOf[String, String](Gen.zip(Gen.alphaStr, Gen.alphaStr))
        .map(_.asJava)
    ) { case (x, y) =>
      val xWithY = newMap(x)
      y.forEach { case (a, b) =>
        xWithY.merge(a, b, (l, r) => semigroupOp(l, r))
        ()
      }
      val yWithX = newMap(y)
      x.forEach { case (a, b) =>
        yWithX.merge(a, b, (l, r) => semigroupOp(l, r))
        ()
      }

      val leftSide  = xWithY.values().asScala.toSet
      val rightSide = yWithX.values().asScala.toSet
      if (leftSide == rightSide)
        true
      else
        false
    }
  }

  /*
   * Implement ScalaCheck helpers for property based tests
   * -----------------------------------------------------
   */
  def mySemiLaticeGenInstance[A: CommutativeMonoid](a: Gen[A]): Gen[MyCRDT[A]] =
    for {
      m <- Gen.mapOf[String, A](Gen.zip(Gen.uuid.map(_.toString), a))
    } yield {
      val tree = new PatriciaTrie[A](m.asJava)
      MyCRDT(tree)
    }

  implicit val genLong   = Gen.long
  implicit val genString = Gen.alphaStr

  val genPatricia: Gen[PatriciaTrie[String]] = for {
    m <- Gen.mapOf[String, String](Gen.zip(Gen.numStr, Gen.alphaStr))
  } yield new PatriciaTrie[String](m.asJava)

  def mySemiLaticeOf[A: CommutativeMonoid](a: Gen[A]): Arbitrary[MyCRDT[A]] =
    Arbitrary(mySemiLaticeGenInstance[A](a))

  implicit val mySemiLatticeOfString: Arbitrary[MyCRDT[Int]] =
    mySemiLaticeOf[Int](Gen.long.map(_.toInt))

  implicit val genRGBA: Gen[RGBA] =
    Gen
      .zip(
        Gen.choose(0, 255),
        Gen.choose(0, 255),
        Gen.choose(0, 255),
        Gen.choose(0, 255)
      )
      .map { case (r, g, b, a) =>
        RGBA.apply(r, g, b, a)
      }

  implicit val mySemiLatticeRGBA: Arbitrary[MyCRDT[RGBA]] =
    mySemiLaticeOf[RGBA](genRGBA)

  implicit val arbitratyRGBA: Arbitrary[RGBA] = Arbitrary(genRGBA)

  /*
   * Implement Cats Laws checking from [[cats.discipline]]
   * -----------------------------------------------------
   */
  val catsLawsSemigroupForMySemiLatticeRGBA
      : SemigroupTests[MyCRDT[RGBA]]#RuleSet =
    SemilatticeTests[MyCRDT[RGBA]].semigroup

  val catsLawsSemiLatticeForMySemiLatticeRGBA
      : SemilatticeTests[MyCRDT[RGBA]]#RuleSet =
    SemilatticeTests[MyCRDT[RGBA]].semilattice

  val catsLawsCommutativeMonoidForRGBA: CommutativeMonoidTests[RGBA]#RuleSet =
    CommutativeMonoidTests[RGBA].commutativeMonoid

}
