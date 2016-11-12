/*
 * Copyright 2016 Guy Van den Broeck and Wannes Meert (UCLA and KU Leuven)
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

package edu.ucla.cs.starai.forclift

import edu.ucla.cs.starai.forclift.inference._
import collection._

sealed abstract class Domain {

  private[this] var nbSplitsVar = 0;

  def nbSplits = nbSplitsVar

  def knownConstants: List[Constant]

  def complement: Domain

  /**
   * Assumes that all constants in excluded are part of the domain.
   */
  def size(domainSizes: DomainSizes, excluded: Set[Constant]): Int

  /**
   * Assumes that all constants in excluded are part of the domain.
   */
  def constants(domainSizes: DomainSizes, excluded: Set[Constant]): List[Constant]

  def contains(c: Constant) = knownConstants.contains(c)

  def subdomain(
    superScript: String = "1",
    complementSuperScript: String = "2",
    subScript: String = "a",
    complementSubScript: String = "b",
    excludedConstants: Set[Constant] = Set.empty): SubDomain = {
    // bad test, because IPG might introduce a constant not known to the domain
    //        assume(excludedConstants.forall { knownConstants.contains(_) })
    nbSplitsVar += 1
    new SubDomain(superScript, subScript, this, excludedConstants) {
      override lazy val complement = new ComplementDomain(complementSuperScript, complementSubScript, Domain.this, this, excludedConstants)
    }
  }

  def root: RootDomain

  def parents: Stream[Domain]

  //strict this superdomain of other
  def superDomain(other: Domain) = {
    other.parents.contains(this)
  }

  //strict this subdomain of other
  def subDomain(other: Domain) = {
    parents.contains(other)
  }

  def disjoint(other: Domain): Boolean = {
    intersect(other) == EmptyDomain
  }

  def intersect(other: Domain): Domain = {
    if (other == this) this
    else if (superDomain(other)) other
    else if (subDomain(other)) this
    else {
      val myAncestry = (this :: parents.toList).reverse
      val otherAncestry = (other :: other.parents.toList).reverse
      val (diff1, diff2) = (myAncestry zip otherAncestry).dropWhile { case (d1, d2) => d1 == d2 }.head
      // assume root domains are disjoint
      if ((diff1.isInstanceOf[RootDomain] || diff2.isInstanceOf[RootDomain]) ||
        diff1.asInstanceOf[SubDomain].complement == diff2) {
        EmptyDomain
      } else throw new IllegalStateException("Complex intersection!")
    }
  }

  def setMinus(other: Domain): List[Domain] = {
    if (other == this) List()
    else if (subDomain(other)) List()
    else if (superDomain(other)) {
      (other :: other.parents.toList).reverse.dropWhile(_ != this).tail.map { _.asInstanceOf[SubDomain].complement }
    } else {
      val myAncestry = (this :: parents.toList).reverse
      val otherAncestry = (other :: other.parents.toList).reverse
      val (diff1, diff2) = (myAncestry zip otherAncestry).dropWhile { case (d1, d2) => d1 == d2 }.head
      // assume root domains are disjoint
      if ((diff1.isInstanceOf[RootDomain] || diff2.isInstanceOf[RootDomain]) ||
        diff1.asInstanceOf[SubDomain].complement == diff2) {
        List()
      } else throw new IllegalStateException("Complex intersection!")
    }
  }

}

/**
 * Representation of a domain of a given size and optionally a domain size.
 *
 * @note Explanation about static and dynamic constants:
 *  - **grounding** and **counting** use the constants in `staticConstants`
 *    and `dynamicConstants`. `dynamicConstants` are only added when there are
 *    not enought constants in `staticConstants`.
 *  - **db** operations like LL ignore the constants in `staticConstants` and
 *    `dynamicConstants`.
 *
 * @param  name
 * @param  staticConstants
 *         Set of given constants that will always be part of the domain.
 */
class RootDomain(val name: String, val staticConstants: List[Constant] = List.empty) extends Domain {

  require(staticConstants.toSet.size == staticConstants.size, "Cannot have duplicate domain constants.")

  /**
   * Dynamically added constants after domain was created.
   * These constants are only used to keep track of anonymous constants
   * introduced while building circuits.
   *
   * @todo Is it ok to compile multiple times? Will this insert every time
   * some new constants? Does this cause problems?
   */
  val dynamicConstants = new mutable.ListBuffer[Constant]
  def addConstant(c: Constant) = {
    require(!knownConstants.contains(c))
    dynamicConstants.append(c)
  }

  def knownConstants = staticConstants ::: dynamicConstants.toList

  //    override def hashCode: Int = {
  //        //41 * staticConstants.foldLeft(1)((c,r) => 41*c + r.hashCode) + name.hashCode
  ////        name.hashCode
  //    }
  //
  //    /**
  //     * equals is necessary to put it in a collection.
  //     *
  //     * @todo    Check with object equality chapter in Scala book
  //     */
  //    override def equals(other: Any): Boolean = {
  //        this eq other
  //        //this.hashCode == other.hashCode
  ////        other match{
  ////            case root: RootDomain => this.name == root.name
  ////            case _ => false
  ////        }
  //    }

  def complement = EmptyDomain

  /**
   * Assumes that all constants in excluded are part of the domain.
   */
  def size(domainSizes: DomainSizes, excluded: Set[Constant]): Int = {
    //assume(excluded.forall{knownConstants.contains(_)}) not true per se, see unit tests
    //assume(domainSizes(this)>=excluded.size)  not true per se, if domain size is 0
    //		assume((knownConstants.toSet--excluded).size <= domainSizes(this) - excluded.size)  not true per se, if domain size is 0
    math.max(domainSizes(this).size - excluded.size, 0)
  }

  def constants(domainSizes: DomainSizes): List[Constant] = {
    //val domainSize = domainSizes(this).size
    //require(!knownConstants.exists(_.toString.startsWith("anon")),
    //"Cannot ground theory with constants starting with anon")
    //Stream.concat(knownConstants, Stream.from(1).map { i => Constant("anon" + i) }).take(domainSize).toList
    domainSizes.constants(this)
  }

  /**
   * Assumes that all constants in excluded are part of the domain.
   * But, they might be placeholders for any domain element.
   */
  def constants(domainSizes: DomainSizes, excluded: Set[Constant]): List[Constant] = {
    val allConstants = constants(domainSizes)
    // assume(excluded.forall{allConstants.contains(_)}) invalid because of placeholder constants
    val allNonExplicitlyExcludedConstants = allConstants filterNot (excluded.toList.contains(_))
    // drop some elements for placeholder constant
    allNonExplicitlyExcludedConstants.drop((excluded filterNot (allConstants.contains(_))).size)
  }

  override def toString = name

  def root = this

  def superScript = List()

  def subScript = List()

  def parents: Stream[Domain] = Stream.Empty

}

object EmptyDomain extends RootDomain("Empty") {

  override def size(domainSizes: DomainSizes, excluded: Set[Constant]) = 0

  override def constants(domainSizes: DomainSizes, excluded: Set[Constant]): List[Constant] = {
    require(excluded.forall { knownConstants.contains(_) })
    require(domainSizes(this).size == 0)
    List()
  }

  override def subdomain(
    superScript: String,
    complementSuperScript: String,
    subScript: String,
    complementSubScript: String,
    excludedConstants: Set[Constant]): SubDomain = throw new IllegalStateException

}
//
//class ComplexIntersection extends Domain("ComplexIntersection"){
//	
//	val name = "Empty"
//	
//	override def subdomain(name: String, complementName: String) = throw new IllegalStateException
//	
//}

object Universe extends RootDomain("U") {

  override def size(domainSizes: DomainSizes, excluded: Set[Constant]) = throw new IllegalStateException("Cannot compute the domain size of the Universe domain.")

  override def constants(domainSizes: DomainSizes, excluded: Set[Constant]): List[Constant] = {
    throw new IllegalStateException
  }

}

abstract class SubDomain(val superScript: String, val subScript: String, 
    parent: Domain, val excludedConstants: collection.Set[Constant]) extends Domain {

  def complement: SubDomain

  def root = parent.root

  def parents: Stream[Domain] = Stream.cons(parent, parent.parents)

  def knownConstants: List[Constant] = root.knownConstants

  // needs to be ordered, therefore List not Set
  lazy val knownIncludedConstants: List[Constant] = (knownConstants filterNot (excludedConstants.toList.contains(_))).toList

  // always assume domainSizes do not include excludedConstants

  /**
   * Assumes that all constants in excluded are part of the domain.
   */
  def size(domainSizes: DomainSizes, excluded: Set[Constant]): Int = {
    assume(excludedConstants.subsetOf(excluded), "Subdomains always have the same minimal set of excluded constants")
    val nbExtraExcluded = excluded.size - excludedConstants.size
    //assume(domainSizes(this)>=nbExtraExcluded) not true because domainSizes(this) might be 0 after atom counting
    math.max(0, domainSizes(this).size - nbExtraExcluded)
  }

  /**
   * Assumes that all constants in excluded are part of the domain.
   */
  def constants(domainSizes: DomainSizes, excluded: Set[Constant]): List[Constant] = {
    assume(excludedConstants.subsetOf(excluded))
    val mySize = size(domainSizes, excluded)
    val parentConstants = parent.constants(domainSizes, excluded)
    assume(parentConstants.size >= mySize)
    parentConstants.take(mySize)
  }

  override def toString = {
    val rootName = root.toString
    val superScript = (this :: parents.toList).reverse.collect { case sub: SubDomain => sub.superScript }.mkString("^{", ",", "}")
    val subScript = (this :: parents.toList).reverse.collect { case sub: SubDomain => sub.subScript }.mkString("_{", ",", "}")
    rootName + subScript + superScript
  }

}

class ComplementDomain(
  superScript: String,
  subScript: String,
  parent: Domain,
  val complement: SubDomain,
  excludedConstants: collection.Set[Constant]) extends SubDomain(superScript, subScript, parent, excludedConstants) {

  /**
   * Assumes that all constants in excluded are part of the domain.
   */
  override def constants(domainSizes: DomainSizes, excluded: Set[Constant]): List[Constant] = {
    assume(excludedConstants.subsetOf(excluded))
    val mySize = size(domainSizes, excluded)
    val parentConstants = parent.constants(domainSizes, excluded)
    assume(parentConstants.size >= mySize)
    parentConstants.takeRight(mySize)
  }

}
