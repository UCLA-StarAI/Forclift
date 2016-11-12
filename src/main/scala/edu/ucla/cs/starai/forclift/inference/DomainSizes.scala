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

package edu.ucla.cs.starai.forclift.inference

import edu.ucla.cs.starai.forclift._
import scala.language.implicitConversions
import edu.ucla.cs.starai.forclift.compiler._
import edu.ucla.cs.starai.forclift.conditioning._

import edu.ucla.cs.starai.forclift.propositional._
import scala.collection._
import util._


/**
 * Domain size parametrization for a domain.
 *
 * @param  size
 *         Explicit size of the domain.
 * @param  constants
 *         Explicit constants for the domain.
 *
 * @note Invariants:
 *  - `Domain.staticConstants` is a subset of `constants`
 *  - `Domain.dynamicConstants` is disjoint of `constants`
 *  - `size` is greater or equal to `Domain.staticConstants.size +
 *    Domain.dynamicConstants.size`.
 */
class DomainSize(
  val size: Int,
  val constants: Set[Constant] = Set()) {
  def +(s: DomainSize) = {
    //require(domain == s.domain)
    new DomainSize(size + s.size, constants ++ s.constants)
  }
  def isEmpty = (size == 0)
  override def toString = size + " " + constants.mkString("{", ",", "}")
}

object DomainSize {
  def apply(
    givenSize: Int,
    domain: Domain,
    givenConstants: Set[Constant] = Set()) =
    {
      // Enforce invariant
      val (size, constants): (Int, Set[Constant]) = domain match {
        case d: RootDomain => {
          val newConstants = givenConstants ++ d.staticConstants
          val newSize = givenSize.max(newConstants.size)
          (newSize, newConstants)
        }
        case _ => (givenSize, givenConstants)
      }
      require(size >= constants.size)
      new DomainSize(size, constants)
    }
}

/** Collection of domain size parametrizations. */
class DomainSizes(
  val self: Map[Domain, DomainSize] = Map.empty,
  val useExplicitConstants: Boolean = false) extends MapProxy[Domain, DomainSize] {

  // Check invariants
  // TODO what are the invariants for non-rootdomains?

  // don't require, enforce!
  //require(this.forall{
  //case (domain:RootDomain, domainSize) =>
  //domain.staticConstants.forall{ constant =>
  //domainSize.constants.contains(constant)
  //}
  //case _ => true
  //}, "Invariant failed:\n"+
  //"Domain.staticConstants is not a subset of explicit constants")

  require(this.forall {
    case (domain: RootDomain, domainSize) =>
      val fire = domain.dynamicConstants.exists { constant =>
        domainSize.constants.contains(constant)
      }
      !fire
    case _ => true
  }, "Invariant failed:\n" +
    "Domain.dynamicConstants overlaps with explicit constants in domain size")

  // HACK: allow for fewer domain elements than there are dynamic constants, 
  // but then assume the dynamic constants are not in the domain somehow.
  assume(this.forall {
    case (domain: RootDomain, domainSize) => {
      // only for root domains!
      //	    val invsat = domainSize.size >= domain.staticConstants.size+domain.dynamicConstants.size
      val invsat = domainSize.size >= domain.staticConstants.size //+domain.dynamicConstants.size
      if (!invsat) {
        println(domainSize.size + " < " + domain.staticConstants.mkString("[", ",", "]") + " union " + domain.dynamicConstants.mkString("[", ",", "]"))
      }
      invsat
    }
    case _ => true
  }, "Invariant failed:\n" +
    "Explicit size of root domain is smaller than Domain.dynamicConstants.size+Domain.staticConstants.size")

  def copy(
    self: Map[Domain, DomainSize] = self,
    useExplicitConstants: Boolean = useExplicitConstants) = {
    new DomainSizes(self, useExplicitConstants)
  }

  def asExplicitConstants = copy(useExplicitConstants = true)
  def asStaticConstants = copy(useExplicitConstants = false)

  
  def +(d: Domain, s: Int) = {
    copy(self = self + (d -> DomainSize(s,d)))
  }
  
  def +(kv: (Domain, Int)) = {
    val kvd: (Domain, DomainSize) = (kv._1, DomainSize(kv._2, kv._1))
    copy(self = self + kvd)
  }

  override def -(key: Domain) = copy(self = self - key)

  def toInts = this.map { case (k, v) => (k, v.size) }

  override def toString = self.iterator.map {
    case (d, s) =>
      //"domain "+d+" "+s +" "+ d.knownConstants.mkString("{",",","}")
      "domain " + d + " " + s
  }.mkString("\n")

  def toStringVerbose = self.iterator.map {
    case (d, s) =>
      "domain: " + d + "\n" +
        "domain.knownConstants: " + d.knownConstants + "\n" +
        "domainSize.size: " + s.size + "\n" +
        "domainSize.constants: " + s.constants + "\n"
  }.mkString("\n")

  def domains = self.keySet

  def project(domains: Set[Domain]) = copy(self = self.filterKeys(domains(_)))

  override val hashCode = super.hashCode

  /**
   * Get list of constants for the given domain. Returns the known constants
   * augmented with new anonymous constants to obtain the given explicit
   * domain size.
   */
  def constants(domain: Domain): List[Constant] = {
    val (domainSize, knownConstants) = this.get(domain) match {
      case Some(ds) => {
        if (useExplicitConstants) {
          (ds.size, ds.constants.toList)
        } else {
          (ds.size, domain.knownConstants)
        }
      }
      case None => throw new IllegalStateException("Unknown domain: %s" format domain.toString)
    }
    require(!knownConstants.exists(_.toString.startsWith("anon")),
      "Cannot ground theory with constants starting with anon")
    Stream.concat(knownConstants, Stream.from(1).map { i =>
      new Constant("anon" + i)
    }).take(domainSize).toList
  }

  def constants(domain: Domain, excluded: Set[Constant]): List[Constant] = {
    val allConstants = constants(domain)
    val allNonExplicitlyExcludedConstants = allConstants filterNot (excluded.toList.contains(_))
    allNonExplicitlyExcludedConstants.drop((excluded filterNot (allConstants.contains(_))).size)
  }

  /**
   * Combine this and other domainSizes maps by adding counts and merging
   * constants.
   */
  def combine(other: DomainSizes): DomainSizes = {
    val result = new mutable.HashMap[Domain, DomainSize]
    result ++= self
    other.foreach {
      case (domain, domainSize) =>
        val num = result.getOrElseUpdate(domain, DomainSize(0, domain))
        result.update(domain, num + domainSize)
    }
    copy(self = result)
  }

  /**
   * Combine this domainSizes with another one but only take over the names
   * in the other map not the counts or constants. (To make two domainSizes
   * consistent)
   */
  def makeConsistentWith(other: DomainSizes): DomainSizes = {
    val result = new mutable.HashMap[Domain, DomainSize]
    result ++= self
    other.foreach {
      case (domain, domainSize) =>
        // If unknown, add domain name with count 0, otherwise do nothing.
        result.getOrElseUpdate(domain, DomainSize(0, domain))
    }
    copy(self = result)
  }

  /** Return those domains that are size 0 and have no known constants. */
  def emptyDomains: Option[Set[Domain]] = {
    val empty = self.filter { case (dom, domSize) => domSize.isEmpty && dom.knownConstants.isEmpty }.map { case (k, v) => k }.toSet
    if (empty.isEmpty) {
      None
    } else {
      Some(empty)
    }
  }
}

object DomainSizes {
  val empty = new DomainSizes(Map.empty)

  implicit def map2DomainSizes(self: Map[Domain, DomainSize]): DomainSizes = new DomainSizes(self)
}
