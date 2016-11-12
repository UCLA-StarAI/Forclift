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

package edu.ucla.cs.starai.forclift.conditioning

import edu.ucla.cs.starai.forclift._
import scala.language.implicitConversions
import edu.ucla.cs.starai.forclift.inference._
import scala.collection._
import constraints._

object ConditioningDomainSet {

  implicit def domainElements2SubDomainSizes(domainElements: Map[Domain, Set[Constant]]): DomainSizes = {
    DomainSizes.empty ++ domainElements.collect {
      case (root: SubDomain, set) =>
        (root, DomainSize(set.size, root))
    }
  }

}

trait ConditioningDomainSet

case class PartialConditioningDomainSet(
  conditioningDomains: List[PartialConditioningDomain]) extends ConditioningDomainSet {

  def addEvidence(
    domainSizes: DomainSizes,
    posEvidence: Set[PositiveUnitClause],
    negEvidence: Set[PositiveUnitClause],
    initialDomainElements: Map[Domain, Set[Constant]] = Map.empty): Map[Domain, Set[Constant]] = {
    val rootDomains = conditioningDomains.map { _.getInitialDomain(domainSizes) }.collect { case Some(mapping) => mapping }
    val domainElements = initialDomainElements ++ rootDomains
    conditioningDomains.foldLeft(domainElements) { (elems, domain) =>
      elems ++ domain.getDomainElements(elems, posEvidence, negEvidence)
    }
  }

}

case class FullConditioningDomainSet(conditioningDomains: List[FullConditioningDomain]) extends ConditioningDomainSet {

  def addEvidence(
    domainSizes: DomainSizes,
    posEvidence: Set[PositiveUnitClause],
    initialDomainElements: Map[Domain, Set[Constant]] = Map.empty): Map[Domain, Set[Constant]] = {
    val rootDomains = conditioningDomains.map { _.getInitialDomain(domainSizes) }.collect { case Some(mapping) => mapping }
    val domainElements = initialDomainElements ++ rootDomains
    conditioningDomains.foldLeft(domainElements) { (elems, domain) =>
      elems ++ domain.getDomainElements(elems, posEvidence)
    }
  }

}

sealed trait ConditioningDomain {
  def catom: PositiveUnitClause
  def positiveDomain: SubDomain
  def negativeDomain: SubDomain = positiveDomain.complement

  val variable = catom.literalVariables.head
  val fullDomain = catom.constrs.domainFor(variable)

  def extractUnifyingDomain(evidenceAtoms: Set[PositiveUnitClause]): Set[Constant] = evidenceAtoms.map { evidence =>
    require(evidence.atom.isGround)
    catom.atom.unifyConstrained(evidence.atom, evidence.constrs, catom.constrs.setDomain(variable, fullDomain.root))
  }.collect { case Some(mgu) => mgu }.map { mgu =>
    assume(mgu.size == 1)
    assume(mgu.head.constants.size == 1)
    mgu.head.constants.head
  }

  def getInitialDomain(domainSizes: DomainSizes): Option[(RootDomain, Set[Constant])] = fullDomain match {
    case root: RootDomain => {
      val constants = root.constants(domainSizes, catom.constrs.ineqConstrs(variable).collect { case c: Constant => c }).toSet
      Some((root -> constants))
    }
    case _ => None
  }
}

case class PartialConditioningDomain(
  catom: PositiveUnitClause,
  positiveDomain: SubDomain,
  knownDomain: SubDomain) extends ConditioningDomain {

  require(catom.literalVariables.size == 1)

  def unknownDomain: SubDomain = knownDomain.complement

  def getDomainElements(
    domainElements: Map[Domain, Set[Constant]],
    posEvidence: Set[PositiveUnitClause],
    negEvidence: Set[PositiveUnitClause]): Map[Domain, Set[Constant]] = {

    val fullDomainConstants = domainElements(fullDomain)

    val candidatePositiveDomain = extractUnifyingDomain(posEvidence)
    // filter out constants that are not in the subdomain constraints
    val positiveDomainConstants = candidatePositiveDomain intersect fullDomainConstants

    val candidateNegativeDomain = extractUnifyingDomain(negEvidence)
    // filter out constants that are not in the subdomain constraints
    val negativeDomainConstants = candidateNegativeDomain intersect fullDomainConstants
    val unknownDomainConstants = (fullDomainConstants diff negativeDomainConstants) diff positiveDomainConstants
    val knownDomainConstants = negativeDomainConstants union positiveDomainConstants
    val newdomainElements = (domainElements
      + (positiveDomain -> positiveDomainConstants)
      + (negativeDomain -> negativeDomainConstants)
      + (unknownDomain -> unknownDomainConstants)
      + (knownDomain -> knownDomainConstants))
    newdomainElements
  }
}

case class FullConditioningDomain(
  catom: PositiveUnitClause,
  positiveDomain: SubDomain) extends ConditioningDomain {

  def getDomainElements(
    domainElements: Map[Domain, Set[Constant]],
    posEvidence: Set[PositiveUnitClause]): Map[Domain, Set[Constant]] = {

    val fullDomainConstants = domainElements(fullDomain)

    val candidatePositiveDomain = extractUnifyingDomain(posEvidence)
    // filter out constants that are not in the subdomain constraints
    val positiveDomainConstants = candidatePositiveDomain intersect fullDomainConstants

    // filter out constants that are not in the subdomain constraints
    val negativeDomainConstants = fullDomainConstants diff positiveDomainConstants
    val newdomainElements = (domainElements
      + (positiveDomain -> positiveDomainConstants)
      + (negativeDomain -> negativeDomainConstants))
    newdomainElements
  }

}
