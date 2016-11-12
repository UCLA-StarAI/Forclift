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

import collection._
import edu.ucla.cs.starai.forclift.inference._

final class CNF(val clauses: List[Clause]) extends SetProxy[Clause] {

  //    assume(!clauses.exists { clause1 =>
  //        clauses.exists { clause2 =>
  //            !(clause1 eq clause2) && (clause1.variables intersect clause2.variables).nonEmpty
  //        }
  //    })

  lazy val self = clauses.toSet

  override lazy val hashCode = super.hashCode

  lazy val variables = clauses.flatMap { _.literalVariables }.toSet

  def isGround = variables.isEmpty

  lazy val constants = clauses.flatMap { _.constants }.toSet

  lazy val predicates = clauses.flatMap { _.predicates }.toSet

  def domains = clauses.flatMap { _.domains }.toSet

  lazy val atoms = clauses.flatMap { _.atoms }.toSet

  def isSingleton = clauses.nonEmpty && clauses.tail.isEmpty
  
  def toLatex(showRootDomains: Boolean = false): String = {
    if (clauses.isEmpty) """$\top$"""
    else {
      //			if(clauses.size>10) (new CNF(clauses.take(10))).toLatex(showRootDomains)
      //			else {
      clauses.map { _.toLatex(showRootDomains) }.mkString("""$\begin{array}{c} """, """ \\ """, """ \end{array}$""")
      //			}
    }
  }

  override def +(clause: Clause) = new CNF(clause :: clauses)

  def isTautology = clauses.isEmpty

  def removeTautologies: CNF = {
    val tautologyFreeClauses = clauses.filterNot { _.isTautology }
    if (tautologyFreeClauses.size == clauses.size) this
    else new CNF(tautologyFreeClauses)
  }

  def toPositiveUnitClauses: Set[PositiveUnitClause] = clauses.flatMap { _.toPositiveUnitClauses }.toSet

  lazy val distinctPositiveUnitClauses = {
    val allCAtoms = clauses.flatMap { _.toPositiveUnitClauses }
    val distinctCAtoms = disjoin(allCAtoms)
    //	    println("Reduced catoms from "+allCAtoms.size+" to "+distinctCAtoms.size)
    distinctCAtoms
  }

  def coShatter = shatterInternalEqualities.shatter

  def equiprobableClasses = {
    val coShatteredThis = coShatter
    val allCAtoms = coShatteredThis.clauses.flatMap { _.toPositiveUnitClauses.map(_.removeExternalConstraints) }
    disjoin(allCAtoms)
  }

  def shatterInternalEqualities: CNF = {
    new CNF(clauses.flatMap { _.shatterInternalEqualities })
  }

  lazy val shatter: CNF = {
    shatterIneqs.shatterDomains
  }

  private[this] final def disjoin(cAtoms: List[PositiveUnitClause]): List[PositiveUnitClause] = {
    val atomsByPredicate = cAtoms.groupBy { _.atom.predicate }
    atomsByPredicate.values.flatMap { predAtoms =>
      val uniqueStringAtoms = predAtoms.map { a => (a.toString -> a) }.toMap
      disjoinHard(uniqueStringAtoms.values.toList)
    }.toList
  }

  private[this] final def disjoinHard(cAtoms: List[PositiveUnitClause]): List[PositiveUnitClause] = cAtoms match {
    case Nil => Nil
    case cAtom :: restAtoms => {
      val standerdizedAtom = cAtom.standardizeApart
      standerdizedAtom :: disjoin(restAtoms.filterNot(standerdizedAtom.equivalent(_)))
    }

  }

  final def shatterIneqs: CNF = {
    var cAtoms = distinctPositiveUnitClauses
    var clauses = this.clauses
    var somethingOnceChanged = false
    var somethingChanged = false
    var nbIterations = 0
    do {
      somethingChanged = false
      nbIterations += 1
      var shClauses = List[Clause]()
      var nextCAtoms = List[PositiveUnitClause]()
      for (clause <- clauses) {
        var shClause = List(clause)
        for (cAtom <- cAtoms) {
          val shatteredForCAtom = shClause.flatMap { _.shatter(cAtom.atom, cAtom.constrs) }
          shClause = shatteredForCAtom
        }
        if (shClause.size > 1) {
          // something was shattered
          somethingOnceChanged = true
          somethingChanged = true
          nextCAtoms = shClause.flatMap { _.toPositiveUnitClauses } ::: nextCAtoms
        } else {
          assume(shClause.head == clause)
        }
        shClauses = shClause ::: shClauses
      }
      // now remove cAtoms and try again shattering the newly introduced ones
      clauses = shClauses
      // TODO check for subsumes in some direction not equivalence?
      cAtoms = disjoin(nextCAtoms.filter { next => !cAtoms.exists { next.subsumes(_) } }) //distinct(nextCAtoms ++ cAtoms) //
    } while (somethingChanged)
    if (clauses.exists { _.needsIneqDomainShattering }) {
      (new CNF(clauses.flatMap { _.shatterIneqDomains })).shatterIneqs
    } else {
      if (!somethingOnceChanged) this
      else new CNF(clauses)
    }
  }

  final private def shatterDomains: CNF = {
    var cAtoms = distinctPositiveUnitClauses
    var clauses = this.clauses
    var somethingOnceChanged = false
    var somethingChanged = false
    var nbIterations = 0
    do {
      somethingChanged = false
      nbIterations += 1
      var shClauses = List[Clause]()
      var nextCAtoms = List[PositiveUnitClause]()
      for (clause <- clauses) {
        var shClause = List(clause)
        for (cAtom <- cAtoms) {
          shClause = shClause.flatMap { _.shatterDomains(cAtom.atom, cAtom.constrs) }
        }
        if (shClause.size > 1) {
          // something was shattered
          somethingOnceChanged = true
          somethingChanged = true
          nextCAtoms = shClause.flatMap { _.toPositiveUnitClauses } ::: nextCAtoms
        } else {
          assume(shClause.head == clause)
        }
        shClauses = shClause ::: shClauses
      }
      // now remove cAtoms and try again shattering the newly introduced ones
      clauses = shClauses
      // TODO check for subsumes in some direction not equivalence?
      cAtoms = disjoin(nextCAtoms.filter { next => !cAtoms.exists { next.subsumes(_) } }) //distinct(nextCAtoms ++ cAtoms) //
    } while (somethingChanged)
    if (clauses.exists { _.needsIneqDomainShattering }) {
      (new CNF(clauses.flatMap { _.shatterIneqDomains })).shatterDomains
    } else {
      if (!somethingOnceChanged) this
      else new CNF(clauses)
    }
  }

  //	def parallel[T](s: Seq[T]) = s.par
  //	def parallel[T](s: Seq[T]) = s

  //	def separator: Option[Set[Var]] = {
  //		val allRootVars = clauses.forall{_.rootVars.nonEmpty}
  //		if(allRootVars){
  //			val clauseRootVars = clauses.map{_.rootVars.toList}
  //			def pickSeperator(candidates: List[List[Var]], clauses: List[Clause]) = {
  //				candidates match{
  //					case (s :: rest1) :: rest2 => {
  //						clauses.head.literals
  //					}
  //					case Nil :: rest =>
  //					case Nil => //ready
  //				}
  //				
  //			}
  //		}else None
  //	}

  //	def shatterDomain(from: Domain, to1: Domain, to2: Domain) = {
  //		val splitClauses = clauses.flatMap{_.shatterDomain(from, to1, to2)}
  //		new CNF(splitClauses)
  //	}

  //	def shatterLiteralsInternally: CNF = {
  //	    val literalOption = clauses.view.flatMap{_.toPositiveUnitClauses}.map{_.internalEqualitySplit}.find{_.size > 1}.map{_.first}
  //	    if(literalOption.isEmpty) this
  //	    else{
  //	        val literal = literalOption.get
  //	        val shLiteral = literal.atom 
  //		    val shIneqConstr = literal.ineqConstrs
  //	    	val shElemConstr = literal.elemConstrs
  //			val shatteredClauses1 = clauses.flatMap{_.shatter(shLiteral, shIneqConstr, shElemConstr)}
  //			val shatteredClauses2 = shatteredClauses1.flatMap{_.shatterDomains(shLiteral, shIneqConstr, shElemConstr)}
  //			(new CNF(shatteredClauses2)).shatterLiteralsInternally
  //        }
  //	}

  def ++(other: CNF) = {
    new CNF(clauses ++ other.clauses)
  }

  def ground(domainSizes: DomainSizes): CNF = {
    new CNF(clauses.flatMap { _.ground(domainSizes) })
  }

  def condition(cliteral: UnitClause): CNF = {
    val conditionedClauses = cliteral match {
      case posLit: PositiveUnitClause =>
        clauses.flatMap { _.condition(true, posLit.atom, posLit.constrs) }
      case negLit: NegativeUnitClause =>
        clauses.flatMap { _.condition(false, negLit.atom, negLit.constrs) }
      case _ => throw new IllegalStateException("Clause not positive or negative")
    }
    new CNF(conditionedClauses)
  }

  def simplify(ignoredClauses: Set[Clause] = Set.empty): CNF = {
    //	    println
    //	    println("Simplifying "+this)
    val propagatableUnitClauses = clauses.filter { propClause =>
      !ignoredClauses(propClause) && propClause.isUnitClause && clauses.exists { clause =>
        !(clause eq propClause) && !propClause.independent(clause)
      }
    }
    if (propagatableUnitClauses.nonEmpty) {
      def propagate(unitClauses: List[Clause], theory: List[Clause]): (List[Clause], List[Clause]) = unitClauses match {
        case Nil => (Nil, theory)
        case unitClause :: otherUnitClauses => {
          //                	println("Propagating "+unitClause)
          val unitLiteral = unitClause.atoms.head
          val otherClauses: List[Clause] = theory filterNot (_ == unitClause)
          val propagatedClauses = otherClauses.flatMap { clause =>
            val conditionedClause = clause.condition(unitClause.isPositiveUnitClause, unitLiteral, unitClause.constrs)
            //                        if(conditionedClause.size <= 1) conditionedClause
            //                        else List(clause)
            conditionedClause
          }.toList
          //                    assume(propagatedClauses.forall{unitClause.independent(_)})
          val propagatedOtherUnitClauses = otherUnitClauses.flatMap { _.condition(unitClause.isPositiveUnitClause, unitLiteral, unitClause.constrs) }
          val (propagatedUnitClauses, simplifiedRest) = propagate(propagatedOtherUnitClauses, propagatedClauses)
          (unitClause :: propagatedUnitClauses, simplifiedRest)
        }
      }
      val (propagated, simplifiedRest) = propagate(propagatableUnitClauses, clauses)
      val newCnf = (new CNF(propagated ++ simplifiedRest)).simplify(ignoredClauses ++ propagated)
      //	        println("SIMPLIFIED")
      //	        println(this)
      //	        println("TO")
      //	        println(newCnf)
      //	        println
      newCnf
    } else this
  }

  //	def tryNegativeUnitPropagation(cnf: CNF) = {
  //		val unitClauseOption = cnf.clauses.find{_.isNegativeUnitClause}
  //		if(unitClauseOption.nonEmpty){
  //			val unitClause = unitClauseOption.get
  //			val unitLiteral = unitClause.literals.head
  //			val otherClauses: List[Clause] = cnf.clauses - unitClause
  //			val propagatedClauses = otherClauses.flatMap{_.condition(false,unitLiteral, unitClause.ineqConstrs, unitClause.elemConstrs)}
  //			val branchCnf = new CNF(propagatedClauses)
  //			val unitCNF = CNF(unitClause)
  //			val msg = "Unit propagation of $" + unitClause.toLatex() + "$."
  //			Some(new And(cnf,compile(unitCNF), compile(branchCnf), msg))
  //		}else None
  //	}

  def independent(other: Clause) = clauses.forall(_.independent(other))
  def dependent(other: Clause) = !independent(other)

  def independentSubtheories: List[CNF] = {
    def partition(depClauses: List[Clause], indepClauses: List[Clause]): (List[Clause], List[Clause]) = {
      if (indepClauses.isEmpty) (depClauses, Nil)
      else depClauses match {
        case clause :: rest => {
          val (indep, dep) = indepClauses.partition(clause.independent(_))
          val (depAll, indepAll) = partition(rest ++ dep, indep)
          (clause :: depAll, indepAll)
        }
        case Nil => (Nil, indepClauses)
      }
    }
    val (dep, indep) = partition(List(clauses.head), clauses.tail)
    if (indep.isEmpty) List(this)
    else (new CNF(dep)) :: (new CNF(indep)).independentSubtheories
  }

  override def toString = {
    if (isTautology) """true"""
    else clauses.toArray.sortBy { _.toString }.mkString("\n")
  }

  def eqToConstraints: CNF = {
    new CNF(clauses.map(_.eqToConstraints))
  }

}

object CNF {

  def apply(clauses: Clause*) = new CNF(clauses.toList)

}
