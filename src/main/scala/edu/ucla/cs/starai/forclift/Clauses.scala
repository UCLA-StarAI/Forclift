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
import util._
import edu.ucla.cs.starai.forclift.inference._
import sun.reflect.generics.reflectiveObjects.NotImplementedException

import constraints._

//Clauses

object Clause {

  val empty = new Clause(List(), List())

  // TODO check for number of literals to create specific subclass
  def apply(posLits: List[Atom],
    negLits: List[Atom],
    initialConstrs: Constraints = Constraints.empty) = new Clause(posLits, negLits, initialConstrs)
}

/**
 * A clause is a set of positive and negative literals constituting a
 * disjunction of literals as used in a CNF.
 *
 * @note Invariant:
 *       Inequality constraints are not between logical variables with disjoint
 *       domains
 */
class Clause(
  val posLits: List[Atom],
  val negLits: List[Atom],
  initialConstrs: Constraints = Constraints.empty) {

  //	def subsumes(other: Clause) = {
  //		//TODO check domains/variables
  //		posLits.subsetOf(other.posLits) && negLits.subsetOf(other.negLits)
  //	}

  val atoms: List[Atom] = posLits ::: negLits

  val literalVariables: Set[Var] = atoms.flatMap { _.variables }.toSet

  lazy val constants: Set[Constant] = atoms.flatMap { _.constants }.toSet

  lazy val predicates = atoms.map { _.predicate }.toSet

  //	if(ineqConstrs.variables.exists{v: Var => !variables(v)}){
  //		println("debug")
  //	}

  final val constrs: Constraints = initialConstrs.addMissingConstraints(literalVariables, atoms)
  
  def constrVariables: Set[Var] = constrs.variables
  def allVariables = constrVariables union literalVariables

  def domains = constrs.domains

  // assumption no linger holds now that we keep all constraints
  // assume(!ineqConstrs.variables.exists { v: Var => !variables(v) })

  def isUnitClause = atoms.size == 1

  def isPositiveUnitClause = negLits.isEmpty && posLits.size == 1

  def isNegativeUnitClause = posLits.isEmpty && negLits.size == 1

  /**
   * Check whether there are constraints not pertaining to the variables in the literals
   */
  def isConditional = (constrVariables -- literalVariables).nonEmpty

  def isUnconditional = !isConditional

  // Cannot test a contradiction unless you're sure that the constraints have a solution!
  def isConditionalContradiction = posLits.isEmpty && negLits.isEmpty

  def isTautology = posLits.exists { l1 => negLits.exists { l2 => (l1 == l2) } }

  def isGround = atoms.forall { _.isGround }
  
  /**
   * Get a most general unifier of the given atom with a literal that is not yet shattered w.r.t. the given constraints.
   * A set of equivalence classes needs shattering when
   * either
   * - there is a class that contains a variable from the literal and a constant
   * - there is a class that contains 2 variables from the literal
   * - there is a constraint between a classes in the atom and a constant that is not present in the literal constraints.
   * - there is a constraint between two classes in the atom that is not present in the literal constraints.
   * and
   * - there is not inequality constraint between elements of the same class.
   */
  //    val getShatteringMgu = Memoize((atom: Atom, atomIneqs: IneqConstr, atomElems: ElemConstr) => {
  def getShatteringMgu(atom: Atom, atomConstrs: Constraints) = {
    atoms.view.map { literal =>
      literal.getShatteringMgu(atom, atomConstrs, constrs)
    }.collect {
      case Some(eqClasses) => eqClasses
    }.headOption
  }

  def shatter(atom: Atom, atomConstrs: Constraints): List[Clause] = {
    //		println("Shattering "+ this + " wrt "+atom+" with "+atomIneqs+" and "+atomElems)
    //		nbShatters+=1
    //		if(nbShatters>20){
    //		    println("Debug")
    //		}
    val mguOption = getShatteringMgu(atom, atomConstrs)
    mguOption match {
      case None => {
        // already shattered
        List(this)
      }
      case Some(mgu) => {
        val reifiedOriginalMGU = mgu.map { _.reify }
        def substituteByRes(variable: Var) = {
          reifiedOriginalMGU.find(_._2.contains(variable)).map { _._1 }.getOrElse {
            println(variable)
            println(reifiedOriginalMGU)
            throw new IllegalStateException
            //                      // do not assume every var is in the atoms, there might be constraints on other variables!
            //                      variable
          }
        }
        def shatterEqualities(clause: Clause, reifiedMGU: List[(Term, Set[Var])]): List[Clause] = {
          reifiedMGU match {
            case (res, set) :: rest => {
              //							if(set.isEmpty){
              //								println("debug")
              //							}
              val firstVar = set.head
              val setRemaining = set - firstVar
              val restEqualities = if (setRemaining.isEmpty) rest else (res, setRemaining) :: rest
              val thisWithEq = clause.substitute(firstVar, res)
              if (res.isInstanceOf[Constant] || clause.literalVariables(res.asInstanceOf[Var])) {
                // split
                val thisWithInEq = clause.addInequality(firstVar, res)
                assume(thisWithInEq.toString != this.toString)
                thisWithInEq :: shatterEqualities(thisWithEq, restEqualities)
              } else {
                // do first substitution, because res is not yet in this clause
                shatterEqualities(thisWithEq, restEqualities)
              }
            }
            case Nil => {
              // substitute and project the atom inequalities to shatter them as well	
              // update: do not project!
              // update2: do project?!?!? confused!
              val relevantAtomIneqs = atomConstrs.ineqConstrs.project(atom.variables).substitute(substituteByRes)
              // remove inequalities already present in this clause	
              val newIneqs = relevantAtomIneqs -- clause.constrs.ineqConstrs
              shatterIneqConstraints(clause, newIneqs)
            }
          }
        }
        def shatterIneqConstraints(clause: Clause, ineqConstr: IneqConstr): List[Clause] = {
          if (ineqConstr.nonEmpty) {
            val (firstVar, set) = ineqConstr.head
            //						if(set.isEmpty){
            //							println("debug")
            //						}
            val firstArg = set.head
            val thisWithInEq = clause.addInequality(firstVar, firstArg)
            val thisWithEq = clause.substitute(firstVar, firstArg)
            assume(thisWithEq.toString != this.toString)
            val remainingIneqConstr = ineqConstr - (firstVar, firstArg)
            thisWithEq :: shatterIneqConstraints(thisWithInEq, remainingIneqConstr)
          } else {
            List(clause)
          }
        }
        val reifiedProjectedMGU = reifiedOriginalMGU.map { case (res, set) => (res, set.intersect(literalVariables)) }.filter { _._2.nonEmpty }
        val shatteredOnceClauses = shatterEqualities(this, reifiedProjectedMGU)
        val shatteredOnceStandardizedClauses = shatteredOnceClauses.map { _.standardizeApart }
        //				println(shatteredOnceStandardizedClauses.map{" - "+_.toString}.mkString("\n"))
        shatteredOnceStandardizedClauses.flatMap { _.shatter(atom, atomConstrs) }
      }
    }
  }

  //    val getDomainShatteringMgu = Memoize((atom: Atom, atomIneqs: IneqConstr, atomElems: ElemConstr) => {
  def getDomainShatteringMgu(atom: Atom, atomConstrs: Constraints) = {
    atoms.view.map { literal =>
      literal.getDomainShatteringMgu(atom, atomConstrs, constrs)
    }.collect {
      case Some(eqClasses) => eqClasses
    }.headOption
  }

  def shatterDomains(atom: Atom, atomConstrs: Constraints): List[Clause] = {
    val mguOption = getDomainShatteringMgu(atom, atomConstrs)
    val result = mguOption match {
      case None => {
        // already shattered
        List(this)
      }
      case Some(mgu) => {
        def shatterDomainConstraints(clause: Clause, eqClasses: List[EquivalenceClass]): List[Clause] = {
          eqClasses match {
            case eqClass :: rest => {
              // remove atomvars because atomvars may overlap with clausevars
              val thisVars = eqClass.variables -- atom.variables
              // it is possible that thisVars.size == 0 when there is another eqClass which does require domain shattering, then just ignore this eqClass
              if (thisVars.size == 0) {
                shatterDomainConstraints(clause, rest)
              } else {
                assume(thisVars.size == 1)
                val thisVar = thisVars.head
                val atomVars = eqClass.variables - thisVar
                val atomDomains = atomConstrs.domainsFor(atomVars)
                val atomDomainsIntersect = atomConstrs.elemConstrs.sharedDomain(eqClass.project(atom.variables));
                if (atomDomainsIntersect == EmptyDomain) {
                  shatterDomainConstraints(clause, rest)
                } else {
                  val thisDomain = clause.constrs.domainFor(thisVar)
                  val intersection = thisDomain.intersect(atomDomainsIntersect)
                  val restDomains = thisDomain.setMinus(atomDomainsIntersect)
                  if (restDomains.isEmpty) {
                    shatterDomainConstraints(clause, rest)
                  } else {
                    val unifyingClause = clause.setDomain(thisVar, intersection)
                    val otherClauses = restDomains.map { clause.setDomain(thisVar, _) }
                    otherClauses ++ shatterDomainConstraints(unifyingClause, rest)
                  }
                }

              }
            }
            case Nil => List(clause)
          }
        }
        val shatteredOnceClauses = shatterDomainConstraints(this, mgu)
        assume(!shatteredOnceClauses.contains(this))
        val shatteredOnceStandardizedClauses = shatteredOnceClauses.map { _.standardizeApart }
        shatteredOnceStandardizedClauses.flatMap { _.shatterDomains(atom, atomConstrs) }
      }
    }
    //        Clause.recursion = 0
    result
  }

  def shatterIneqsAndDomains(atom: Atom, atomConstrs: Constraints) = {
    shatter(atom, atomConstrs).flatMap { _.shatterDomains(atom, atomConstrs) }
  }

  def shatterInternalEqualities: List[Clause] = {
    atoms.view.map { literal =>
      if (literal.variables.size >= 2) {
        literal.variables.subsets(2).find { pair =>
          val List(x, y) = pair.toList
          // check whether they can be equal according to the domain definitions!!!
          constrs.canBeEqual(x, y)
        }.map { pair =>
          val List(x, y) = pair.toList
          val clauses1 = this.addInequality(x, y).shatterInternalEqualities.map { _.standardizeApart }
          val clauses2 = this.substitute(x, y).shatterInternalEqualities.map { _.standardizeApart }
          clauses1 ++ clauses2
        }
      } else None
    }.collectFirst { case Some(clauses) => clauses }.getOrElse(List(this))
  }

  def propagatePositive(atom: Atom, atomConstrs: Constraints) = {
    // check whether shattered
    assume(getShatteringMgu(atom, atomConstrs).isEmpty, "Clause needs to be shattered before unit proagation")
    assume(getDomainShatteringMgu(atom, atomConstrs).isEmpty, "Clause needs to be shattered before unit proagation")
    if (posLits.exists { literal =>
      literal.unifies(
        atom,
        atomConstrs,
        constrs)
    }) {
      List()
    } else if (negLits.exists { literal =>
      literal.unifies(
        atom,
        atomConstrs,
        constrs)
    }) {
      val newNegLiterals = negLits.filterNot { literal =>
        literal.unifies(atom,
          atomConstrs,
          constrs)
      }
      val remainingVars = (posLits.flatMap { _.variables } ++ newNegLiterals.flatMap { _.variables }).toSet
      //            if(newNegLiterals.isEmpty && posLits.isEmpty) {
      //                throw new Exception("TEST")
      //            }
      List(new Clause(
        posLits,
        newNegLiterals,
        constrs))
    } else List(this)
  }

  def propagateNegative(atom: Atom, atomConstrs: Constraints) = {
    // check whether shattered
    assume(getShatteringMgu(atom, atomConstrs).isEmpty, "Clause needs to be shattered before unit proagation")
    assume(getDomainShatteringMgu(atom, atomConstrs).isEmpty, "Clause needs to be shattered before unit proagation")
    if (negLits.exists { literal =>
      literal.unifies(
        atom,
        atomConstrs,
        constrs)
    }) {
      List()
    } else if (posLits.exists { literal =>
      literal.unifies(
        atom,
        atomConstrs,
        constrs)
    }) {
      val newPosLiterals = posLits.filterNot {
        _.unifies(
          atom,
          atomConstrs,
          constrs)
      }
      val remainingVars = (negLits.flatMap { _.variables } ++ newPosLiterals.flatMap { _.variables }).toSet
      List(new Clause(
        newPosLiterals,
        negLits,
        constrs)) //.project(remainingVars)))
    } else List(this)
  }

  def condition(sign: Boolean, atom: Atom, atomConstrs: Constraints): List[Clause] = {
    val shatteredClauses = shatter(atom, atomConstrs)
    val shatteredClauses2 = shatteredClauses.flatMap { _.shatterDomains(atom, atomConstrs) }
    val propagatedClauses = if (sign) {
      shatteredClauses2.flatMap { _.propagatePositive(atom, atomConstrs) }
    } else shatteredClauses2.flatMap { _.propagateNegative(atom, atomConstrs) }
    propagatedClauses
  }

  def substitute(from: Var, to: Term): Clause = {
    def substitution(v: Var) = if (v == from) to else v
    substitute(substitution)
  }

  def substitute(substitution: Var => Term): Clause = {
    Clause(
      posLits.map { _.substitute(substitution) },
      negLits.map { _.substitute(substitution) },
      constrs.substitute(substitution))
  }

  def setDomain(variable: Var, domain: Domain) = {
    Clause(posLits,
      negLits,
      constrs.setDomain(variable, domain))
  }

  def addInequality(v: Var, a: Term) = {
    Clause(posLits, negLits, constrs.addInequality(v, a))
  }

  def addLiteral(sign: Boolean, atom: Atom) = {
    if (sign) Clause(atom :: posLits, negLits, constrs)
    else Clause(posLits, atom :: negLits, constrs)
  }

  def standardizeApart = {
    val map = new mutable.HashMap[Var, Var]()
    substitute(v => {
      assume(!map.values.exists(_ == v))
      map.getOrElseUpdate(v, new Var)
    })
  }

  def independent(other: Clause) = {
    if (isConditionalContradiction || other.isConditionalContradiction) true
    else {
      val independent = atoms.forall { literal =>
        other.atoms.forall { otherLiteral =>
          !literal.unifies(otherLiteral, other.constrs, constrs)
        }
      }
      assume((this.toString != other.toString) || !independent)
      independent
    }
  }

  def dependent(other: Clause) = !independent(other)

  def needsShattering(catom: PositiveUnitClause): Boolean = {
    needsShattering(catom.atom, catom.constrs)
  }

  def needsShattering(atom: Atom, atomConstrs: Constraints): Boolean = {
    (getShatteringMgu(atom, atomConstrs).nonEmpty ||
      getDomainShatteringMgu(atom, atomConstrs).nonEmpty)
  }

  /**
   * Shatter inequalities so that both sides have the same set of groundings
   */
  def needsIneqDomainShattering: Boolean = constrs.needsIneqDomainShattering

  //TODO move to Constraints
  def shatterIneqDomains: List[Clause] = {
    constrs.ineqDomainShatteringVarVarPair match {
      case Some((v1, v2)) => {
        val v1Domain = constrs.domainFor(v1)
        val v2Domain = constrs.domainFor(v2)
        val intersection = v1Domain.intersect(v2Domain)
        val restDomains = v1Domain.setMinus(v2Domain)
        val firstClause = setDomain(v1, intersection)
        val otherClauses = restDomains.map { setDomain(v1, _) }
        (firstClause :: otherClauses).flatMap { _.shatterIneqDomains }.map { _.standardizeApart }
      }
      case None => constrs.ineqDomainShatteringVarTermPair match {
        case Some((v1, t2)) => {
          val clause1 = substitute(v1, t2)
          val clause2 = addInequality(v1, t2)
          List(clause1, clause2).flatMap { _.shatterIneqDomains }.map { _.standardizeApart }
        }
        case None => List(this)
      }
    }
  }

  def groundLiterals = atoms.filter { _.isGround }

  def singletonLiterals = atoms.filter { _.isSingleton }

  def length = atoms.length

  lazy val rootVars = atoms.foldLeft(literalVariables) { _ intersect _.variables }

  def independentLiterals: Option[(Clause, Clause)] = {
    def independent(a: Atom, b: Atom) = {
      val relatedAVars = a.variables union constrs.differentFrom(a.variables).collect { case v: Var => v }
      val relatedBVars = b.variables union constrs.differentFrom(b.variables).collect { case v: Var => v }
      (relatedAVars intersect relatedBVars).isEmpty

    }
    def partition(depLiterals: List[Atom], indepLiterals: List[Atom]): (List[Atom], List[Atom]) = {
      if (indepLiterals.isEmpty) (depLiterals, Nil)
      else depLiterals match {
        case literal :: rest => {
          val (indep, dep) = indepLiterals.partition(independent(literal, _))
          val (depAll, indepAll) = partition(rest ++ dep, indep)
          (literal :: depAll, indepAll)
        }
        case Nil => (Nil, indepLiterals)
      }
    }
    val (dep, indep) = partition(List(atoms.head), atoms.tail)
    if (indep.isEmpty) None
    else {
      val vars1 = dep.flatMap { _.variables }.toSet
      val vars2 = indep.flatMap { _.variables }.toSet
      val posLits1 = posLits.filter { l => dep.exists { l eq _ } }
      val posLits2 = posLits.filter { l => indep.exists { l eq _ } }
      val negLits1 = negLits.filter { l => dep.exists { l eq _ } }
      val negLits2 = negLits.filter { l => indep.exists { l eq _ } }
      val cl1 = new Clause(posLits1, negLits1, constrs)
      val cl2 = new Clause(posLits2, negLits2, constrs)
      // no need to standardize apart
      Some((cl1, cl2))
    }
  }

  def toUnitClause: UnitClause = {
    if (isPositiveUnitClause) new PositiveUnitClause(atoms.head, constrs)
    else if (isNegativeUnitClause) new NegativeUnitClause(atoms.head, constrs)
    else if (isConditionalContradiction) toContradictionClause
    else throw new IllegalStateException
  }

  def toPositiveUnitClause: PositiveUnitClause = {
    if (isPositiveUnitClause || isNegativeUnitClause) new PositiveUnitClause(atoms.head, constrs)
    else throw new IllegalStateException
  }

  def toNegativeUnitClause: NegativeUnitClause = {
    if (isPositiveUnitClause || isNegativeUnitClause) new NegativeUnitClause(atoms.head, constrs)
    else throw new IllegalStateException
  }

  def toContradictionClause: ContradictionClause = {
    if (isConditionalContradiction) new ContradictionClause(constrs)
    else throw new IllegalStateException
  }

  def toPositiveUnitClauses: Set[PositiveUnitClause] = {
    val unitClauses = for (literal <- atoms) yield {
      // fixed somewhere else, by projecting when used for shattering
      (new PositiveUnitClause(literal, constrs)).standardizeApart
    }
    unitClauses.toSet
  }

  def stripConstraints = new Clause(posLits,negLits,Constraints.empty)
  
  /**
   * Return a list of all ground clauses based on this clause.
   * Takes into account the constraints on the clauses.
   *
   * @param  domainSizes
   */
  def ground(domainSizes: DomainSizes): List[Clause] = {
    if (literalVariables.isEmpty) {
		// check whether constraints have a solution
		val dummyUnitClause = if(isConditionalContradiction) toContradictionClause 
				  				else new PositiveUnitClause(atoms.head, constrs);
		if (dummyUnitClause.nbGroundings(domainSizes) > 0) {
		  // return clause without constraints (they are satisfiable)
		  List(this.stripConstraints)
		} else {
		  List() // constraints are empty
		}
    } else {
      if (needsIneqDomainShattering) {
        shatterIneqDomains.flatMap { _.ground(domainSizes) }
      } else {
        val v = literalVariables.head
        val domain = constrs.domainFor(v)
        val excludedConstants: Set[Constant] = constrs.differentFrom(Set(v)).collect { case c: Constant => c }
        val constants = domain.constants(domainSizes, excludedConstants.toSet)
        val groundedOnce = constants.map { substitute(v, _) }
        groundedOnce.flatMap { _.ground(domainSizes) }
      }
    }
  }

  //    def ground(v: Var, domainSizes: DomainSizes, subdomain: Set[Constant]): List[Clause] = {
  //        require(variables.contains(v))
  //        val domain = constrs.domainFor(v)
  //        val excludedConstants: Set[Constant] = constrs.differentArguments(Set(v)).collect { case c: Constant => c }
  //        val allConstants = domain.constants(domainSizes, excludedConstants.toSet)
  //        val allConstantsSet = allConstants.toSet
  //        assume(allConstants.size == allConstantsSet.size)
  //        val constants = allConstantsSet intersect subdomain
  //        val groundedOnce = constants.map { c =>
  //            substitute(v, c)
  //        }
  //        groundedOnce.toList
  //    }

  def projectConstraints: Clause = new Clause(
    posLits,
    negLits,
    constrs.project(literalVariables))

  def toLatex(showRootDomains: Boolean = false) = {
    val nameSpace = new VarNameSpace
    val literalStr = if (posLits.isEmpty && negLits.isEmpty) "false" else (posLits.map { _.toLatex(nameSpace) } union negLits.map { """\neg """ + _.toLatex(nameSpace) }).mkString(""" \lor """)
    List(literalStr, constrs.toLatex(nameSpace, showRootDomains)).filter { _.nonEmpty }.mkString(", ")
  }

  override lazy val toString: String = {
    val nameSpace = new VarNameSpace
    val literalStr = if (posLits.isEmpty && negLits.isEmpty) "\bot" else (posLits.map { _.toString(nameSpace) } union negLits.map { _.toString(nameSpace, false) }).mkString(" ∨ ")
    List(literalStr, constrs.toString(nameSpace)).filter { _.nonEmpty }.mkString(", ")
  }

  /**
   * Replace the = operator (or eq/2) by constraints.
   *
   * {{{
   * f(x,y) v  eq(x,y) => f(x,x)
   * f(x,y) v !eq(x,y) => f(x,y), x != y
   * }}}
   */
  def eqToConstraints: Clause = {
    val (posequalities, posliterals) = posLits.partition { _.predicate == Predicate.eq }
    val (negequalities, negliterals) = negLits.partition { _.predicate == Predicate.eq }

    val eqPredConstrTuples: List[(Var, Term)] = posequalities.map {
      _.args.toList match {
        case v1 :: v2 :: Nil if v1.isInstanceOf[Var] && v2.isInstanceOf[Term] => (v1.asInstanceOf[Var], v2.asInstanceOf[Term])
        case _ => throw new IllegalStateException("Equality has no two variables as arguments")
      }
    }

    val eqSubsTuples: List[(Var, Term)] = negequalities.map {
      _.args.toList match {
        case v1 :: v2 :: Nil if v1.isInstanceOf[Var] && v2.isInstanceOf[Term] => (v1.asInstanceOf[Var], v2.asInstanceOf[Term])
        case _ => throw new IllegalStateException("Equality has no two variables as arguments")
      }
    }

    val eqSubsMap = immutable.HashMap[Var, Term](eqSubsTuples: _*)
    val eqSubs = (v: Var) => eqSubsMap.getOrElse(v, v)

    (new Clause(
      posliterals,
      negliterals,
      initialConstrs.addInequalities(eqPredConstrTuples))).substitute(eqSubs).standardizeApart
  }
}

sealed trait UnitClause extends Clause {

  def atom: Atom

  def predicate = atom.predicate

  override lazy val shatterIneqDomains = super.shatterIneqDomains.map { _.toUnitClause }

  override lazy val projectConstraints = super.projectConstraints.toUnitClause

  def nbGroundings(domainSizes: DomainSizes): GInt = {
    if (nbConstraintGroundings(domainSizes) == 0) 0
    else {
      projectConstraints.nbConstraintGroundings(domainSizes)
    }
  }
  
  @inline def hasConstraintSolution(domainSizes: DomainSizes): Boolean = {
    // optimized for performance
    val iter = shatterIneqDomains.iterator
    while(iter.hasNext){
      if(iter.next.hasConstraintSolutionAssumingShatteredDomains(domainSizes)) return true
    }
    return false
  }
  
  @inline def nbConstraintGroundings(domainSizes: DomainSizes): GInt = {
    // optimized for performance
    var count = 0;
    val iter = shatterIneqDomains.iterator
    while(iter.hasNext){
      count += iter.next.nbGroundingsAssumingShatteredDomains(domainSizes)
    }
    count
//    shatterIneqDomains.foldLeft[GInt](0) { _ + _.nbGroundingsAssumingShatteredDomains(domainSizes) }
  }

  @inline private def hasConstraintSolutionAssumingShatteredDomains(domainSizes: DomainSizes): Boolean = {
    constrs.hasSolutionAssumingShatteredDomains(domainSizes)
  }

  @inline private def nbGroundingsAssumingShatteredDomains(domainSizes: DomainSizes): GInt = {
    constrs.nbGroundingsAssumingShatteredDomains(domainSizes)
  }

}

class PositiveUnitClause(
  val atom: Atom,
  val initialConstrs: Constraints = Constraints.empty)
  extends Clause(List(atom), List(), initialConstrs) with UnitClause {

  def removeExternalConstraints = new PositiveUnitClause(atom, constrs.project(atom.variables))

  override def shatterIneqsAndDomains(atom: Atom, atomConstrs: Constraints): List[PositiveUnitClause] = {
    val superShatter = super.shatterIneqsAndDomains(atom, atomConstrs)
    superShatter.map { _.toPositiveUnitClause }
  }

  //    def projectOnLiteralVars:PositiveUnitClause = {
  //      new PositiveUnitClause(atom, constrs.project(atom.variables))
  //    }

  def minus(others: Set[PositiveUnitClause]): List[PositiveUnitClause] = {
    others.foldLeft(List(this)) { (prev, clause) =>
      prev.flatMap { _ minus clause }
    }
  }

  def minus(other: PositiveUnitClause): List[PositiveUnitClause] = {
    val shatteredClauses = this.shatterIneqsAndDomains(other.atom, other.constrs)
    shatteredClauses.filter { _.independent(other) }
  }

  def subsumes(other: PositiveUnitClause): Boolean = (
    this.needsShattering(other.atom, other.constrs)
    && !other.needsShattering(this.atom, this.constrs))

  // equals requires corresponding hashCode! cannot do that easily
  def equivalent(other: PositiveUnitClause) = {
    val equivalent = ((this eq other) || (!this.independent(other) &&
      !this.needsShattering(other.atom, other.constrs) &&
      !other.needsShattering(this.atom, this.constrs)))
    assume(other.toString != this.toString || equivalent)
    //	    if(!equivalent) println(this +" and "+other +" are NOT equivalent")
    //	    else println(this +" and "+other +" are equivalent")
    equivalent
  }

  def inverseSubstitution(c: Constant, ineqs: Set[Constant], domain: Domain): PositiveUnitClause = {
    val v = new Var
    val newAtom = atom.inverseSubstitution(c, v)
    val newConstrs = constrs.inverseSubstitution(c, v, ineqs, domain)
    new PositiveUnitClause(newAtom, newConstrs)
  }

  def reverseDomainSplitting(from: Domain, subDomain: SubDomain): PositiveUnitClause = {
    new PositiveUnitClause(atom, constrs.reverseDomainSplitting(from, subDomain)).standardizeApart
  }

  override def standardizeApart: PositiveUnitClause = {
    super.standardizeApart.asInstanceOf[PositiveUnitClause]
  }

  // used by standardizeApart
  override def substitute(substitution: Var => Term): PositiveUnitClause = {
    new PositiveUnitClause(
      atom.substitute(substitution),
      constrs.substitute(substitution))
  }

  /**
   * Get a grounding whose constants are known to the domain.
   * When no constants are known, add them.
   */
  def getGrounding(domainSizes: DomainSizes) = {
    val queryGround = ground(domainSizes)
    require(!queryGround.isEmpty, "No groundings possible for " + this)
    val anonymousQuery = queryGround.head.groundLiterals.head
    val termRenaming = collection.mutable.Map.empty[Term, Constant]
    for ((d, c) <- anonymousQuery.predicate.domains.zip(anonymousQuery.args)) {
      val constant = c.asInstanceOf[Constant]
      if (d.knownConstants.contains(constant)) {
        termRenaming(constant) = constant
      } else {
        // we have to rename the constant to something not "anon*"
        //println("Warning: Adding new constant to "+d)
        termRenaming(constant) = new Constant("added_known_" + d.knownConstants.size)
        d.addConstant(termRenaming(constant))
      }
    }
    val query = anonymousQuery.predicate(anonymousQuery.args.map { termRenaming(_) }: _*)
    assume(this.subsumes(query.toPositiveUnitClause), this + " does not entail its grounding " + query)
    query
  }

}

class NegativeUnitClause(val atom: Atom,
  initialConstrs: Constraints = Constraints.empty)
  extends Clause(List(), List(atom), initialConstrs) with UnitClause {

}

class ContradictionClause(initialConstrs: Constraints = Constraints.empty)
  extends Clause(List(), List(), initialConstrs) with UnitClause {

  def atom: Atom = throw new IllegalArgumentException

}
