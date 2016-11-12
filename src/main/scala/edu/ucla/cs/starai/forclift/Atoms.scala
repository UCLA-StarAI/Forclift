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

import scala.collection._
import util._
import constraints._

//Predicates
final case class Predicate(
  val name: Symbol,
  val arity: Int,
  //TODO omit from equality???
  val domains: Seq[RootDomain]) {

  def this(name: Symbol, arity: Int) = {
    this(name, arity, Array.fill(arity)(Universe))
  }

  override val hashCode = name.hashCode * 41 + arity.hashCode

  override def toString = name.name.toString

  def toStringFull = name.name.toString + (if(domains.isEmpty) "" else domains.map(_.name).mkString("(", ",", ")"))

  def apply(args: Term*) = new Atom(this, args: _*)

  def toAtom: PositiveUnitClause = {
    val vars = Array.fill(arity) { new Var }
    (new PositiveUnitClause(new Atom(this, vars: _*))).standardizeApart
  }
}

object Predicate {
  lazy val eq = new Predicate(Symbol("eq"), 2)
}

//Atoms
final case class Atom(val predicate: Predicate, val args: Term*) {

  assume(predicate.arity == args.length)

  val variables = args.collect { case v: Var => v }.toSet

  val constants = args.collect { case c: Constant => c }.toSet

  def isGround = variables.isEmpty

  def isSingleton = (variables.size == 1)

  def toPositiveUnitClause = new PositiveUnitClause(this)

  def domain(v: Var) = {
    val index = args.indexOf(v)
    if (index < 0) throw new IllegalArgumentException
    else predicate.domains(args.indexOf(v))
  }

  def objectHashCode = super.hashCode

  override val hashCode = (predicate, args).hashCode

  val unificationCache = new java.util.HashMap[Atom, Option[List[EquivalenceClass]]]

  def getCachedUnificationResult(other: Atom) = {
    unificationCache.get(other)
  }

  def unify(other: Atom): Option[List[EquivalenceClass]] = {
    unifyInternal(other)
    //		val result = getCachedUnificationResult(other)
    //		if(result != null){
    ////			CacheStats.hit
    //			result
    //		}else {
    //			val result2 = other.getCachedUnificationResult(this)
    //			if(result2 != null){
    ////				CacheStats.hit
    //				result2
    //			}else {
    ////				CacheStats.miss
    //				val newResult = unifyInternal(other)
    //				unificationCache.put(other, newResult)
    //				newResult
    //			}
    //		}
  }

  /**
   * Get the MGU of this atom with another atom.
   */
  private[this] def unifyInternal(other: Atom): Option[List[EquivalenceClass]] = {
    if (predicate != other.predicate) None
    else {
      var finalEquivalenceClasses: List[EquivalenceClass] = Nil
      var tempEquivalenceClasses = (args zip other.args).map { case (l, r) => new EquivalenceClass(l, r) }.toList
      // filter singleton classes, like when two constants appear in the same position
      tempEquivalenceClasses = tempEquivalenceClasses.filter { _.size > 1 }
      if (tempEquivalenceClasses.exists { eqClass =>
        eqClass.isInconsistent
      }) {
        None
      } else {
        while (tempEquivalenceClasses.nonEmpty) {
          var currentEquivalenceClass = tempEquivalenceClasses.head
          tempEquivalenceClasses = tempEquivalenceClasses.tail
          var changed = true;
          while (changed) {
            val (overlapping, disjoint) = tempEquivalenceClasses.partition(_.variablesOverlap(currentEquivalenceClass))
            currentEquivalenceClass = overlapping.foldLeft(currentEquivalenceClass) { _.join(_) }
            tempEquivalenceClasses = disjoint
            changed = overlapping.nonEmpty
          }
          if (currentEquivalenceClass.isInconsistent) {
            return None
          }
          finalEquivalenceClasses = currentEquivalenceClass :: finalEquivalenceClasses
        }
        Some(finalEquivalenceClasses)
      }
    }
  }

  def unifies(
    atom: Atom,
    atomConstrs: Constraints,
    thisConstrs: Constraints) = {
    unifyConstrained(atom, atomConstrs, thisConstrs).nonEmpty
  }

  /**
   * Get the MGU of this atom with another atom under the given constraints.
   *
   * @param atomConstrs         Constraints
   * @param thisConstrs         Constraints
   *
   * @todo Why is there a case where thisIneqs and atomIneqs is still used
   *       when giving ineqConstrOption?
   */
  def unifyConstrained(
    atom: Atom,
    atomConstrs: Constraints,
    thisConstrs: Constraints): Option[List[EquivalenceClass]] = {
    unify(atom) match {
      case Some(eqClasses) =>
        val constrs = thisConstrs.join(atomConstrs)
        if (constrs.conflictsWith(eqClasses)) None
        else {
          // verify some invariants
          assume(eqClasses.forall { eqClass =>
            if (eqClass.exists { _.isInstanceOf[Constant] }) {
              val allDomains = constrs.domainsFor(eqClass.variables)
              val check = allDomains.forall { _.isInstanceOf[RootDomain] }
              if(!check){
                println("bug")
              }
              check
            } else true
          }, "If an equality class.contains(_) a constant, the domains of the other variables should be root.")
          assume(eqClasses.forall { eqClass =>
            val otherConstants = constrs.differentConstants(eqClass)
            val allDomains = constrs.domainsFor(eqClass.variables)
            val correct = (
              allDomains.forall { domain =>
                (domain.isInstanceOf[RootDomain] ||
                  otherConstants.forall(domain.asInstanceOf[SubDomain].excludedConstants.contains(_)))
              }
              ||
              (thisConstrs.differentConstants(eqClass)
                == atomConstrs.differentConstants(eqClass)))
            //                        if (!correct) {
            //                            // find out what's wrong
            //                            val badDomain = allDomains.find { domain =>
            //                                !(
            //                                    domain.isInstanceOf[RootDomain] ||
            //                                    otherConstants.forall(domain.asInstanceOf[SubDomain].excludedConstants.contains(_)))
            //                            }
            //                            println(badDomain);
            //                            println(thisConstrs.differentConstants(eqClass) +" == "+atomConstrs.differentConstants(eqClass));
            //                            println("BUG")
            //                        }
            correct
          }, "If an equality class.contains(_) a inequality constraint on a constant, " +
            "1) the domains of the other variables should be root or exclude the constant" +
            "2) the domains do not exclude the constant but there is an inequality constraint for every variable of the domain and the constant")
          Some(eqClasses)
        }
      case None => None
    }
  }

  def getInternalShatteringMgu(
    atom: Atom,
    atomConstrs: Constraints,
    thisConstrs: Constraints) = {
    val mguOption = unifyConstrained(atom, atomConstrs, thisConstrs)
    mguOption match {
      case Some(eqClasses) => {
        eqClasses.find { eqClass =>
          val constr = atomConstrs join thisConstrs
          val allDomains = constr.domainsFor(eqClass.variables)
          allDomains.toSet.size > 1
        }
      }
      case None => None
    }
  }

  /**
   * Two atoms need shattering when either
   * - there is a equivalence class that.contains(_) a variable from this atom and a constant
   * - there is a class that.contains(_) 2 variables from this atom
   * - there is a constraint between a class in the atom and a constant that is not present in the literal constraints.
   * - there is a constraint between two classes in the atom that is not present in the literal constraints, and which is not trivial given the domains.
   */
  def needsNormalShattering(
    atom: Atom,
    eqClasses: List[EquivalenceClass],
    atomConstrs: Constraints,
    thisConstrs: Constraints) = (
    //* - there is a class that.contains(_) a variable from the literal and a constant
    eqClasses.exists { eqClass => eqClass.hasVariableFrom(this.variables) && eqClass.hasConstant }
    //* - there is a class that.contains(_) 2 variables from the literal
    || eqClasses.exists { eqClass => eqClass.hasEqualityBetween(this.variables) }
    //* - there is a constraint between a class in the atom and a constant that is not present in the literal constraints.
    || eqClasses.exists { eqClass =>
      val projectedThisEqClass = eqClass.project(this.variables)
      if (projectedThisEqClass.exists { _.isInstanceOf[Var] }) {
        val projectedOtherEqClass = eqClass.project(atom.variables)
        val differentOtherConstants = atomConstrs.differentConstants(projectedOtherEqClass)
        val differentThisConstants = thisConstrs.differentConstants(projectedThisEqClass)
        differentOtherConstants.exists { !differentThisConstants.contains(_) }
      } else false
    }
    //* - there is a constraint between two classes in the atom that is not present in the literal constraints, and which is not trivial given the domains.
    || {
      val eqClassesVars = eqClasses.map { _.removeConstants }
      eqClassesVars.exists { eqClass1 =>
        val diffOther = atomConstrs.differentVars(eqClass1)
        val diffThis = thisConstrs.differentVars(eqClass1)
        eqClassesVars.exists { eqClass2 =>
          if (eqClass1.hashCode < eqClass2.hashCode) {
            (eqClass2.exists { v: Term => diffOther.contains(v.asInstanceOf[Var]) }
              && !eqClass2.exists { v: Term => diffThis.contains(v.asInstanceOf[Var]) }
              && {
                // the inequality constraint's negation cannot be impossible given the domains
                eqClass1.exists { eq1Var =>
                  (
                    this.variables(eq1Var.asInstanceOf[Var]) &&
                    {
                      val dom1 = thisConstrs.domainFor(eq1Var.asInstanceOf[Var])
                      eqClass2.exists { eq2Var =>
                        (
                          this.variables(eq2Var.asInstanceOf[Var]) &&
                          !dom1.disjoint(thisConstrs.domainFor(eq2Var.asInstanceOf[Var])))
                      }
                    })
                }
              } //not possible?
              //						 				&& !((new EquivalenceClass(eqClass1)).project(atom.variables).sharedDomain(atomElems) 
              //						 					disjoint (new EquivalenceClass(eqClass2)).project(atom.variables).sharedDomain(atomElems))
              )
          } else false
        }
      }
    })

  def getShatteringMgu(
    atom: Atom,
    atomConstrs: Constraints,
    thisConstrs: Constraints) = {
    val mguOption = unifyConstrained(atom, atomConstrs, thisConstrs)
    mguOption match {
      case Some(eqClasses) =>
        val shattering = needsNormalShattering(atom, eqClasses, atomConstrs, thisConstrs)
        if (shattering) {
          // assumption incorrect, because during smoothing, one might subtract f(X,Y),X!=Y,X\inD_1 from f(X,Y)
          //						assume(!eqClasses.exists{ eqClass => 
          //							val eqClassVarsAtom = eqClass.variables intersect atom.variables
          //							val eqClassVarsThis = eqClass.variables intersect this.variables
          //							if(eqClassVarsAtom.isEmpty || eqClassVarsThis.isEmpty) {
          //							    false
          //							}else{
          //							 	val atomSubDomain = eqClass.project(atom.variables).sharedDomain(atomElems)
          //							 	val thisDomains = thisElems.domains(eqClassVarsThis)
          //							 	if(thisDomains.exists{_.superDomain(atomSubDomain)}){
          //							 	    println("DEBUG")
          //							 	}
          //							 	thisDomains.exists{_.superDomain(atomSubDomain)}
          //							}
          //						})
          // assumption incorrect, because during smoothing, one might subtract f(X,Y),X!=Y,X\inD_1 from f(X,Y)
          //						assume(eqClasses.forall{eqClass => 
          //							val allElemConstrs = atomElems join thisElems
          //							val allDomains = eqClass.variables.map(allElemConstrs(_))
          //							if(!allDomains.forall{_.isInstanceOf[RootDomain]}){
          //							    println("DEBUG")
          //							}
          //							allDomains.forall{_.isInstanceOf[RootDomain]}
          //						}, "If inequality shattering is needed, the domains of the variables should be root.")

          // next assumption is removed because now domains are intersected when variables are unified
          //					    // assume domains are equal
          //					    assume(eqClasses.forall{eqClass => 
          //							val allElemConstrs = atomElems join thisElems
          //							val allDomains = eqClass.variables.map(allElemConstrs(_))
          //							if(allDomains.toSet.size != 1){
          //							    println("DEBUG")
          //							}
          //							allDomains.toSet.size == 1
          //						}, "If inequality shattering is needed, the domains of the variables should be identical. This is required because users of this method assume that. They don't merge different domains.")
          Some(eqClasses)
        } else None
      case None => None
    }
  }

  /**
   * Two atoms need domain shattering when either
   * - the domain of a var in this atom is a superset of the domain of a var in the other atom in the same equivalence class
   */
  def getDomainShatteringMgu(
    atom: Atom,
    atomConstrs: Constraints,
    thisConstrs: Constraints) = {
    val mguOption = unifyConstrained(atom, atomConstrs, thisConstrs)
    mguOption match {
      case Some(eqClasses) =>
        assume(!needsNormalShattering(atom, eqClasses, atomConstrs, thisConstrs), "Cannot shatter domains when normal shattering is needed first.")
        val shattering = ({
          eqClasses.exists { eqClass =>
            val atomSubDomain = atomConstrs.elemConstrs.sharedDomain(eqClass.project(atom.variables))
            val thisDomains = thisConstrs.domainsFor(eqClass.variables intersect this.variables)
            thisDomains.exists { _.superDomain(atomSubDomain) }
          }
        })
        if (shattering) {
          // this assumption no longer holds, because you can do domain shattering for f(X,X) w.r.t. f(Z,Y), when Z and Y are in a subdomain
          //assume(eqClasses.forall { _.size == 2 }, "After inequality shattering, equivalence classes should always be over 2 variables. Because inequality shattering introduces inequality constraints to enforce this.")
          Some(eqClasses)
        } else None
      case None => None
    }
  }

  def substitute(substitution: Var.Substitution) = {
    new Atom(predicate, args.map {
      _ match {
        case v: Var => v.substitute(substitution)
        case c: Constant => c
      }
    }: _*)
  }

  def inverseSubstitution(c: Constant, v: Var): Atom = {
    new Atom(predicate, args.map {
      _ match {
        case `c` => v
        case t => t
      }
    }: _*)
  }

  def toLatex(nameSpace: NameSpace[Var, String]) = {
    var lastUsedVar = -1;
    val argStrings = for (arg <- args) yield {
      arg match {
        case variable: Var => nameSpace.getName(variable)
        case _: Constant => arg.toString
      }
    }
    if (predicate.arity == 0) """\operatorname{""" + predicate + "}"
    else """\operatorname{""" + predicate + "}" + argStrings.mkString("(", ",", ")")
  }

  // small performance improvement when caching string
  override lazy val toString: String = toString(ToStringNameSpace)

  def toString(nameSpace: NameSpace[Any, String], sign: Boolean = true) = {
    var lastUsedVar = -1;
    val argStrings = for (arg <- args) yield {
      nameSpace.getName(arg)
    }
    val signStr = if (sign) "" else "¬"
    if (predicate.arity == 0) signStr + nameSpace.getName(predicate)
    else signStr + predicate + (if (argStrings.nonEmpty) argStrings.mkString("(", ",", ")") else "")
  }

}
