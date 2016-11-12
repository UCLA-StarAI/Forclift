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

package edu.ucla.cs.starai.forclift.constraints

import collection._
import scala.language.implicitConversions 

import edu.ucla.cs.starai.forclift._
import util._
import util.extracollection._

//TODO move to Var
final class ElemConstr(
  final val self: Map[Var, Domain] = Map()) extends MapProxy[Var, Domain] {

  def variables = keySet

  lazy val domains = values.toSet

  override lazy val hashCode = super.hashCode

  def sharedDomain(eqClass: EquivalenceClass): Domain = {
    domains(eqClass.variables).reduceLeft { _ intersect _ }
  }

  //	def sharedDomain(constrs: Constraints): Domain = {
  //	  sharedDomain(constrs.elemConstrs)
  //	}

  def conflictsWith(eqClasses: List[EquivalenceClass]): Boolean = {
    for (eqClass <- eqClasses) {
      val variableSharedDomain = sharedDomain(eqClass)
      if (variableSharedDomain == EmptyDomain
        || eqClass.constants.exists {
          _.domain.exists { _.disjoint(variableSharedDomain) }
        }) {
        return true;
      }
    }
    return false;
  }

  def substitute(substitution: Var.Substitution): ElemConstr = {
    val clone = mutable.Map.empty[Var, Domain]
    //substitute keys
    for ((term, domain) <- this) {
      term.substitute(substitution) match {
        case subsVar: Var => {
          //assume(!clone.contains(subsVar) || clone(subsVar) == domain, "Cannot merge two variables with different domains.")
          if (clone.contains(subsVar) && clone(subsVar) != domain) {
            //take the intersection when two elem constraints are merged
            clone += (subsVar -> clone(subsVar).intersect(domain))
          } else clone += (subsVar -> domain)
        }
        case subsConstant: Constant => //no op
      }
    }
    new ElemConstr(clone.toMap)
  }

  def mapDomains(f: Domain => Domain): ElemConstr = {
    new ElemConstr(self.mapValues(f))
  }

  //	def shatterDomain(from: Domain, to1: Domain, to2: Domain): List[ElemConstr] = {
  //		find{case (variable,domain) => (domain == from)}.map{ case (variable,domain) =>
  //			val this1 = new ElemConstr(this + (variable->to1))
  //			val this2 = new ElemConstr(this + (variable->to2))
  //			this1.shatterDomain(from, to1, to2) ::: this2.shatterDomain(from, to1, to2)
  //		}.getOrElse(List(this))
  //	}

  def project(variables: Set[Var]): ElemConstr = {
    new ElemConstr(self.filterKeys(variables(_)).toMap)
  }

  def join(other: ElemConstr): ElemConstr = {
    if (this eq other) this
    else {
      //assume(!this.exists{case (k,_) => other.contains(k)}) // commented because of shattering with other atom in same clause
      val joinedMaps: Map[Var, Domain] = this ++ other
      new ElemConstr(joinedMaps)
    }
  }

  def domains(variables: Set[Var]): Set[Domain] = {
    variables.map { this(_) }
  }

  def +(kv: (Var, Domain)): ElemConstr = this + (kv._1, kv._2)

  def +(variable: Var, domain: Domain): ElemConstr = {
    new ElemConstr(self + (variable -> domain))
  }

  override def toString = toString(ToStringNameSpace)

  def toString(nameSpace: NameSpace[Var, String], elemSymbol: String = " ∈ ", showRootDomains: Boolean = false) = {
    filter(showRootDomains || !_._2.isInstanceOf[RootDomain]).map {
      case (v, domain) =>
        val vName = nameSpace.getName(v)
        vName + elemSymbol + domain
    }.toArray.sorted.mkString(", ")
  }

}

object ElemConstr {

  val empty: ElemConstr = new ElemConstr

  implicit def map2ElemConstr(map: Map[Var, Domain]): ElemConstr = {
    new ElemConstr(map)
  }

  def apply(elems: (Var, Domain)*): ElemConstr = {
    val map = elems.toMap
    new ElemConstr(map)
  }

}
