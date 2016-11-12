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

import edu.ucla.cs.starai.forclift._
import util._
import util.extracollection._

final class EquivalenceClass(
  final val members: Set[Term]) extends SetProxy[Term] {

  def this(elms: Term*) = this(elms.toSet)

  def this(elms: Iterable[Term]) = this(elms.toSet)

  val self = members

  def variablesOverlap(other: EquivalenceClass) = {
    // only count overlapping variables. 
    // overlapping constants might come from distinct domains, we don't want to merge those equivalence classes!
    // consistency with inequality constraints between constants is checked when unifying with constraints.
    members.exists { m => m.isInstanceOf[Var] && other.members.contains(m) }
  }

  def join(other: EquivalenceClass) = {
    new EquivalenceClass(members union other.members)
  }

  def removeConstants: EquivalenceClass = new EquivalenceClass(filter { _.isInstanceOf[Var] })

  //todo with other given constraints
  @inline def isInconsistent = {
    members.exists { e1 =>
      e1.isInstanceOf[Constant] && members.exists { e2 =>
        e2.isInstanceOf[Constant] && e1 != e2
      }
    }
  }

  def isSingleton = members.size == 1

  def variables = members.collect { case variable: Var => variable }
  def constants = members.collect { case c: Constant => c }

  def hasConstant = members.exists { _.isInstanceOf[Constant] }
  def hasVariable = members.exists { _.isInstanceOf[Var] }

  def hasVariableFrom(variables: Set[Var]) = {
    members.exists { a => a.isInstanceOf[Var] && variables(a.asInstanceOf[Var]) }
  }

  def hasEqualityBetween(variables: Set[Var]) = {
    members.exists { a =>
      a.isInstanceOf[Var] && variables(a.asInstanceOf[Var]) && members.exists { a2 =>
        a2.isInstanceOf[Var] && a2 != a && variables(a2.asInstanceOf[Var])
      }
    }
  }

  def project(vars: Set[Var]) = {
    new EquivalenceClass(members.filter(_ match {
      case v: Var => vars(v)
      case c: Constant => true
    }))
  }

  def reify: (Term, Set[Var]) = {
    val res = members.find { _.isInstanceOf[Constant] }.getOrElse { new Var }
    val set = members.filter { _ ne res }
    assume(set.forall { _.isInstanceOf[Var] })
    (res, set.map { _.asInstanceOf[Var] })
  }

  def equalitySet: Set[(Var, Term)] = {
    val rightSide = members.find { _.isInstanceOf[Constant] }.getOrElse { members.head }
    members.filterNot(_ eq rightSide).map { leftSide =>
      (leftSide.asInstanceOf[Var], rightSide)
    }
  }

}
