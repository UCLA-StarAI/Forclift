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

//Arguments
sealed trait Term

sealed /*not case*/ class Var( /*domain: Domain[T]*/ ) extends Term {

  override def toString = "X" + (hashCode % 10000)

  def toString(id: Int): String = {
    var name = 'X' + id
    if (name > 'Z') {
      name = name - 'Z' + 'A'
    }
    name.toChar.toString
  }

  def substitute(s: Var.Substitution) = s(this)

}


object Var {
  type Substitution = Var => Term
}


case class Constant(val value: Any) extends Term {

  private var _domain: Option[Domain] = None

  def setDomain(domain: Domain): Constant = {
    require(_domain == None)
    this._domain = Some(domain)
    this
  }
  
  override val hashCode = value.hashCode

  def domain = _domain

  //	override def toString = {
  //        if(domain.isEmpty) value.toString
  //        else value.toString + "_{"+domain.get+"}"
  //    }

  override def toString = value.toString

}

// TODO: What denkt guy van de verandering van Var naar Any voor namespaces
//       Ik heb meer algemene namespaces nodig om om te zetten naar correcte
//        alchemy syntax (alternatieve zijn, meerdere namespaces, syntax objects, ...
class VarNameSpace extends NameSpace[Any, String] {

  var lastUsedVar = -1;

  override protected def createName(obj: Any) = {
    obj match {
      case variable: Var => {
        lastUsedVar += 1
        variable.toString(lastUsedVar)
      }
      //case _ => throw new IllegalStateException("Namespace only supports variables")
      case _ => obj.toString
    }
  }
}
