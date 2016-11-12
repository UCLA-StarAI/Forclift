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

package edu.ucla.cs.starai.forclift.examples.models

import edu.ucla.cs.starai.forclift._

class WorkshopAttributesModel(
  nbPeople: Int,
  knownPeople: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends FactorGraphModel {

  def theoryString = (
    "domain Person " + nbPeople + knownPeople.mkString(" {", ",", "}") + "\n" +
    """
predicate series
predicate attr1
predicate attr2
predicate attends(Person)

attends(P) and series 0.501 0.499
attends(P) and attr1 0.7 0.3
attends(P) and attr2 0.7 0.3
"""
    + evidence.mkString("\n"))

}

class Workshop6AttributesModel(
  nbPeople: Int,
  knownPeople: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends FactorGraphModel {

  def theoryString = (
    "domain Person " + nbPeople + knownPeople.mkString(" {", ",", "}") + "\n" +
    """
predicate series
predicate attr1
predicate attr2
predicate attr3
predicate attr4
predicate attr5
predicate attr6
predicate attends(Person)

attends(P) and series 0.501 0.499
attends(P) and attr1 0.51 0.49
attends(P) and attr2 0.51 0.49
attends(P) and attr3 0.505 0.495
attends(P) and attr4 0.505 0.495
attends(P) and attr5 0.501 0.499
attends(P) and attr6 0.501 0.499
"""
    + evidence.mkString("\n"))

}
