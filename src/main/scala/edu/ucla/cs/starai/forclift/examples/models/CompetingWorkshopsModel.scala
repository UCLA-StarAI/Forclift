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

class CompetingWorkshopsModel(
  nbPeople: Int,
  nbWorkshops: Int,
  knownPeople: Seq[String] = Nil,
  knownWorkshops: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends FactorGraphModel {

  def theoryString = (
    "domain Person " + nbPeople + knownPeople.mkString(" {", ",", "}") + "\n" +
    "domain Workshop " + nbWorkshops + knownWorkshops.mkString(" {", ",", "}") + "\n" +
    """
predicate series
predicate hot(Workshop)
predicate attends(Person)

hot(W) and attends(P) 0.2 0.8
attends(P) and series 0.501 0.499
"""
    + evidence.mkString("\n"))

}
