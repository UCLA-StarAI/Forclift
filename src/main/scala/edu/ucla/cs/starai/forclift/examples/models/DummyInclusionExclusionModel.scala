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

class DummyInclusionExclusionModel(
  domainSize: Int,
  knownElements: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends WeightedCNFModel {

  def theoryString = (
    "domain D " + domainSize + knownElements.mkString(" {", ",", "}") + """

predicate p(D,D) 1.2 1
predicate q(D,D) 1.3 1
predicate r(D) 1.4 1

p(X,Y) v r(X) v q(A,B) v r(B)
"""
    + evidence.mkString("\n"))
}
