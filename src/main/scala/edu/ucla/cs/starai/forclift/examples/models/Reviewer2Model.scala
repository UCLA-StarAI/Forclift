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

class Reviewer2Model(
  domainSize: Int,
  knownElements: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends WeightedCNFModel {

  def theoryString = (
    "domain D " + domainSize + knownElements.mkString(" {", ",", "}") + "\n" +
    """
predicate p(D) 1 1
predicate q(D) 1 1
predicate f1(D) 0.7 1
predicate f2(D,D) 0.3 1

p(X) v q(X) v !f1(X)
!p(X) v f1(X)
!q(X) v f1(X)

p(X) v q(Y) v !f2(X,Y), X!=Y
!p(X) v !f2(X,Y), X!=Y
!q(Y) v !f2(X,Y), X!=Y
"""
    + evidence.mkString("\n"))

}
