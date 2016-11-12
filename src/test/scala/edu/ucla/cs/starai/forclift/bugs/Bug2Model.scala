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

package edu.ucla.cs.starai.forclift.bugs

import edu.ucla.cs.starai.forclift.examples.models._

class Bug2Model(
  domainSize: Int,
  knownElements: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends WeightedCNFModel {

  def theoryString = (
    "domain Person " + domainSize + knownElements.mkString(" {", ",", "}") + "\n" +
    """
predicate f4(Person,Person) 4.0551999668446745 1.0
predicate smokes(Person) 1.0 1.0
predicate smokes1(Person,Person) 1.0 1.0
predicate theta3(Person,Person) 0.024201427434700842 0.9757985725652991
predicate theta2(Person,Person) 0.022639774517592998 0.977360225482407
predicate friends1(Person,Person) 1.0 1.0
predicate friends(Person,Person) 1.0 1.0
predicate theta1(Person,Person) 0.003827671862274425 0.9961723281377256
predicate smokes2(Person,Person) 1.0 1.0

friends(X,Y) ∨ ¬friends1(Y,X), Y≠X
friends(X,Y) ∨ ¬theta1(X,Y), Y≠X
friends1(X,Y) ∨ ¬friends(Y,X), Y≠X
smokes1(X,Y) ∨ ¬theta2(X,Y), Y≠X
smokes1(X,Y) ∨ ¬f4(X,Y) ∨ ¬smokes2(X,Y) ∨ ¬friends1(X,Y), Y≠X
smokes2(X,Y) ∨ ¬theta3(X,Y), Y≠X
theta1(X,Y) ∨ ¬friends(X,Y), Y≠X
theta2(X,Y) ∨ ¬smokes1(X,Y), Y≠X
theta3(X,Y) ∨ ¬smokes2(X,Y), Y≠X
f4(X,Y) ∨ friends1(X,Y), X≠Y
f4(X,Y) ∨ smokes2(X,Y), Y≠X
f4(X,Y) ∨ ¬smokes1(X,Y), X≠Y
"""
    + evidence.mkString("\n"))

}
