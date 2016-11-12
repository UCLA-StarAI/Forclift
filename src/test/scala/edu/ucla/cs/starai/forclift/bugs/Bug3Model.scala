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

class Bug3Model(size: Int) extends WeightedCNFModel {

  def theoryString = (
    "domain Page " + size + " {p}" + """
predicate a(Page,Page) 1 1
predicate b(Page,Page) 1 1
predicate l(Page,Page) 1 1
a(X,Y) ∨ ¬l(X,Y), X≠Y, Y≠p
b(X,Y) ∨ ¬l(X,Y), Y≠X, Y≠p
l(X,Y) ∨ ¬a(X,Y), X≠Y, Y≠p
l(X,Y) ∨ ¬b(X,Y), X≠Y, Y≠p
""")

}
