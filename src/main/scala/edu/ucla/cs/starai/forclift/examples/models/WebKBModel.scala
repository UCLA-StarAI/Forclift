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

class WebKBModel(
  nbPages: Int,
  knownPages: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends FactorGraphModel {

  def theoryString = (
    "domain Word 3 {exam,course,grade}\n" +
    "domain Class 2 {professor,student}\n" +
    "domain Page " + nbPages + knownPages.mkString(" {", ",", "}") + "\n" +
    """
predicate has(Page,Word)
predicate pageclass(Page,Class)
predicate linked(Page,Page)

!has(P,exam) v pageclass(P,student) 1.2 1
!has(P,course) v pageclass(P,student) 1.3 1
!has(P,grade) v pageclass(P,student) 1.4 1
!has(P,exam) v pageclass(P,professor) 1.4 1
!has(P,course) v pageclass(P,professor) 1.3 1
!has(P,grade) v pageclass(P,professor) 1.2 1

!pageclass(P,student) v !linked(P,Q) v pageclass(Q,student) 1.2 1
!pageclass(P,student) v !linked(P,Q) v pageclass(Q,professor) 1.1 1
!pageclass(P,professor) v !linked(P,Q) v pageclass(Q,student) 1.2 1
!pageclass(P,professor) v !linked(P,Q) v pageclass(Q,professor) 1.5 1
"""
    + evidence.mkString("\n"))

}
