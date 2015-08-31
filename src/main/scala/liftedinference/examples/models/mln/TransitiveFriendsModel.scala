/*
 * Copyright 2015 Guy Van den Broeck and Wannes Meert
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

package liftedinference.examples.models.mln

import liftedinference._
import examples.models._

class TransitiveFriendsModel(
  nbPeople: Int,
  knownPeople: Seq[String] = Nil,
  evidence: Seq[String] = Nil) extends MLNModel {

  def theoryString = (
    "person = " + (1 to nbPeople).map { "P" + _ }.mkString("{", ", ", "}") + """
Friends(person,person)
1.1 Friends(x,y) ^ Friends(y,z) => Friends(x,z)
""" + evidence.map { _ + "." }.mkString("\n"))
  //1000 Friends(x,y) ^ Friends(y,z) => Friends(x,z)
  //2 Friends(x,y) ^ Friends(y,z) => Friends(x,z)

}
