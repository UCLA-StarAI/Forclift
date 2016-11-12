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

/*
domain person 5

predicate Friends_{f_{3}(X,Y),Y≠X<0,1>}(person,person) 1.0 1.0
predicate Smokes(person) 1.0 1.0
predicate \theta_{Friends_{f_{3}(X,X)<0,0>}(X)==Friends(X,X)}(person) 0.15824140103759082 0.8417585989624091
predicate \theta_{Smokes(X)==Smokes_2(X,Y), X≠Y}(person) 0.1903618583920823 0.8096381416079177
predicate \theta_{Friends_{f_{3}(X,Y),Y≠X<0,1>}(X,Y), X≠Y==Friends(X,Y), X≠Y}(person,person) 0.22139601529222863 0.7786039847077714
predicate \theta_{Cancer_{f_{2}(X)}(X)==Cancer(X)}(person) 0.5061038467534594 0.4938961532465405
predicate f_{5}(person) 7.38905609893065 1.0
predicate \theta_{Smokes_{f_{5}(X)}(X)==Smokes(X)}(person) 0.018256217044425465 0.9817437829555746
predicate \theta_{Cancer(X)==Cancer_{f_{2}(X)}(X)}(person) 0.1545059859158492 0.8454940140841507
predicate \theta_{Smokes_{f_{1}(X)}(X)==Smokes(X)}(person) 0.017893641934668792 0.9821063580653312
predicate Smokes_1(person,person) 1.0 1.0
predicate \theta_{Smokes(X)==Smokes_{f_{5}(X)}(X)}(person) 0.2157582239896709 0.7842417760103291
predicate \theta_{Cancer(X)==Cancer_{f_{5}(X)}(X)}(person) 0.5045926649169761 0.495407335083024
predicate Smokes_{f_{1}(X)}(person) 1.0 1.0
predicate Smokes_{f_{4}(X,X)}(person) 1.0 1.0
predicate theta_3(person,person) 0.024201427434700842 0.9757985725652991
predicate \theta_{Cancer_{f_{5}(X)}(X)==Cancer(X)}(person) 0.1548906647066956 0.8451093352933043
predicate \theta_{Friends(X,X)==Friends_{f_{4}(X,X)<0,0>}(X)}(person,person) 0.15735334329925366 0.8426466567007465
predicate \theta_{Friends_{f_{4}(X,X)<0,0>}(X)==Friends(X,X)}(person) 0.05541817781204304 0.9445818221879569
predicate Cancer_{f_{5}(X)}(person) 1.0 1.0
predicate f_{3}(person,person) 20.085536923187668 1.0
predicate f_{1}(person) 3.3201169227365472 1.0
predicate Friends_{f_{4}(X,X)<0,0>}(person) 1.0 1.0
predicate f_{4}(person,person) 4.0551999668446745 1.0
predicate \theta_{Smokes(X)==Smokes_{f_{4}(X,X)}(X)}(person) 0.5 0.5
predicate theta_2(person,person) 0.022639774517592998 0.977360225482407
predicate \theta_{Friends(X,X)==Friends_{f_{3}(X,X)<0,0>}(X)}(person,person) 0.04848811589514701 0.951511884104853
predicate f_{2}(person) 5.473947391727199 1.0
predicate Cancer_{f_{2}(X)}(person) 1.0 1.0
predicate Cancer(person) 1.0 1.0
predicate Friends_1(person,person) 1.0 1.0
predicate \theta_{Smokes_{f_{4}(X,X)}(X)==Smokes(X)}(person) 0.004925390959306842 0.9950746090406931
predicate \theta_{Smokes(X)==Smokes_1(X,Y), X≠Y}(person) 0.1947457114770916 0.8052542885229083
predicate Friends(person,person) 1.0 1.0
predicate \theta_{Smokes(X)==Smokes_{f_{1}(X)}(X)}(person) 0.23197879630029752 0.7680212036997026
predicate Smokes_{f_{5}(X)}(person) 1.0 1.0
predicate theta_1(person,person) 0.003827671862274425 0.9961723281377256
predicate Smokes_2(person,person) 1.0 1.0
predicate Friends_{f_{3}(X,X)<0,0>}(person) 1.0 1.0

Friends(X,Y) ∨ ¬Friends_1(Y,X), Y≠X
Friends(X,Y) ∨ ¬theta_1(X,Y), Y≠X
Friends_1(X,Y) ∨ ¬Friends(Y,X), Y≠X
Smokes_1(X,Y) ∨ ¬theta_2(X,Y), Y≠X
Smokes_1(X,Y) ∨ ¬f_{4}(X,Y) ∨ ¬Smokes_2(X,Y) ∨ ¬Friends_1(X,Y), Y≠X
Smokes_2(X,Y) ∨ ¬theta_3(X,Y), Y≠X
theta_1(X,Y) ∨ ¬Friends(X,Y), Y≠X
theta_2(X,Y) ∨ ¬Smokes_1(X,Y), Y≠X
theta_3(X,Y) ∨ ¬Smokes_2(X,Y), Y≠X
f_{4}(X,Y) ∨ Friends_1(X,Y), X≠Y
f_{4}(X,Y) ∨ Smokes_2(X,Y), Y≠X
f_{4}(X,Y) ∨ ¬Smokes_1(X,Y), X≠Y
*/

/* 
 * Copyright (C) 2011 Guy Van den Broeck (guy.vandenbroeck@cs.kuleuven.be)
 * 
 * This file is part of WFOMC (http://dtai.cs.kuleuven.be/ml/systems/wfomc).
 *
 * WFOMC is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * WFOMC is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with WFOMC.  If not, see <http://www.gnu.org/licenses/>.
 * 
 */

package edu.ucla.cs.starai.forclift.bugs

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.Matchers
import org.scalatest.Spec

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models._

@RunWith(classOf[JUnitRunner])
class TestBug3 extends ModelBehaviours {

  describe("Bug3Model of size 3") {
    val correctLogWMC = 13.16979643063896 +- 0.00001
    val model = new Bug3Model(3)
    it should behave like verySmallModel(model, correctLogWMC)
  }
  describe("Bug3Model of size 4") {
    val correctLogWMC = 20.79441541679836 +- 0.00001
    val model = new Bug3Model(4)
    it should behave like verySmallModel(model, correctLogWMC)
  }
  describe("Bug3Model of size 5") {
    val correctLogWMC = 29.805328764077643 +- 0.00001
    val model = new Bug3Model(5)
    it should behave like verySmallModel(model, correctLogWMC)
  }

}
