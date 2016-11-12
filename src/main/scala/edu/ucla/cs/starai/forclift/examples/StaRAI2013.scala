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

package edu.ucla.cs.starai.forclift.examples

import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.examples.models.MLNModel
import nnf._

object StaRAI2013 {

  def main(args: Array[String]): Unit = {

    for (k <- 0 to 100) {

      //      val model = new MLNModel {
      //
      //    	  def theoryString = (
      //"page = " + (1 to 20).map { "P" + _ }.mkString("{", ", ", "}") + """
      //Linked(page,page)
      //Positive(page)
      //""" + (1 to k).map {i => "Q"+i+"(page)\nR"+i+"(page)"}.mkString("", "\n", "") + """
      //1.1 Positive(p1) ^ Linked(p1,p2) => Positive(p2)
      //""" +
      //(if(k>0) (1 to k).map {i => 
      //  "( Q"+i+"(p1)"+" ^ R"+i+"(p2) )"
      //  }.mkString("( ", " v ", " ) <=> Linked(p1,p2).") 
      //  else "")
      //    		)
      //
      //      }
      //      
      val model = new MLNModel {

        def theoryString = (
          "page = " + (1 to 10).map { "P" + _ }.mkString("{", ", ", "}") + """
Linked(page,page)
ProfPage(page)
CoursePage(page)
""" + (1 to k).map { i => "Q" + i + "(page)\nR" + i + "(page)" }.mkString("", "\n", "") + """
1.1 ProfPage(p1) ^ Linked(p1,p2) => CoursePage(p2)
""" +
          (if (k > 0) (1 to k).map { i =>
            "( Q" + i + "(p1)" + " ^ R" + i + "(p2) )"
          }.mkString("( ", " v ", " ) <=> Linked(p1,p2).")
          else ""))

      }
      ////      
      //      
      //      val model = new MLNModel {
      //
      //    	  def theoryString = (
      //"student = " + (1 to 20).map { "S" + _ }.mkString("{", ", ", "}") + """
      //professor = """ + (1 to 20).map { "P" + _ }.mkString("{", ", ", "}") + """
      //Advises(professor,student)
      //Famous(professor)
      //Success(student)
      //""" + (1 to k).map {i => "Q"+i+"(professor)\nR"+i+"(student)"}.mkString("", "\n", "") + """
      //1.1 Famous(p) ^ Advises(p,s) => Success(s)
      //""" +
      //(if(k>0) (1 to k).map {i => 
      //  "( Q"+i+"(p)"+" ^ R"+i+"(s) )"
      //  }.mkString("( ", " v ", " ) <=> Advises(p,s).") 
      //  else "")
      //    		)
      //
      //      }

      println();
      println("K=" + k);
      println("Model=");
      println(model.theoryString);
      println("CNF=");
      println(model.theory);

      //      NNFNode.nbNodes = 0;
      val nnf = model.theory.nnf
      println("Size: " + model.theory.nnf.size);
      println("EvalOrder: " + model.theory.nnf.evalOrder);
      //      println("WMC: "+model.theory.logWmcDP);
      //      model.theory.showNnfPdf(true, 100, "aistats",false);

    }

  }
}
