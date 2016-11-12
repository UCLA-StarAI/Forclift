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

package edu.ucla.cs.starai.forclift.languages.mln

import java.io._
import scala.io._
import edu.ucla.cs.starai.forclift.languages.focnf.FOCNF
import edu.ucla.cs.starai.forclift._
import edu.ucla.cs.starai.forclift.inference._

case class FormatFOCNFAsMLN(
    focnf: FOCNF
) {
  lazy val mln = {
    val wformulas = focnf.formulas.map{formula =>
      WeightedFormula(formula, 0.0, true)
    }
    
    // Exhaustively add all groundings if one of the predicate weights is zero
    val atoms = focnf.atomWeights.map{case (atom,ws) => atom}.toSet
    val zero_pred = focnf.predicateWeights.filter{case (pred,ws) => ws.posWDouble == 0 || ws.negWDouble == 0}.toList
    val new_atomWeights = zero_pred.flatMap{case (pred,ws) =>
      val atom = pred((0 until pred.arity).map(i => new Var):_*)
      val ground_atoms = focnf.groundAtom(atom).toSet
      val new_atoms = ground_atoms -- atoms
      new_atoms.map((_,ws))
    }
    
    val atom_formulas = (focnf.atomWeights++new_atomWeights).flatMap{case (atom,ws) =>
      val default_ws = focnf.predicateWeights.get(atom.predicate) match {
        case Some(w) => w
        case None => Weights(1,1)
      }
      val pos_w = (if (default_ws.posWDouble == 0) ws.posWDouble else ws.posWDouble/default_ws.posWDouble)
      val neg_w = (if (default_ws.negWDouble == 0) ws.negWDouble else ws.negWDouble/default_ws.negWDouble)
      
      val pos_f = pos_w match {
        case 0 => WeightedFormula(NegFormula(LiteralFormula(atom)), 0, true) :: Nil
        case 1 => Nil
        case _ => WeightedFormula(LiteralFormula(atom), Math.log(pos_w)) :: Nil
      }
      val neg_f = neg_w match {
        case 0 => WeightedFormula(LiteralFormula(atom), 0, true) :: Nil
        case 1 => Nil
        case _ => WeightedFormula(NegFormula(LiteralFormula(atom)), Math.log(neg_w)) :: Nil
      }
      
      pos_f ::: neg_f
    }
    
    val new_pred_ws = (focnf.predicateWeights -- zero_pred.map(_._1))
    
    MLN(wformulas ::: atom_formulas,
        focnf.predicates,
        focnf.domainSizes,
        Nil,
        new_pred_ws)
  }
  
}
