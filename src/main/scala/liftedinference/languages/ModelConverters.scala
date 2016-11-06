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

package liftedinference.languages

import liftedinference.inference.WeightedCNF
import liftedinference.languages.focnf.FOCNF
import liftedinference.languages.mln.MLN
import liftedinference.propositional.DimacsCNF
import liftedinference.propositional.DimacsCNF
import scala.language.implicitConversions

object ModelConverters{
  
  //TODO turn into a "pimp my library" pattern and take methods our of model classes
  
  implicit def modelToWeightedCNF(that: StatRelModel) : WeightedCNF = {
     that match{
       case wcnf: WeightedCNF => wcnf
       case mln: MLN => mln
       case _ => throw new UnsupportedOperationException
     }
  }
  
  implicit def mlnToWeightedCNF(model: MLN) : WeightedCNF = {
    if (model.getAlchemySemantics()) model.toWeightedCNF()
    else model.toWeightedCNFWithoutSplitting()
  }
  
  implicit def fgToWeightedCNF(model: FactorGraph): WeightedCNF = {
    model.toWeightedCNF()
  }
  
  implicit def focnfToWeightedCNF(model: FOCNF): WeightedCNF = {
    model.weightedCNF()
  }
    
}
