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

package edu.ucla.cs.starai.forclift.nnf.visitors

import scala.language.implicitConversions
import edu.ucla.cs.starai.forclift.nnf._

abstract class NnfVisitor[I,O] {

  def visit(node: NNFNode, input: I): O = node match{
      
      // Leaf Nodes
      case leaf: UnitLeaf => visitUnitLeaf(leaf, input)
      case leaf: SmoothingNode => visitSmoothingNode(leaf, input)
      case leaf: ContradictionLeaf => visitContradictionLeaf(leaf, input)
      case TrueNode => visitTrue(input)
      case FalseNode => visitFalse(input)
      case leaf: GroundingNode => visitGroundingNode(leaf, input)
      
      // Complex Nodes
      case and: And => visitAndNode(and, input)
      case or: Or => visitOrNode(or, input)
      case ref: Ref => visitRefNode(ref, input)
      case ie: InclusionExclusion => visitInclusionExclusionNode(ie, input)

      // First-Order Nodes
      case forall: IndependentPartialGroundingNode => visitForallNode(forall, input)
      case exists: CountingNode => visitExists(exists, input)
      case dr: DomainRecursionNode => visitDomainRecursion(dr, input)

      case _ => throw new IllegalArgumentException
    }

  protected def visitDomainRecursion(dr: DomainRecursionNode, input: I): O
  protected def visitExists(exists: CountingNode, input: I): O
  protected def visitForallNode(forall: IndependentPartialGroundingNode, input: I): O
  protected def visitInclusionExclusionNode(ie: InclusionExclusion, input: I): O
  protected def visitOrNode(or: Or, input: I): O
  protected def visitAndNode(and: And, input: I): O
  protected def visitRefNode(ref: Ref, input: I): O
  protected def visitSmoothingNode(leaf: SmoothingNode, input: I): O
  protected def visitContradictionLeaf(leaf: ContradictionLeaf, input: I): O
  protected def visitUnitLeaf(leaf: UnitLeaf, input: I): O
  protected def visitGroundingNode(leaf: GroundingNode, input: I): O
  protected def visitFalse(input: I): O
  protected def visitTrue(input: I): O 
  
}
