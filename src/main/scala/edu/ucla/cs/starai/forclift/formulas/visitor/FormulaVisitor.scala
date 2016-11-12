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

package edu.ucla.cs.starai.forclift.formulas.visitor

import edu.ucla.cs.starai.forclift._

abstract class FormulaVisitor[I,O] {

  def visit(formula: Formula, input: I) = formula match{
    case f: TrueFormula    => visitTrueFormula(f, input)
    case f: FalseFormula   => visitFalseFormula(f, input)
    case f: LiteralFormula => visitLiteralFormula(f, input)
    case f: NegFormula     => visitNegFormula(f, input)
    case f: ConjFormula    => visitConjFormula(f, input)
    case f: DisjFormula    => visitDisjFormula(f, input)
    case f: ImplFormula    => visitImplFormula(f, input)
    case f: EqFormula      => visitEqFormula(f, input)
    case f: ExistFormula   => visitExistFormula(f, input)
    case f: ForallFormula  => visitForallFormula(f, input)
    case f: EmptyFormula  => visitEmptyFormula(f, input)
    case f: WildNegFormula  => visitWildNegFormula(f, input)
    case f: AtomicExpFormula  => visitAtomicExpFormula(f, input)
    case f: IndivisibleSubformula  => visitIndivisibleSubformula(f, input)
  }
  
  protected def visitTrueFormula(formula: TrueFormula, input: I): O
  protected def visitFalseFormula(formula: FalseFormula, input: I): O
  protected def visitLiteralFormula(formula: LiteralFormula, input: I): O
  protected def visitNegFormula(formula: NegFormula, input: I): O
  protected def visitConjFormula(formula: ConjFormula, input: I): O
  protected def visitDisjFormula(formula: DisjFormula, input: I): O
  protected def visitImplFormula(formula: ImplFormula, input: I): O
  protected def visitEqFormula(formula: EqFormula, input: I): O
  protected def visitExistFormula(formula: ExistFormula, input: I): O
  protected def visitForallFormula(formula: ForallFormula, input: I): O
  protected def visitEmptyFormula(formula: EmptyFormula, input: I): O
  protected def visitWildNegFormula(formula: WildNegFormula, input: I): O
  protected def visitAtomicExpFormula(formula: AtomicExpFormula, input: I): O
  protected def visitIndivisibleSubformula(formula: IndivisibleSubformula, input: I): O
}
