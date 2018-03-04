package com.ot.bones.validation

import com.ot.bones.ToHList._

/** Aliases for simplifying the creation of ObjectFieldGroup. */
/** Convenient ways to declare Object specification */
trait ToHList {

  def obj2[A, AA, B, BB](op1: FieldDefinition[A, DataDefinitionOp[A]],
                         op2: FieldDefinition[B, DataDefinitionOp[B]]) = HList2(op1, op2)

  def obj3[A, B, C](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]]) =
    HList3(op1, op2, op3)

  def obj4[A, B, C, D](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]]) =
    HList4(op1, op2, op3, op4)

  def obj5[A, B, C, D, E](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]], op5: FieldDefinition[E, DataDefinitionOp[E]]) =
    HList5(op1, op2, op3, op4, op5)

  def obj6[A, B, C, D, E, F](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]], op5: FieldDefinition[E, DataDefinitionOp[E]],
                             op6: FieldDefinition[F, DataDefinitionOp[F]]) =
    HList6(op1, op2, op3, op4, op5, op6)

  def obj7[A, B, C, D, E, F, G](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]], op5: FieldDefinition[E, DataDefinitionOp[E]],
                                op6: FieldDefinition[F, DataDefinitionOp[F]], op7: FieldDefinition[G, DataDefinitionOp[G]]) =
    HList7(op1, op2, op3, op4, op5, op6, op7)

  def obj8[A, B, C, D, E, F, G, H](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]], op5: FieldDefinition[E, DataDefinitionOp[E]],
                                   op6: FieldDefinition[F, DataDefinitionOp[F]], op7: FieldDefinition[G, DataDefinitionOp[G]], op8: FieldDefinition[H, DataDefinitionOp[H]]) =
    HList8(op1, op2, op3, op4, op5, op6, op7, op8)

  def obj9[A, B, C, D, E, F, G, H, I](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]], op5: FieldDefinition[E, DataDefinitionOp[E]],
                                      op6: FieldDefinition[F, DataDefinitionOp[F]], op7: FieldDefinition[G, DataDefinitionOp[G]], op8: FieldDefinition[H, DataDefinitionOp[H]], op9: FieldDefinition[I, DataDefinitionOp[I]]) =
    HList9(op1, op2, op3, op4, op5, op6, op7, op8, op9)

  def obj10[A, B, C, D, E, F, G, H, I, J](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]], op5: FieldDefinition[E, DataDefinitionOp[E]],
                                          op6: FieldDefinition[F, DataDefinitionOp[F]], op7: FieldDefinition[G, DataDefinitionOp[G]], op8: FieldDefinition[H, DataDefinitionOp[H]], op9: FieldDefinition[I, DataDefinitionOp[I]], op10: FieldDefinition[J, DataDefinitionOp[J]]) =
    HList10(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)

  def obj11[A, B, C, D, E, F, G, H, I, J, K](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]], op5: FieldDefinition[E, DataDefinitionOp[E]],
                                             op6: FieldDefinition[F, DataDefinitionOp[F]], op7: FieldDefinition[G, DataDefinitionOp[G]], op8: FieldDefinition[H, DataDefinitionOp[H]], op9: FieldDefinition[I, DataDefinitionOp[I]], op10: FieldDefinition[J, DataDefinitionOp[J]],
                                             op11: FieldDefinition[K, DataDefinitionOp[K]]) =
    HList11(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)

  def obj12[A, B, C, D, E, F, G, H, I, J, K, L](op1: FieldDefinition[A, DataDefinitionOp[A]], op2: FieldDefinition[B, DataDefinitionOp[B]], op3: FieldDefinition[C, DataDefinitionOp[C]], op4: FieldDefinition[D, DataDefinitionOp[D]], op5: FieldDefinition[E, DataDefinitionOp[E]],
                                                op6: FieldDefinition[F, DataDefinitionOp[F]], op7: FieldDefinition[G, DataDefinitionOp[G]], op8: FieldDefinition[H, DataDefinitionOp[H]], op9: FieldDefinition[I, DataDefinitionOp[I]], op10: FieldDefinition[J, DataDefinitionOp[J]],
                                                op11: FieldDefinition[K, DataDefinitionOp[K]], op12: FieldDefinition[L, DataDefinitionOp[L]]) =
    HList12(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)



}


