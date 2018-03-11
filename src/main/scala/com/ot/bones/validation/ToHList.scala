package com.ot.bones.validation

import com.ot.bones.ToHList._

/** Aliases for simplifying the creation of ObjectFieldGroup. */
/** Convenient ways to declare Object specification */
trait ToHList {

  def obj2[A, AA, B, BB](op1: FieldDefinition[A],
                         op2: FieldDefinition[B]) = HList2(op1, op2)

  def obj3[A, B, C](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C]) =
    HList3(op1, op2, op3)

  def obj4[A, B, C, D](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D]) =
    HList4(op1, op2, op3, op4)

  def obj5[A, B, C, D, E](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E]) =
    HList5(op1, op2, op3, op4, op5)

  def obj6[A, B, C, D, E, F](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                             op6: FieldDefinition[F]) =
    HList6(op1, op2, op3, op4, op5, op6)

  def obj7[A, B, C, D, E, F, G](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                op6: FieldDefinition[F], op7: FieldDefinition[G]) =
    HList7(op1, op2, op3, op4, op5, op6, op7)

  def obj8[A, B, C, D, E, F, G, H](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                   op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H]) =
    HList8(op1, op2, op3, op4, op5, op6, op7, op8)

  def obj9[A, B, C, D, E, F, G, H, I](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                      op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I]) =
    HList9(op1, op2, op3, op4, op5, op6, op7, op8, op9)

  def obj10[A, B, C, D, E, F, G, H, I, J](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                          op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J]) =
    HList10(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10)

  def obj11[A, B, C, D, E, F, G, H, I, J, K](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                             op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J],
                                             op11: FieldDefinition[K]) =
    HList11(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11)

  def obj12[A, B, C, D, E, F, G, H, I, J, K, L](op1: FieldDefinition[A], op2: FieldDefinition[B], op3: FieldDefinition[C], op4: FieldDefinition[D], op5: FieldDefinition[E],
                                                op6: FieldDefinition[F], op7: FieldDefinition[G], op8: FieldDefinition[H], op9: FieldDefinition[I], op10: FieldDefinition[J],
                                                op11: FieldDefinition[K], op12: FieldDefinition[L]) =
    HList12(op1, op2, op3, op4, op5, op6, op7, op8, op9, op10, op11, op12)



}


