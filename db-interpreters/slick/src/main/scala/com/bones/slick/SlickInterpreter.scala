package com.bones.slick

//import slick.model.Table

//import scala.annotation.{StaticAnnotation, compileTimeOnly}
//import scala.language.existentials
//import scala.language.experimental.macros
//import scala.reflect.macros._
//import slick.jdbc.H2Profile.api._
//import slick.relational.RelationalProfile
//@compileTimeOnly("Enable macro paradise to expand macro annotations")
//class typedTable() extends StaticAnnotation {
//  def macroTransform[T](
//    table: SlickTable[T],
//    relationalProfile: RelationalProfile,
//    annottees: Any*): relationalProfile.Table[T] =
//    macro SlickTableGenerator.impl
//}

object SlickTableGenerator {
//  def impl[T](c: whitebox.Context)(
//    table: SlickTable[T],
//    relationalProfile: RelationalProfile,
//    annottees: c.Expr[Any]*): c.Expr[relationalProfile.Table[T]] = {
//    import c.universe._
//
//    val q"""new typedTable[$cls]().macroTransform($a)""" = c.macroApplication
//    val tpe = c.typecheck(q"(??? : $cls)").tpe
//    val fields = tpe.decls
//      .collectFirst {
//        case m: MethodSymbol if m.isPrimaryConstructor => m
//      }
//      .get
//      .paramLists
//      .head
//    val columns = fields.map(f => {
//      val name = f.name.toTermName
//      val decoded = name.decodedName.toString.toUpperCase
//      val returnType = tpe.decl(name).typeSignature
//      q"val $name = column[$returnType]($decoded)"
//    })
//
//    def modifiedObject(classDef: ClassDef): c.Expr[relationalProfile.Table[T]] = {
//      val ModuleDef(_, objectName, template) = classDef
//      val body = template.body.tail // Drop the init method
//      val decoded = objectName.decodedName.toString.toUpperCase
//      val ret = q"""
//        class $objectName(tag: Tag) extends Table[$cls](tag, $decoded) {
//          ..$columns
//          ..$body
//        }
//      """
//      c.Expr[relationalProfile.Table[T]](ret)
//    }
//
//    annottees.map(_.tree) match {
//      case (objectDecl: ClassDef) :: _ => modifiedObject(objectDecl)
//      case x                           => c.abort(c.enclosingPosition, s"@table can only be applied to a class, not to $x")
//    }
//  }
}
