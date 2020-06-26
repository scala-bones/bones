package com.bones.schemas

import java.time.Instant

import com.bones.data.Sugar
import com.bones.data.custom.{ScalaCoreInjectedSugar, ScalaCoreSugar, ScalaCoreValue}
import shapeless.ops.coproduct
import shapeless.{:+:, CNil, Inl, Inr}

object CustomCovSchema {

  trait CustomAlgebra[A]
  case object MarkdownData extends CustomAlgebra[String]
  val markdown = MarkdownData

  trait DateExtAlgebra[A]
  case object InstantData extends DateExtAlgebra[Instant]
  val instant = InstantData

  case class BlogPost(
                       id: Int,
                       title: String,
                       tags: List[String],
                       publishDate: Instant,
                       content: String)

  type BlogAlgebra[A] = CustomAlgebra[A] :+: DateExtAlgebra[A] :+: ScalaCoreValue[A] :+: CNil

  object BlogPost extends ScalaCoreInjectedSugar[BlogAlgebra] with Sugar[BlogAlgebra] {


    override def scalaCoreInjected[A]: coproduct.Inject[BlogAlgebra[A], ScalaCoreValue[A]] =
      input => Inr(Inr(Inl(input)))

    def a1[A](customAlgebra: CustomAlgebra[A]): BlogAlgebra[A] =
      Inl(customAlgebra)

    def a2[A](instantData: DateExtAlgebra[A]): BlogAlgebra[A] = Inr(Inl(instantData))

    val baseSchema =
      ("id", int(iv.min(1))) ::
        ("title", string(sv.max(50))) ::
        ("tags", list(string())) :<:
        ("publishDate", a2(instant)) ::
        ("content", a1(markdown)) ::
        kvpNil

    val blogPostSchema = baseSchema.convert[BlogPost]
  }

  val blogPostInstant = BlogPost(
    1,
    "My Blog Post",
    List("music", "household goods"),
    Instant.now(),
    "Ipsum Plurbus Unum")
}
