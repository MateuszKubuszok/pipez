package pipez

import scala.util.chaining.scalaUtilChainingOps

// TODO: case class -> java bean
// TODO: java bean -> case class
// TODO: test backticked names like `a b`
// TODO: test conversion for types with type parameters

class NoContextCodecDerivationSpec extends munit.FunSuite {

  // case class -> case class

  test("case class -> case class derivation: no config, no conversion") {
    assertEquals(
      PipeDerivation.derive[NoContextCodec, ZeroIn, ZeroOut].decode(ZeroIn()),
      Right(ZeroOut())
    )
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseOnesIn, CaseOnesOut].decode(CaseOnesIn(1)),
      Right(CaseOnesOut(1))
    )
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseManyIn, CaseManyOut].decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOut(1, "a", 2L))
    )
  }

  test("case class -> case class derivation: param conversion") {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseOnesIn, CaseOnesOutMod].decode(CaseOnesIn(1)),
      Right(CaseOnesOutMod("1"))
    )
    assertEquals(
      PipeDerivation.derive[NoContextCodec, CaseManyIn, CaseManyOutMod].decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutMod("1", "a", 2L))
    )
  }

  test("case class -> case class derivation: addField config (create field value from whole object)") {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, ZeroIn, CaseZeroOutExt].addField(_.x, (in: ZeroIn) => Right(in.toString))
        )
        .decode(ZeroIn()),
      Right(CaseZeroOutExt("ZeroIn()"))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseOnesIn, CaseOnesOutExt]
            .addField(_.x, (in: CaseOnesIn) => Right(in.toString))
        )
        .decode(CaseOnesIn(1)),
      Right(CaseOnesOutExt(1, "CaseOnesIn(1)"))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseManyIn, CaseManyOutExt]
            .addField(_.x, (in: CaseManyIn) => Right(in.toString))
        )
        .decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutExt(1, "a", 2L, "CaseManyIn(1,a,2)"))
    )
  }

  test("case class -> case class derivation: plugIn config (create field value from some source field)") {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseOnesIn, CaseOnesOutExt]
            .plugIn(_.a, _.x, (a: Int) => Right(a.toString))
        )
        .decode(CaseOnesIn(1)),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseManyIn, CaseManyOutExt]
            .plugIn(_.a, _.x, (a: Int) => Right(a.toString))
        )
        .decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
  }

  test(
    "case class -> case class derivation: renameField config + conversion (should create value, converting some source field)"
  ) {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseOnesIn, CaseOnesOutExt].renameField(_.a, _.x)
        )
        .decode(CaseOnesIn(1)),
      Right(CaseOnesOutExt(1, "1"))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseManyIn, CaseManyOutExt].renameField(_.a, _.x)
        )
        .decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutExt(1, "a", 2L, "1"))
    )
  }

  test(
    "case class -> case class derivation: fieldMatchingCaseInsensitive (should match fields with similar names but with different capitalisation)"
  ) {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseOnesIn, CaseOnesOutUpp].fieldMatchingCaseInsensitive
        )
        .decode(CaseOnesIn(1)),
      Right(CaseOnesOutUpp(1))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, CaseManyIn, CaseManyOutUpp].fieldMatchingCaseInsensitive
        )
        .decode(CaseManyIn(1, "a", 2L)),
      Right(CaseManyOutUpp(1, "a", 2L))
    )
  }

  // Java Beans -> Java Beans

  test("Java Beans -> Java Beans derivation: no config, no conversion") {
    assertEquals(
      PipeDerivation.derive[NoContextCodec, BeanOnesIn, BeanOnesOut].decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOut().tap(_.setA(1)))
    )
    assertEquals(
      PipeDerivation
        .derive[NoContextCodec, BeanManyIn, BeanManyOut]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOut().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)))
    )
  }

  test("Java Beans -> Java Beans derivation: param conversion") {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    assertEquals(
      PipeDerivation.derive[NoContextCodec, BeanOnesIn, BeanOnesOutMod].decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutMod().tap(_.setA("1")))
    )
    assertEquals(
      PipeDerivation
        .derive[NoContextCodec, BeanManyIn, BeanManyOutMod]
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutMod().tap(_.setA("1")).tap(_.setB("a")).tap(_.setC(2L)))
    )
  }

  test("Java Beans -> Java Beans derivation: addField config (create field value from whole object)") {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, ZeroIn, BeanZeroOutExt].addField(_.getX(),
                                                                                (in: ZeroIn) => Right(in.toString)
          )
        )
        .decode(ZeroIn()),
      Right(new BeanZeroOutExt().tap(_.setX("ZeroIn()")))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanOnesIn, BeanOnesOutExt]
            .addField(_.getX(), (in: BeanOnesIn) => Right(in.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("BeanOnesIn(1)")))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanManyIn, BeanManyOutExt]
            .addField(_.getX(), (in: BeanManyIn) => Right(in.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("BeanManyIn(1,a,2)")))
    )
  }

  test("Java Beans -> Java Beans derivation: plugIn config (create field value from some source field)") {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanOnesIn, BeanOnesOutExt]
            .plugIn(_.getA(), _.getX(), (a: Int) => Right(a.toString))
        )
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanManyIn, BeanManyOutExt]
            .plugIn(_.getA(), _.getX(), (a: Int) => Right(a.toString))
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
  }

  test(
    "Java Beans -> Java Beans derivation: renameField config + conversion (should create value, converting some source field)"
  ) {
    implicit val aCodec: NoContextCodec[Int, String] = int => Right(int.toString)
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanOnesIn, BeanOnesOutExt].renameField(_.getA(), _.getX())
        )
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutExt().tap(_.setA(1)).tap(_.setX("1")))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanManyIn, BeanManyOutExt].renameField(_.getA(), _.getX())
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutExt().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)).tap(_.setX("1")))
    )
  }

  test(
    "Java Beans -> Java Beans derivation: fieldMatchingCaseInsensitive (should match fields with similar names but with different capitalisation)"
  ) {
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanOnesIn, BeanOnesOutUpp].fieldMatchingCaseInsensitive
        )
        .decode(new BeanOnesIn().tap(_.setA(1))),
      Right(new BeanOnesOutUpp().tap(_.setA(1)))
    )
    assertEquals(
      PipeDerivation
        .derive(
          PipeDerivationConfig[NoContextCodec, BeanManyIn, BeanManyOutUpp].fieldMatchingCaseInsensitive
        )
        .decode(new BeanManyIn().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L))),
      Right(new BeanManyOutUpp().tap(_.setA(1)).tap(_.setB("a")).tap(_.setC(2L)))
    )
  }
}