package zio.sql.mysql

import java.sql.ResultSet
import java.time.Year

import zio.sql.Sql

trait MysqlSqlModule extends Sql { self =>

  override type TypeTagExtension[+A] = MysqlSpecific.MysqlTypeTag[A]

  object MysqlSpecific {
    trait MysqlTypeTag[+A] extends Tag[A] with Decodable[A]

    object MysqlTypeTag {
      implicit case object TYear extends MysqlTypeTag[Year] {
        override def decode(column: Int, resultSet: ResultSet): Either[DecodingError, Year] =
          scala.util
            .Try(Year.of(resultSet.getByte(column).toInt))
            .fold(
              _ => Left(DecodingError.UnexpectedNull(column)),
              r => Right(r)
            )
      }

    }
  }

  object MysqlFunctionDef {
    val Crc32     = FunctionDef[String, Long](FunctionName("crc32"))
    val Degrees   = FunctionDef[Double, Double](FunctionName("degrees"))
    val Log2      = FunctionDef[Double, Double](FunctionName("log2"))
    val Log10     = FunctionDef[Double, Double](FunctionName("log10"))
    val Pi        = Expr.FunctionCall0[Double](FunctionDef[Any, Double](FunctionName("pi")))
    val BitLength = FunctionDef[String, Int](FunctionName("bit_length"))
  }

}
