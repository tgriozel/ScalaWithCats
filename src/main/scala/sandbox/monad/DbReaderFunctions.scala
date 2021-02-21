package sandbox.monad

import cats.data.Reader
import sandbox.DbReader
import sandbox.model.Db

object DbReaderFunctions {

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader { db: Db =>
      db.usernames.get(userId)
    }

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader { db: Db =>
      db.passwords.get(username).contains(password)
    }

  def checkLogin(userId: Int, password: String): DbReader[Boolean] =
    for {
      usernameOpt <- findUsername(userId)
      isPasswordCorrect <- usernameOpt
        .map(userName => checkPassword(userName, password))
        .getOrElse(Reader { _: Db => false })
    } yield isPasswordCorrect



}
