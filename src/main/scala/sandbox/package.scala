import cats.data.Reader
import sandbox.model.Db

package object sandbox {

  type DbReader[T] = Reader[Db, T]

}
