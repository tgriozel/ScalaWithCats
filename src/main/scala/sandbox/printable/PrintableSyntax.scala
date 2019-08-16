package sandbox.printable

object PrintableSyntax {
  implicit class PrintableOps[A : Printable](value: A) {
    def format: String = Printable.format(value)
    def print(): Unit = Printable.print(value)
  }
}
