package sandbox.printable

import sandbox.model.Cat

object PrintableInstances {
  implicit def printableCat: Printable[Cat] = new Printable[Cat] {
    override def format(value: Cat): String = s"${value.name} is a ${value.age} year-old ${value.color} cat"
  }
}
