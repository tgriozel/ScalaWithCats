package sandbox

object Main extends App {

  def chapter1(): Unit = {
    import cats.syntax.eq._
    import sandbox.eq.EqInstances._
    import sandbox.model.Cat
    import sandbox.printable.PrintableInstances._
    import sandbox.printable.PrintableSyntax._
    val felix = Cat("Felix", 5, "orange")
    val johnny = Cat("Johnny", 4, "black")
    if (felix === johnny)
      println("Cats are equal")
    else
      println("Cats are different")
    felix.print()
  }

  def chapter2(): Unit = {
    import cats.instances.int._
    import cats.instances.option._
    import sandbox.model.Order
    import sandbox.monoid.MonoidInstances.orderMonoid
    import sandbox.monoid.SuperAdder
    val sum = SuperAdder.add(List(1, 2, 3, 4, 5))
    println(sum)
    val sumOpt = SuperAdder.add(List(None, Some(1), None, Some(4), None))
    println(sumOpt)
    val ordersTotal = SuperAdder.add(List(Order(2.0, 3), Order(3.0, 1)))
    println(ordersTotal)
  }

  def chapter3(): Unit = {
    import cats.syntax.functor._
    import sandbox.functor.FunctorInstances._
    import sandbox.model.{Branch, Leaf, Tree}
    val (leaf1, leaf2, leaf3, leaf4) = (Leaf(1), Leaf(2), Leaf(3), Leaf(4))
    val branchA = Branch(leaf1, leaf2)
    val branchB = Branch(leaf3, leaf4)
    val tree: Tree[Int] = Branch(branchA, branchB)
    val func = (value: Int) => (value * 10).toString
    val transformedTree = tree.map(func)
    println(transformedTree)
  }

  def chapter4a(): Unit = {
    import sandbox.monad.EvalFolder
    val folder = new EvalFolder()
    val result = folder.foldRight(Range(0, 10000).toList, 0)(_ + _)
    println(result)
  }

  def chapter4b(): Unit = {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits._
    import scala.concurrent.duration._
    import sandbox.monad.FactorialWriter._
    val a = Future(factorial(5))
    val b = Future(factorial(5))
    val results = Await.result(Future.sequence(Vector(a, b)), 5.seconds)
    results.foreach { futureRes =>
      val (log, res) = futureRes.run
      println(log.mkString("\n") + "\n" + res)
    }
  }

  def chapter4c(): Unit = {
    import sandbox.model.Db
    import sandbox.monad.DbReaderFunctions
    val db = Db(Map(1 -> "1", 2 -> "2"), Map("1" -> "1p", "2" -> "2p"))
    println(DbReaderFunctions.checkLogin(1, "1p").run(db))
    println(DbReaderFunctions.checkLogin(2, "2").run(db))
    println(DbReaderFunctions.checkLogin(3, "3p").run(db))
  }

  def chapter4d(): Unit = {
    import sandbox.state.CalcOperator
    require(CalcOperator.evalPostFix("") == 0)
    require(CalcOperator.evalPostFix("1") == 1)
    require(CalcOperator.evalPostFix("23*1+") == 7)
    require(CalcOperator.evalPostFix("57+67+*") == 156)
    require(CalcOperator.evalPostFix("23*1+225**") == 20)
    require(CalcOperator.evalPostFix("23*1+225**+9+1/1*") == 36)
  }

  chapter4d()

}
