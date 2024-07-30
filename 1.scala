object Inventry {

  def retrieveAllProductNames(
      inventory: Map[Int, (String, Int, Double)]
  ): Unit = {

    val productNames = inventory.values.map(_._1)
    productNames.foreach(println)
  }

  def calculateTotalValue(
      inventory: Map[Int, (String, Int, Double)]
  ): Double = {
    inventory.values.map { case (_, quantity, price) =>
      quantity * price
    }.sum
  }

  def isEmpty(inventory: Map[Int, (String, Int, Double)]): Boolean = {
    inventory.isEmpty
  }

  def mergeInventories(
      inventory1: Map[Int, (String, Int, Double)],
      inventory2: Map[Int, (String, Int, Double)]
  ): Map[Int, (String, Int, Double)] = {
    val merged = inventory1.map { case (id, (name, quantity, price)) =>
      inventory2.get(id) match {
        case Some((_, qty2, price2)) =>
          id -> (name, quantity + qty2, price2)
        case None =>
          id -> (name, quantity, price)
      }
    }
    val finalMerged = merged ++ inventory2.filterNot { case (id, _) =>
      inventory1.contains(id)
    }
    finalMerged
  }

  def printProductDetails(
      inventory: Map[Int, (String, Int, Double)],
      id: Int
  ): Unit = {
    inventory.get(id) match {
      case Some((name, quantity, price)) =>
        println(
          s"Product ID $id: N++ame=$name, Quantity=$quantity, Price=$price"
        )
      case None =>
        println(s"Product ID $id does not exist in the inventory.")
    }
  }

  def main(args: Array[String]): Unit = {

    val Inventory1: Map[Int, (String, Int, Double)] = Map(
      545 -> ("ProductA", 50, 20.0),
      102 -> ("ProductB", 30, 30.0),
      433 -> ("ProductC", 20, 25.0)
    )

    val Inventory2: Map[Int, (String, Int, Double)] = Map(
      102 -> ("ProductB", 40, 35.0),
      104 -> ("ProductD", 10, 15.0)
    )

    println("Product names in inventory1:")
    retrieveAllProductNames(Inventory1)

    println(
      "Total value of all products in inventory1: " + calculateTotalValue(
        Inventory1
      )
    )

    println("Is inventory1 empty? " + isEmpty(Inventory1))

    val mergedInventory = mergeInventories(Inventory1, Inventory2)

    println("Merged Inventory:")
    mergedInventory.foreach { case (id, (name, quantity, price)) =>
      println(s"ID: $id, Name: $name, Quantity: $quantity, Price: $price")
    }

    printProductDetails(Inventory1, 102)

  }

}
