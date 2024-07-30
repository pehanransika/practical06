import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine
import scala.io.StdIn.readInt

object student_record {

  def calculateGrade(percentage: Double): Char = {
    if (percentage >= 90) 'A'
    else if (percentage >= 75) 'B'
    else if (percentage >= 50) 'C'
    else 'D'
  }

  def getStudentInfo(): (String, Int, Int, Double, Char) = {
    println("Enter name:")
    val name = readLine()
    println("Enter student marks:")
    val marks = readInt()
    println("Enter total possible marks:")
    val total_marks = readInt()

    val percentage = (marks.toDouble / total_marks.toDouble) * 100
    val grade = calculateGrade(percentage)

    (name, marks, total_marks, percentage, grade)
  }

  def printStudentRecord(
      studentInfo: (String, Int, Int, Double, Char)
  ): Unit = {
    println("Name: " + studentInfo._1)
    println("Marks: " + studentInfo._2)
    println("Total marks: " + studentInfo._3)
    println("Percentage: " + studentInfo._4)
    println("Grade: " + studentInfo._5)
  }

  def displayAllStudentRecords(
      studentInfoList: ListBuffer[(String, Int, Int, Double, Char)]
  ): Unit = {
    if (studentInfoList.isEmpty) {
      println("No student records available.")
    } else {
      studentInfoList.foreach { studentInfo =>
        printStudentRecord(studentInfo)
        println("-" * 20)
      }
    }
  }

  def validateInput(
      name: String,
      marks: Int,
      total_marks: Int
  ): (Boolean, Option[String]) = {
    if (name.isEmpty) {
      (false, Some("Name cannot be empty"))
    } else if (marks < 0 || total_marks < 0 || marks > total_marks) {
      (false, Some("Marks cannot be negative or greater than total marks"))
    } else {
      (true, None)
    }
  }

  def getStudentInfoWithRetry(): (String, Int, Int, Double, Char) = {
    var isValid = false
    var name = ""
    var marks = 0
    var total_marks = 0
    var percentage = 0.0
    var grade = 'D'
    var errorMsg: Option[String] = None
    while (!isValid) {
      val (name1, marks1, total1, perc, grade1) = getStudentInfo()
      name = name1
      marks = marks1
      total_marks = total1
      percentage = perc
      grade = grade1

      val validation = validateInput(name, marks, total_marks)
      isValid = validation._1
      errorMsg = validation._2

      if (!isValid) {
        println(s"Error: ${errorMsg.get}")
      }
    }
    (name, marks, total_marks, percentage, grade)
  }

  def main(args: Array[String]): Unit = {
    val studentInfoList = ListBuffer[(String, Int, Int, Double, Char)]()
    var continue = true
    while (continue) {
      println(
        "Options:\n1. Enter student information\n2. Display all student records\n3. Quit"
      )
      val input = readLine()
      input match {
        case "1" =>
          val studentInfo = getStudentInfoWithRetry()
          studentInfoList += studentInfo
        case "2" =>
          displayAllStudentRecords(studentInfoList)
        case "3" =>
          continue = false
        case _ =>
          println("Invalid input. Please enter 1, 2, or 3.")
      }
    }
    println("Program terminated.")
  }
}
