import scala.io.StdIn

case class Book(title: String, author: String, isbn: String)

object LibraryManagement {

  var library: Set[Book] = Set(
    Book("Kaalaya", "Perera", "343423"),
    Book("The Time", "Silva", "453454"),
    Book("Harry Potter", "Rowling", "324324")
  )

  def addBook(book: Book): Unit = {
    library += book
    println(s"Book '${book.title}' added to the library.")
  }

  def removeBook(isbn: String): Unit = {
    var itemFound = false
    for (book <- library) {
      if (book.isbn == isbn) {
        library -= book
        itemFound = true
        println(s"Book '${book.title}' removed")
      }
    }
    if (!itemFound) {
      println(s"No book found for ISBN $isbn.")
    }
  }

  def checkBook(isbn: String): Boolean = {
    var found = false
    for (book <- library) {
      if (book.isbn == isbn) {
        found = true
      }
    }
    found
  }

  def displayLibrary(): Unit = {
    if (library.isEmpty) {
      println("Empty")
    } else {
      library.foreach { book =>
        println(
          s"Title - ${book.title}, Author - ${book.author}, ISBN - ${book.isbn}"
        )
      }
    }
  }

  def searchBookByTitle(): Unit = {
    println("Enter the title to Search")
    val titleToSearch = StdIn.readLine()
    var found = false
    for (book <- library) {
      if (book.title.equalsIgnoreCase(titleToSearch)) {
        found = true
        println("Book Found")
        println(
          s"Details: Title: ${book.title}, Author: ${book.author}, ISBN: ${book.isbn}"
        )
      }
    }
    if (!found) {
      println("Book Not Found")
    }
  }

  def displayBooksByAuthor(author: String): Unit = {
    var itemFound = false
    for (book <- library) {
      if (book.author == author) {
        itemFound = true
        println(
          s"Title: ${book.title}, Author: ${book.author}, ISBN: ${book.isbn}"
        )
      }
    }
    if (!itemFound) {
      println(s"No books found by author $author.")
    }
  }
}

object StudentManagement {

  def validateInput(
      name: String,
      marks: Int,
      totalMarks: Int
  ): (Boolean, Option[String]) = {
    if (name.trim.isEmpty) {
      (false, Some("Name cannot be empty."))
    } else if (marks < 0 || marks > totalMarks) {
      (false, Some("Marks must be between 0 and total possible marks."))
    } else if (totalMarks <= 0) {
      (false, Some("Total possible marks must be greater than 0."))
    } else {
      (true, None)
    }
  }

  def getStudentInfo: (String, Int, Int, Double, Char) = {
    var isValid = false
    var studentInfo: (String, Int, Int, Double, Char) = ("", 0, 0, 0.0, 'F')

    while (!isValid) {
      println("Enter student's name:")
      val name = StdIn.readLine()

      println("Enter marks obtained:")
      val marks = StdIn.readInt()

      println("Enter total possible marks:")
      val totalMarks = StdIn.readInt()

      val (valid, errorMessage) = validateInput(name, marks, totalMarks)

      if (valid) {
        val percentage = (marks.toDouble / totalMarks) * 100
        val grade = percentage match {
          case p if p >= 90 => 'A'
          case p if p >= 75 => 'B'
          case p if p >= 50 => 'C'
          case _            => 'D'
        }

        studentInfo = (name, marks, totalMarks, percentage, grade)
        isValid = true
      } else {
        println(errorMessage.get)
      }
    }

    studentInfo
  }

  def printStudentRecord(record: (String, Int, Int, Double, Char)): Unit = {
    val (name, marks, totalMarks, percentage, grade) = record
    println(s"Student Name: $name")
    println(s"Marks Obtained: $marks")
    println(s"Total Possible Marks: $totalMarks")
    println(f"Percentage: $percentage%.2f")
    println(s"Grade: $grade")
  }

  def getStudentInfoWithRetry: (String, Int, Int, Double, Char) = {
    getStudentInfo
  }
}

object MainApp {

  def main(args: Array[String]): Unit = {
    println("Library Management System")
    println("Library Collection - ")
    LibraryManagement.displayLibrary()

    println("\nAdding Book")
    LibraryManagement.addBook(Book("Madol Duwa", "ABS", "34234"))

    println("\nRemove Book")
    LibraryManagement.removeBook("453454")

    println("\nChecking if a book is in the library by ISBN:")
    println(LibraryManagement.checkBook("343423"))

    println("\nSearching for a book by title:")
    LibraryManagement.searchBookByTitle()

    println("\nDisplaying all books by a specific author:")
    LibraryManagement.displayBooksByAuthor("Silva")

    println("\nStudent Management System")
    val studentRecord = StudentManagement.getStudentInfoWithRetry
    StudentManagement.printStudentRecord(studentRecord)
  }
}
