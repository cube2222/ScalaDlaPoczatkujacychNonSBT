/**
  * Created by Jakub Martin on 2/8/2016.
  */
import java.io
import scala.util.parsing.combinator._


object Main {
  def main(args: Array[String]): Unit = {
    val ft = FileTree.loadFileTree(new io.File("/tmp"))
    val filesAndDirectories = ft.getFilesAndDirectories()
    println("Files:")
    filesAndDirectories._1.map(_.value).foreach(println)
    println("Directories:")
    filesAndDirectories._2.map(_.value).foreach(println)

    println("duplicates:")
    ft.getDuplicates().foreach(println)

    println("ft:")
    println(ft)

    val newFt = FileTree.fileTreeFromStringParser.parseAll(FileTree.fileTreeFromStringParser.obj, ft.toString())
    println(newFt)
    println(newFt.get.getDuplicates())
  }
}
