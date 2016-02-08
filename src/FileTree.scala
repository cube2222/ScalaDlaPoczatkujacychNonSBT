/**
  * Created by Jakub Martin on 2/8/2016.
  */
import java.io
import java.io._
import scala.util.parsing.combinator.RegexParsers

@SerialVersionUID(100L)
sealed trait FileTree extends Serializable {
  val value: io.File
  def getSizeInBytes(): Long = {
    def go(ft: FileTree, sizeSum: Long): Long = ft match {
      case Directory(x, Nil) => 0
      case Directory(x, xt) => xt.map(_ getSizeInBytes).sum
      case File(x) => x.length()
    }
    go(this, 0)
  }

  def getFilesAndDirectories(): (List[File], List[Directory]) = {
    def getFiles(ft: FileTree): List[File] = ft match {
      case Directory(x, xt) => xt.flatMap(getFiles)
      case File(x) => File(x) :: Nil
      case _ => Nil
    }
    def getDirectories(ft: FileTree) : List[Directory] = ft match {
      case Directory(x, xt) => Directory(x, xt) :: xt.flatMap(getDirectories)
      case File(_) => Nil
    }
    (getFiles(this), getDirectories(this))
  }

  def getDuplicates(): List[String] = {
    val filesAndDirectories = this.getFilesAndDirectories
    val duplicateFiles: List[String] = filesAndDirectories._1.groupBy(_.value.getName()).mapValues(_.size).filter(x => x._2 > 1).map(_._1).toList
    val duplicateDirectories: List[String] = filesAndDirectories._2.groupBy(_.value.getName()).mapValues(_.size).filter(x => x._2 > 1).map(_._1).toList
    return duplicateFiles ::: duplicateDirectories
  }

  override def toString(): String = {
    def go(ft: FileTree): StringBuilder = ft match {
      case Directory(x, xt) => {
        (new StringBuilder()).append("Directory ").append(x).append(" ").append("{").append("\n").append(xt.map(_.toString()).mkString(";")).append("}").append("\n")
      }
      case File(x) => {
        (new StringBuilder()).append("File ").append(x).append("\n")
      }
    }
    go(this).toString()
  }

}
case class Directory(value: io.File, children: List[FileTree]) extends FileTree
case class File(value: io.File) extends FileTree


object FileTree {

  def loadFileTree(directory: io.File): FileTree = {
    if (directory.isFile()) File(directory)
    else Directory(directory, directory.listFiles().map(loadFileTree).toList)
  }

  object fileTreeFromStringParser extends RegexParsers  {
    def obj : Parser[FileTree] = dir | fil
    def dir : Parser[Directory] = "Directory " ~ "((?:[a-zA-Z]\\:){0,1}(?:[\\\\/][\\w.]+){1,})".r ~ ls ^^ ((x) => {
      Directory(new io.File(x._1._2), x._2)
    })
    def fil : Parser[File] = "File " ~ "((?:[a-zA-Z]\\:){0,1}(?:[\\\\/][\\w.]+){1,})".r ^^ ((x) => {
      File(new io.File(x._2))
    })
    def ls : Parser[List[FileTree]] = "{"~repsep(obj, ";")~"}" ^^ ((x) => {
      x._1._2
    })
  }

}


