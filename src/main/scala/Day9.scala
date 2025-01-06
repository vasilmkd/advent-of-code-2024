import scala.collection.mutable
import scala.io.Source

object Day9:
  def readInput(): String =
    Source.fromResource("day9.txt").getLines().mkString

  private def diskLayout(diskMap: String): Array[Int] =
    var i = 0
    val builder = Array.newBuilder[Int]
    while i < diskMap.length do
      val size = diskMap(i).toString.toInt
      var s = 0
      val id = if i % 2 == 0 then i / 2 else -1
      while s < size do
        builder += id
        s += 1
      i += 1
    builder.result()

  private def defrag1(diskLayout: Array[Int]): Unit =
    var left = diskLayout.indexOf(-1)
    var right = diskLayout.lastIndexWhere(_ != -1)
    while left < right do
      diskLayout(left) = diskLayout(right)
      diskLayout(right) = -1
      while diskLayout(left) != -1 do
        left += 1
      while diskLayout(right) == -1 do
        right -= 1

  private def checksum(diskLayout: Array[Int]): Long =
    diskLayout
      .zipWithIndex
      .collect:
        case (x, y) if x != -1 => (x * y).toLong
      .sum

  case class File(id: Int, start: Int, size: Int)

  case class FreeSpace(start: Int, size: Int)

  case class Disk(files: Vector[File], freeSpaces: Vector[FreeSpace])

  private def createDisk(diskMap: String): Disk =
    val files = Vector.newBuilder[File]
    val freeSpaces = Vector.newBuilder[FreeSpace]
    var stringIndex = 0
    var diskIndex = 0
    diskMap.foreach: c =>
      val size = c.toString.toInt
      val fileId = if stringIndex % 2 == 0 then stringIndex / 2 else -1
      if size > 0 && fileId != -1 then
        files += File(fileId, diskIndex, size)
      else if size > 0 then
        freeSpaces += FreeSpace(diskIndex, size)
      diskIndex += size
      stringIndex += 1
    Disk(files.result(), freeSpaces.result())

  private def defragDisk(disk: Disk): Disk =
    val Disk(files, freeSpaces) = disk
    val sortedFiles = files.sortBy(_.id)
    val iterator = sortedFiles.reverseIterator
    val newFilesSet = mutable.Set.empty[File]
    val newFreeSpacesSet = freeSpaces.to(mutable.Set)
    while iterator.hasNext do
      val file = iterator.next()
      val leftMostSpace = newFreeSpacesSet.filter(_.size >= file.size).minByOption(_.start)
      if leftMostSpace.isDefined && file.start > leftMostSpace.get.start then
        newFreeSpacesSet.remove(leftMostSpace.get)
        val newFile = File(file.id, leftMostSpace.get.start, file.size)
        newFilesSet += newFile
        val newFreeSpaceLeft = leftMostSpace.get.size - file.size
        if newFreeSpaceLeft > 0 then
          newFreeSpacesSet += FreeSpace(leftMostSpace.get.start + file.size, newFreeSpaceLeft)
        val newFreeSpaceRight = FreeSpace(file.start, file.size)
        newFreeSpacesSet += newFreeSpaceRight
      else
        newFilesSet += file
    Disk(newFilesSet.toVector.sortBy(_.start), newFreeSpacesSet.toVector.sortBy(_.start))

  private def createDiskLayout(disk: Disk): Array[Int] =
    val size = disk.files.last.start + disk.files.last.size
    val array = new Array[Int](size)
    array.indices.foreach(i => array(i) = -1)
    disk.files.foreach:
      case File(id, start, size) =>
        var i = start
        while i < start + size do
          array(i) = id
          i += 1
    array

  def part1(): Long =
    val input = readInput()
    val disk = diskLayout(input)
    defrag1(disk)
    checksum(disk)

  def answer1: Long = 6359213660505L

  def part2(): Long =
    val input = readInput()
    val disk = createDisk(input)
    val defragged = defragDisk(disk)
    checksum(createDiskLayout(defragged))

  def answer2: Long = 6381624803796L
