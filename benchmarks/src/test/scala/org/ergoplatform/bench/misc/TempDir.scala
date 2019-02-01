package org.ergoplatform.bench.misc

object TempDir {

  protected val randomPrefixLength = 10

  def createTempDir: java.io.File = {
    val rndString = scala.util.Random.alphanumeric.take(randomPrefixLength).mkString
    createTempDirForPrefix(rndString)
  }

  private def createTempDirForPrefix(prefix: String): java.io.File = {
    val file = java.nio.file.Files.createTempDirectory(prefix).toFile
    file.deleteOnExit()
    file
  }

}
