/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package dotty.tools.io

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import java.net.{URI, URL}

/** This class implements an in-memory file.
 *
 *  @author  Philippe Altherr
 *  @version 1.0, 23/03/2004
 *
 *  ''Note:  This library is considered experimental and should not be used unless you know what you are doing.''
 */
class VirtualFile(
    override val path: String,
    initialContents: Array[Byte],
    maybeContainer: Option[AbstractFile]
) extends AbstractFile {
  def this(path: String, initialContents: Array[Byte]) =
    this(path, initialContents, None)

  override def container: Option[AbstractFile] = maybeContainer

  private var content = initialContents

  override val name: String =
    // VirtualDirectory always joins a child with `/`. Peel that leaf before
    // handling standalone fake names such as `<example>`, whose parent path
    // might not be accepted by the host platform's Path implementation.
    val lastSlash = path.lastIndexOf('/')
    if lastSlash >= 0 && lastSlash < path.length - 1 then
      path.substring(lastSlash + 1)
    else if path.startsWith("<") || path.startsWith("\"") then path
    else
      val fileName = java.nio.file.Path.of(path).getFileName
      if fileName == null then ""
      else fileName.toString

  // For compatibility, until we remove `AbstractFile.jpath`.
  override def jpath: JPath | Null = try java.nio.file.Path.of(path) catch case _: Exception => null

  override def toURL: Option[URL] = None

  /** Always returns true, even if jpath is a non-existing file. */
  override def exists: Boolean = true

  override def input: InputStream = new ByteArrayInputStream(content)

  override def output: OutputStream =
    new ByteArrayOutputStream() {
      override def close(): Unit = {
        super.close()
        content = toByteArray()
      }
    }

  /** Is this abstract file a directory? */
  override def isDirectory: Boolean = false

  /** @inheritdoc */
  override def isVirtual: Boolean = true

  override def lastModified: Long = 0

  override def iterator: Iterator[AbstractFile] = unsupported()

  override def lookupName(name: String, directory: Boolean): AbstractFile | Null = unsupported()
}
