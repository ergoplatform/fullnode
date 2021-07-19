package org.ergoplatform.modifiers.history

import org.ergoplatform.modifiers.history.header.Header

case class HeaderChain(headers: IndexedSeq[Header]) {

  headers.indices.foreach { i =>
    if (i > 0) require(headers(i).parentId == headers(i - 1).id,
      s"Incorrect chain: ${headers(i - 1)},${headers(i)}")
  }

  def exists(f: Header => Boolean): Boolean = headers.exists(f)

  @SuppressWarnings(Array("TraversableHead"))
  def head: Header = headers.head

  def headOption: Option[Header] = headers.headOption

  def last: Header = headers.last

  def tail: HeaderChain = HeaderChain(headers.tail)

  def take(i: Int): HeaderChain = HeaderChain(headers.take(i))

  def drop(i: Int): HeaderChain = HeaderChain(headers.drop(i))

  def takeAfter(h: Header): HeaderChain = {
    val commonIndex = headers.indexWhere(_.id == h.id)
    val commonBlockThenSuffixes = headers.takeRight(headers.length - commonIndex)
    HeaderChain(commonBlockThenSuffixes)
  }

  def apply(idx: Int): Header = headers(idx)

  lazy val size: Int = length

  lazy val length: Int = headers.size

  def ++(c: HeaderChain): HeaderChain = HeaderChain(headers ++ c.headers)

}

object HeaderChain {

  lazy val empty = HeaderChain(IndexedSeq.empty[Header])

  def apply(seq: Seq[Header]): HeaderChain = HeaderChain(seq.toIndexedSeq)

}
