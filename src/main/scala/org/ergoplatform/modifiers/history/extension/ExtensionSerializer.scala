package org.ergoplatform.modifiers.history.extension

import org.ergoplatform.settings.Constants
import scorex.core.serialization.ScorexSerializer
import scorex.util.serialization.{Reader, Writer}
import scorex.util.{bytesToId, idToBytes}

object ExtensionSerializer extends ScorexSerializer[Extension] {

  override def serialize(obj: Extension, w: Writer): Unit = {
    w.putBytes(idToBytes(obj.headerId))
    w.putUShort(obj.fields.size)
    obj.fields.foreach { case (key, value) =>
      w.putBytes(key)
      w.putUByte(value.length)
      w.putBytes(value)
    }
  }

  override def parse(r: Reader): Extension = {
    val startPosition = r.position
    val headerId = bytesToId(r.getBytes(Constants.ModifierIdSize))
    val fieldsSize = r.getUShort()
    val fieldsView = (1 to fieldsSize).toStream.map { _ =>
      val key = r.getBytes(Extension.FieldKeySize)
      val length = r.getUByte()
      val value = r.getBytes(length)
      (key, value)
    }
    val fields = fieldsView.takeWhile(_ => r.position - startPosition < Constants.MaxExtensionSizeMax)
    require(r.position - startPosition < Constants.MaxExtensionSizeMax)
    Extension(headerId, fields, Some(r.position - startPosition))
  }

}
