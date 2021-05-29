package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.popow.NipopowAlgos
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header}
import org.ergoplatform.settings.ValidationRules._
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.ValidationState
import scorex.util.bytesToId

/**
  * Class that implements extension validation based on current to ErgoValidationSettings
  */
class ExtensionValidator[T](validationState: ValidationState[T], popowAlgos: NipopowAlgos) extends ScorexEncoding {

  def validateExtension(extension: Extension,
                        header: Header,
                        prevExtensionOpt: Option[ExtensionCandidate],
                        prevHeaderOpt: Option[Header]): ValidationState[T] = {
    validateInterlinks(extension, header, prevExtensionOpt, prevHeaderOpt)
      .validate(exKeyLength, extension.fields.forall(_._1.lengthCompare(Extension.FieldKeySize) == 0), extension.encodedId)
      .validate(exValueLength, extension.fields.forall(_._2.lengthCompare(Extension.FieldValueMaxSize) <= 0), extension.encodedId)
      .validate(exDuplicateKeys, extension.fields.map(kv => bytesToId(kv._1)).distinct.length == extension.fields.length, extension.encodedId)
      .validate(exEmpty, header.isGenesis || extension.fields.nonEmpty, extension.encodedId)
  }

  private def validateInterlinks(extension: Extension,
                                 header: Header,
                                 prevExtensionOpt: Option[ExtensionCandidate],
                                 prevHeaderOpt: Option[Header]): ValidationState[T] = {
    (prevHeaderOpt, prevExtensionOpt) match {
      case (Some(parent), Some(parentExt)) =>
        val parentLinksTry = NipopowAlgos.unpackInterlinks(parentExt.fields)
        val currentLinksTry = NipopowAlgos.unpackInterlinks(extension.fields)

        val expectedLinksTry = parentLinksTry
          .map { prev => popowAlgos.updateInterlinks(parent, prev) }

        validationState
          .validateNoFailure(exIlEncoding, currentLinksTry)
          .validate(exIlStructure, expectedLinksTry == currentLinksTry, s"$expectedLinksTry == $currentLinksTry")
      case _ =>
        validationState
          .validate(exIlUnableToValidate, header.isGenesis || prevExtensionOpt.isEmpty)
    }
  }
}
