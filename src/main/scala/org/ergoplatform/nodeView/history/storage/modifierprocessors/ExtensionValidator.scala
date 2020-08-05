package org.ergoplatform.nodeView.history.storage.modifierprocessors

import org.ergoplatform.modifiers.history.PoPowAlgos._
import org.ergoplatform.modifiers.history.{Extension, ExtensionCandidate, Header}
import org.ergoplatform.settings.ErgoValidationSettings
import org.ergoplatform.settings.ValidationRules._
import scorex.core.utils.ScorexEncoding
import scorex.core.validation.{ModifierValidator, ValidationState}
import scorex.util.bytesToId
import scorex.util.encode.Base16

/**
  * Class that implements extension validation based on current to ErgoValidationSettings
  */
class ExtensionValidator[T](validationState: ValidationState[T]) extends ScorexEncoding {

  def validateExtension(extension: Extension,
                        header: Header,
                        prevExtensionOpt: Option[ExtensionCandidate],
                        prevHeaderOpt: Option[Header]): ValidationState[T] = {
    validateInterlinks(extension, header, prevExtensionOpt, prevHeaderOpt)
      .validate(exKeyLength, extension.fields.forall(_._1.lengthCompare(Extension.FieldKeySize) == 0), extension.encodedId)
      .validate(exValueLength, extension.fields.forall(_._2.lengthCompare(Extension.FieldValueMaxSize) <= 0), extension.encodedId)
      .validate(exDuplicateKeys, extension.fields.map(kv => Base16.encode(kv._1)).distinct.length == extension.fields.length, extension.encodedId)
      .validate(exEmpty, header.isGenesis || extension.fields.nonEmpty, extension.encodedId)
  }

  private def validateInterlinks(extension: Extension,
                                 header: Header,
                                 prevExtensionOpt: Option[ExtensionCandidate],
                                 prevHeaderOpt: Option[Header]): ValidationState[T] = {
    (prevHeaderOpt, prevExtensionOpt) match {
      case (Some(parent), Some(parentExt)) =>
        val parentLinksTry = unpackInterlinks(parentExt.fields)
        val currentLinksTry = unpackInterlinks(extension.fields)

        val expectedLinksTry = parentLinksTry
          .map { prev => updateInterlinks(parent, prev) }

        validationState
          .validateNoFailure(exIlEncoding, currentLinksTry)
          .validate(exIlStructure, expectedLinksTry == currentLinksTry, s"$expectedLinksTry == $currentLinksTry")
      case _ =>
        validationState
          .validate(exIlUnableToValidate, header.isGenesis || prevExtensionOpt.isEmpty)
    }
  }
}
