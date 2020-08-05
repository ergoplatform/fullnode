package org.ergoplatform.settings

import net.ceedubs.ficus.readers.ValueReader
import scorex.util.ModifierId

trait ModifierIdReader {

  implicit val modifierIdReader: ValueReader[ModifierId] = { (cfg, path) =>
    ModifierId(cfg.getString(path))
  }

}
