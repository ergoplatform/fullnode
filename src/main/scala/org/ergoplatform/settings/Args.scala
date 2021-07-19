package org.ergoplatform.settings

final case class Args(userConfigPathOpt: Option[String] = None, networkTypeOpt: Option[NetworkType] = None)

object Args {
  def empty: Args = Args(None, None)
}
