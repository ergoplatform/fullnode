package org.ergoplatform.http.routes

import akka.http.scaladsl.model.{ContentTypes, HttpEntity, StatusCodes, UniversalEntity}
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.testkit.ScalatestRouteTest
import io.circe.Json
import io.circe.syntax._
import org.ergoplatform.http.api.BlocksApiRoute
import org.ergoplatform.modifiers.ErgoFullBlock
import org.ergoplatform.modifiers.history.header.Header
import org.ergoplatform.settings.Algos
import org.ergoplatform.utils.Stubs
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scorex.util.ModifierId

class BlocksApiRouteSpec extends AnyFlatSpec
  with Matchers
  with ScalatestRouteTest
  with Stubs {

  val prefix = "/blocks"

  val route: Route = BlocksApiRoute(nodeViewRef, digestReadersRef, settings).route

  val headerIdBytes: ModifierId = history.lastHeaders(1).headers.head.id
  val headerIdString: String = Algos.encode(headerIdBytes)

  it should "get last blocks" in {
    Get(prefix) ~> route ~> check {
      status shouldBe StatusCodes.OK
      history.headerIdsAt(0, 50).map(Algos.encode).asJson.toString() shouldEqual responseAs[String]
    }
  }

  it should "post block correctly" in {
    val (st, bh) = createUtxoState()
    val block: ErgoFullBlock = validFullBlock(parentOpt = None, st, bh)
    val blockJson: UniversalEntity = HttpEntity(block.asJson.toString).withContentType(ContentTypes.`application/json`)
    Post(prefix, blockJson) ~> route ~> check {
      status shouldBe StatusCodes.OK
    }
  }

  it should "get last headers" in {
    Get(prefix + "/lastHeaders/1") ~> route ~> check {
      status shouldBe StatusCodes.OK
      history.lastHeaders(1).headers.map(_.asJson).asJson.toString() shouldEqual responseAs[String]
    }
  }

  it should "get block at height" in {
    Get(prefix + "/at/0") ~> route ~> check {
      status shouldBe StatusCodes.OK
      history.headerIdsAtHeight(0).map(Algos.encode).asJson.toString() shouldEqual responseAs[String]
    }
  }

  it should "get chain slice" in {
    Get(prefix + "/chainSlice?fromHeight=0") ~> route ~> check {
      status shouldBe StatusCodes.OK
      chain.map(_.header).asJson.toString() shouldEqual responseAs[String]
    }
    Get(prefix + "/chainSlice?fromHeight=2&toHeight=4") ~> route ~> check {
      status shouldBe StatusCodes.OK
      chain.slice(2, 4).map(_.header).asJson.toString() shouldEqual responseAs[String]
    }
  }

  it should "get block by header id" in {
    Get(prefix + "/" + headerIdString) ~> route ~> check {
      status shouldBe StatusCodes.OK
      val expected = history.typedModifierById[Header](headerIdBytes)
        .flatMap(history.getFullBlock)
        .map(_.asJson)
        .get
        .toString
      responseAs[String] shouldEqual expected
    }
  }

  it should "get header by header id" in {
    Get(prefix + "/" + headerIdString + "/header") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val expected = history
        .typedModifierById[Header](headerIdBytes)
        .flatMap(history.getFullBlock)
        .map(_.header.asJson)
        .get
        .toString
      responseAs[String] shouldEqual expected
    }
  }

  it should "get transactions by header id" in {
    Get(prefix + "/" + headerIdString + "/transactions") ~> route ~> check {
      status shouldBe StatusCodes.OK
      val header = history.typedModifierById[Header](headerIdBytes).value
      val fullBlock = history.getFullBlock(header).value
      val expected = fullBlock.blockTransactions.asJson.toString
      responseAs[String] shouldEqual expected
    }
  }

}
