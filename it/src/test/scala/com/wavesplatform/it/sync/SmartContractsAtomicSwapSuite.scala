package com.wavesplatform.it.sync

import com.wavesplatform.crypto
import com.wavesplatform.it.api.SyncHttpApi._
import com.wavesplatform.it.transactions.BaseTransactionSuite
import com.wavesplatform.it.util._
import com.wavesplatform.lang.v1.{Parser, TypeChecker}
import com.wavesplatform.state._
import com.wavesplatform.utils.dummyTypeCheckerContext
import org.scalatest.CancelAfterFailure
import play.api.libs.json.JsNumber
import scorex.account.PrivateKeyAccount
import scorex.transaction.Proofs
import scorex.transaction.smart.SetScriptTransaction
import scorex.transaction.smart.script.v1.ScriptV1
import scorex.transaction.transfer._
import scorex.crypto.encode.{Base58 => ScorexBase58}

class SmartContractsAtomicSwapSuite extends BaseTransactionSuite with CancelAfterFailure {

  private val BobBC1: String   = sender.createAddress()
  private val AliceBC1: String = sender.createAddress()
  private val swapBC1: String  = sender.createAddress()

  private val BobBC2: String   = sender.createAddress()
  private val AliceBC2: String = sender.createAddress()
  private val swapBC2: String  = sender.createAddress()

  private val transferAmount: Long = 1.waves
  private val fee: Long            = 0.001.waves

  private val AlicesPK = PrivateKeyAccount.fromSeed(sender.seed(AliceBC1)).right.get
  private val BobsPK   = PrivateKeyAccount.fromSeed(sender.seed(BobBC2)).right.get

  private val secretText = "some secret message from Alice"
  private val secretHash = ScorexBase58.encode(secretText.getBytes)
  private val shaSecret  = "BN6RTYGWcwektQfSFzH8raYo9awaLgQ7pLyWLQY4S4F5"

  private val sc1 = {
    val untyped = Parser(s"""
    let Bob = extract(addressFromString("${BobBC1}")).bytes
    let Alice = extract(addressFromString("${AliceBC1}")).bytes
    let AlicesPK = base58'${ByteStr(AlicesPK.publicKey)}'
    
    let txRecipient = addressFromRecipient(tx.recipient).bytes
    let txSender = addressFromPublicKey(tx.senderPk).bytes

    let txFromAlice = if((txSender == Alice) && (tx.type == 4)) then true else false
    let txToBob = if((txRecipient == Bob) && ((sha256(tx.proof0) == base58'$shaSecret') || sigVerify(tx.bodyBytes,tx.proof1,AlicesPK)) && (200 >= height)) then true else false
    let backToAliceAfterHeight = if ((height >= 201) && (txRecipient == Alice)) then true else false

    txFromAlice || txToBob || backToAliceAfterHeight

      """.stripMargin).get.value
    TypeChecker(dummyTypeCheckerContext, untyped).explicitGet()
  }

  private val sc2 = {
    val untyped = Parser(s"""
    let Alice = extract(addressFromString("${AliceBC2}")).bytes
    let Bob = extract(addressFromString("${BobBC2}")).bytes
    let BobsPK = base58'${ByteStr(BobsPK.publicKey)}'
    
    let txRecipient = addressFromRecipient(tx.recipient).bytes
    let txSender = addressFromPublicKey(tx.senderPk).bytes

    let txFromBob = if((txSender == Bob) && (tx.type == 4)) then true else false
    let txToAlice = if((txRecipient == Alice) && ((sha256(tx.proof0) == base58'$shaSecret') || sigVerify(tx.bodyBytes,tx.proof1,BobsPK)) && (100 >= height)) then true else false
    let backToBobAfterHeight = if ((height >= 101) && (txRecipient == Bob)) then true else false

    txFromBob || txToAlice || backToBobAfterHeight
      """.stripMargin).get.value
    TypeChecker(dummyTypeCheckerContext, untyped).explicitGet()
  }

  test("step0: balances initialization") {
    val toAliceBC1TxId = sender.transfer(sender.address, AliceBC1, 10 * transferAmount, fee).id
    nodes.waitForHeightAriseAndTxPresent(toAliceBC1TxId)

    val toSwapBC1TxId = sender.transfer(sender.address, swapBC1, fee, fee).id
    nodes.waitForHeightAriseAndTxPresent(toSwapBC1TxId)

    val toBobBC2TxId = sender.transfer(sender.address, BobBC2, 10 * transferAmount, fee).id
    nodes.waitForHeightAriseAndTxPresent(toBobBC2TxId)

    val toSwapBC2TxId = sender.transfer(sender.address, swapBC2, fee, fee).id
    nodes.waitForHeightAriseAndTxPresent(toSwapBC2TxId)
  }

  test("step1: create and setup smart contract SC1") {
    val pkSwapBC1 = PrivateKeyAccount.fromSeed(sender.seed(swapBC1)).right.get
    val script    = ScriptV1(sc1).explicitGet()
    val sc1SetTx = SetScriptTransaction
      .selfSigned(version = SetScriptTransaction.supportedVersions.head,
                  sender = pkSwapBC1,
                  script = Some(script),
                  fee = fee,
                  timestamp = System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(sc1SetTx.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val swapBC1ScriptInfo = sender.addressScriptInfo(swapBC1)

    swapBC1ScriptInfo.script.isEmpty shouldBe false
    swapBC1ScriptInfo.scriptText.isEmpty shouldBe false
  }

  test("step2: Alice make transfer to swapBC1") {
    val txToSwapBC1 =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = PrivateKeyAccount.fromSeed(sender.seed(AliceBC1)).right.get,
          recipient = PrivateKeyAccount.fromSeed(sender.seed(swapBC1)).right.get,
          amount = transferAmount + fee + 0.004.waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = fee + 0.04.waves,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(txToSwapBC1.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("step3: create and setup smart contract SC2") {
    val pkSwapBC2 = PrivateKeyAccount.fromSeed(sender.seed(swapBC2)).right.get
    val script    = ScriptV1(sc2).explicitGet()
    val setScriptTransaction = SetScriptTransaction
      .selfSigned(version = SetScriptTransaction.supportedVersions.head,
                  sender = pkSwapBC2,
                  script = Some(script),
                  fee = fee,
                  timestamp = System.currentTimeMillis())
      .explicitGet()

    val setScriptId = sender
      .signedBroadcast(setScriptTransaction.json() + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt)))
      .id

    nodes.waitForHeightAriseAndTxPresent(setScriptId)

    val swapBC2ScriptInfo = sender.addressScriptInfo(swapBC2)

    swapBC2ScriptInfo.script.isEmpty shouldBe false
    swapBC2ScriptInfo.scriptText.isEmpty shouldBe false
  }

  test("step4: Bob make transfer to swapBC2") {
    val txToSwapBC2 =
      TransferTransactionV2
        .selfSigned(
          version = 2,
          assetId = None,
          sender = PrivateKeyAccount.fromSeed(sender.seed(BobBC2)).right.get,
          recipient = PrivateKeyAccount.fromSeed(sender.seed(swapBC2)).right.get,
          amount = transferAmount + fee + 0.004.waves,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = 0.001.waves + 0.04.waves,
          attachment = Array.emptyByteArray
        )
        .explicitGet()

    val transferId = sender
      .signedBroadcast(txToSwapBC2.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt)))
      .id
    nodes.waitForHeightAriseAndTxPresent(transferId)
  }

  test("step5: Alice make transfer from swapBC2 to AliceBC2 ") {
    val unsigned =
      TransferTransactionV2
        .create(
          version = 2,
          assetId = None,
          sender = PrivateKeyAccount.fromSeed(sender.seed(swapBC2)).right.get,
          recipient = PrivateKeyAccount.fromSeed(sender.seed(AliceBC2)).right.get,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = fee + 0.004.waves,
          attachment = Array.emptyByteArray,
          proofs = Proofs.empty
        )
        .explicitGet()

    val secret = ByteStr(secretText.getBytes())
    val sigBob = ByteStr(crypto.sign(BobsPK, unsigned.bodyBytes()))
    val signed = unsigned.copy(proofs = Proofs(Seq(secret, sigBob)))
    val versionedTransferId =
      sender.signedBroadcast(signed.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)

  }

  test("step6: Bob make transfer from swapBC1 to BobBC1 ") {
    val unsigned =
      TransferTransactionV2
        .create(
          version = 2,
          assetId = None,
          sender = PrivateKeyAccount.fromSeed(sender.seed(swapBC1)).right.get,
          recipient = PrivateKeyAccount.fromSeed(sender.seed(BobBC1)).right.get,
          amount = transferAmount,
          timestamp = System.currentTimeMillis(),
          feeAssetId = None,
          feeAmount = fee + 0.004.waves,
          attachment = Array.emptyByteArray,
          proofs = Proofs.empty
        )
        .explicitGet()

    val proof    = ByteStr(secretText.getBytes())
    val sigAlice = ByteStr(crypto.sign(AlicesPK, unsigned.bodyBytes()))
    val signed   = unsigned.copy(proofs = Proofs(Seq(proof, sigAlice)))
    val versionedTransferId =
      sender.signedBroadcast(signed.json() + ("type" -> JsNumber(TransferTransactionV2.typeId.toInt))).id

    nodes.waitForHeightAriseAndTxPresent(versionedTransferId)
  }

  protected def calcDataFee(data: List[DataEntry[_]]): Long = {
    val dataSize = data.map(_.toBytes.length).sum + 128
    if (dataSize > 1024) {
      fee * (dataSize / 1024 + 1)
    } else fee
  }
}
