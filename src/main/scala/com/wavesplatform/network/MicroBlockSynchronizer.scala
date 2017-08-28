package com.wavesplatform.network

import java.util.concurrent.TimeUnit

import com.google.common.cache.{Cache, CacheBuilder}
import com.wavesplatform.network.MicroBlockSynchronizer._
import com.wavesplatform.state2.ByteStr
import io.netty.channel.ChannelHandler.Sharable
import io.netty.channel._
import monix.eval.Task
import scorex.transaction.NgHistory
import scorex.utils.ScorexLogging

import scala.collection.mutable.{Set => MSet}
import scala.concurrent.duration.FiniteDuration

@Sharable
class MicroBlockSynchronizer(settings: Settings, history: NgHistory)
  extends ChannelInboundHandlerAdapter with ScorexLogging {

  private implicit val scheduler = monix.execution.Scheduler.singleThread("microblock-synchronizer")

  private val awaitingMicroBlocks = cache[MicroBlockSignature, Object](settings.invCacheTimeout)
  private val knownMicroBlockOwners = cache[MicroBlockSignature, MSet[ChannelHandlerContext]](settings.invCacheTimeout)
  private val successfullyRecievedMicroBlocks = cache[MicroBlockSignature, Object](settings.processedMicroBlocksCacheTimeout)

  private def awaitingMicroBlockResponse(microBlockSig: MicroBlockSignature): Boolean = Option(awaitingMicroBlocks.getIfPresent(microBlockSig)).isDefined
  private def alreadyProcessedMicroBlock(microBlockSig: MicroBlockSignature): Boolean = Option(successfullyRecievedMicroBlocks.getIfPresent(microBlockSig)).isDefined

  def requestMicroBlock(microBlockSig: MicroBlockSignature, attemptsAllowed: Int): Unit =
    if (attemptsAllowed > 0 && !alreadyProcessedMicroBlock(microBlockSig) && !awaitingMicroBlockResponse(microBlockSig)) {
      val knownChannels = knownMicroBlockOwners.get(microBlockSig, () => MSet.empty)
      random(knownChannels) match {
        case Some(ctx) =>
          knownChannels -= ctx
          ctx.writeAndFlush(MicroBlockRequest(microBlockSig))
          awaitingMicroBlocks.put(microBlockSig, dummy)
          Task { // still not received the microBlock
            if (!alreadyProcessedMicroBlock(microBlockSig))
              requestMicroBlock(microBlockSig, attemptsAllowed - 1)
          }.delayExecution(settings.waitResponseTimeout)
            .runAsync
        case None =>
      }
    }


  override def channelRead(ctx: ChannelHandlerContext, msg: AnyRef): Unit = msg match {
    case MicroBlockResponse(mb) => Task {
      log.trace(id(ctx) + "Received MicroBlockResponse " + mb)
      knownMicroBlockOwners.invalidate(mb.totalResBlockSig)
      awaitingMicroBlocks.invalidate(mb.totalResBlockSig)
      successfullyRecievedMicroBlocks.put(mb.totalResBlockSig, dummy)
    }.runAsync
    case mi@MicroBlockInv(totalResBlockSig, prevResBlockSig) => Task {
      log.trace(id(ctx) + "Received " + mi)
      history.lastBlockId() match {
        case Some(lastBlockId) =>
          if (lastBlockId == prevResBlockSig) {
            knownMicroBlockOwners.get(totalResBlockSig, () => MSet.empty) += ctx
            requestMicroBlock(totalResBlockSig, 2)
          } else
            log.trace(s"Discarding $mi because it doesn't match last (micro)block")
        case None =>
      }
    }.runAsync
    case _ => super.channelRead(ctx, msg)
  }
}

object MicroBlockSynchronizer {

  type MicroBlockSignature = ByteStr

  case class Settings(waitResponseTimeout: FiniteDuration,
                      processedMicroBlocksCacheTimeout: FiniteDuration,
                      invCacheTimeout: FiniteDuration)

  def random[T](s: MSet[T]): Option[T] = {
    val n = util.Random.nextInt(s.size)
    val ts = s.iterator.drop(n)
    if (ts.hasNext) Some(ts.next)
    else None
  }

  def cache[K <: AnyRef, V<: AnyRef](timeout: FiniteDuration): Cache[K, V] = CacheBuilder.newBuilder()
    .expireAfterWrite(timeout.toMillis, TimeUnit.MILLISECONDS)
    .build[K, V]()

  private val dummy = new Object()
}
