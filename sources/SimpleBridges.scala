package user.sjrd.simplebridges

import com.funlabyrinthe.core.*
import com.funlabyrinthe.core.graphics.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object SimpleBridges extends Module

@definition def isAboveBridge(using Universe) = Attribute.create[Boolean](false)

@definition def simpleBridgeCreator(using Universe) = new SimpleBridgeCreator

class SimpleBridgeCreator(using ComponentInit) extends ComponentCreator[SimpleBridge]:
  @transient @noinspect
  val centerPainter: Painter =
    universe.EmptyPainter + "Bridges/BridgeCenter"

  @transient @noinspect
  val openingPainters: List[Painter] =
    Direction.values.toList.map(d => universe.EmptyPainter + s"Bridges/Bridge$d")

  category = ComponentCategory("bridges", "Bridges")

  icon += "Bridges/BridgeCenter"
  icon += "Bridges/BridgeNorth"
  icon += "Bridges/BridgeSouth"
  icon += "Creators/Creator"
end SimpleBridgeCreator

class SimpleBridge(using ComponentInit) extends PosComponent:
  var openings: Set[Direction] = Direction.values.toSet

  category = ComponentCategory("bridges", "Bridges")

  // Prevent any use of the plank
  override def dispatch[A]: PartialFunction[SquareMessage[A], A] = {
    case PlankInteraction(kind, player, passOverPos, leaveFrom, arriveAt) =>
      false
  }

  override protected def doDraw(context: DrawSquareContext): Unit =
    import context.*

    val creator = simpleBridgeCreator
    context.drawTiled(creator.centerPainter)
    for dir <- openings do
      context.drawTiled(creator.openingPainters(dir.ordinal))
  end doDraw

  override def hookEntering(context: MoveContext): Unit =
    import context.*

    for dir <- player.direction do
      if !openings.contains(dir.opposite) then
        hooked = false
  end hookEntering

  override def hookExiting(context: MoveContext): Unit =
    import context.*

    for dir <- player.direction do
      if player.attributes(isAboveBridge) then
        if !openings.contains(dir) then
          cancel()
      else
        if openings.contains(dir) then
          cancel()
        else
          hooked = false
  end hookExiting

  override def hookEntered(context: MoveContext): Unit =
    import context.*

    for dir <- player.direction do
      if openings.contains(dir.opposite) then
        player.attributes(isAboveBridge) = true
      else
        player.hide()
        hooked = false
  end hookEntered

  override def hookExited(context: MoveContext): Unit =
    import context.*

    if player.attributes(isAboveBridge) then
      player.attributes(isAboveBridge) = false
    else
      player.show()
      hooked = false
  end hookExited

  override def hookExecute(context: MoveContext): Unit =
    import context.*

    if !player.attributes(isAboveBridge) then
      hooked = false
  end hookExecute
end SimpleBridge
