package tutorial.webapp

import scala.scalajs.js.JSApp
import Math._
import org.scalajs.dom
import dom.document
import dom.html

case class Point(x: Int, y: Int)
case class Rect2D(x: Int, y: Int, width: Int, height: Int)
case class Player(current: Rect2D, will: Option[Rect2D])
case class Wood(current: Rect2D)

object TutorialApp extends JSApp {

	val canvasTop    = "50"
	val canvasLeft   = "20"

	val tileSize = 24

	val fieldWidth     = tileSize * 23
	val fieldHeight    = tileSize * 13

	val playerWidth    = tileSize
	val playerHeight   = tileSize

	val movingInterval = tileSize

	val woodLength         = 4
	val woodWidthAndHeight = tileSize

	// player object
	var player:Player = {
		// center
		val initialPosition:Point = Point(fieldWidth / 2 - playerWidth / 2, fieldHeight / 2 - playerHeight / 2)
		Player(Rect2D(initialPosition.x, initialPosition.y, playerWidth, playerHeight), None)
	}

	// wood object
	val woods:Seq[Wood] = {

		val baseX = tileSize * 3
		val baseY = tileSize * 2

		val leftTop     = Point(baseX, baseY)
		val leftBottom  = Point(baseX, fieldHeight - baseY - tileSize)
		val rightTop    = Point(fieldWidth - baseX  - tileSize, baseY)
		val rightBottom = Point(fieldWidth - baseX  - tileSize, fieldHeight - baseY  - tileSize)

		val positions = Seq(leftTop, leftBottom, rightTop, rightBottom)

		positions.map { position =>
			Wood(Rect2D(position.x, position.y, woodWidthAndHeight, woodWidthAndHeight))
		}

	}

	def main() :Unit = {

		// block body scrolling
		document.body.style.overflow = "hidden"

		// gen and draw static background canvas. this will draw only once
		drawBackground()

		val ctx2D:dom.CanvasRenderingContext2D = initCanvas()

		// binding events
		bindingKeyEvent()

		// start render loop
		val famerate = 60
		dom.setInterval(() => { render(ctx2D) }, 1000 / famerate)
	}

	def drawBackground() = {

		val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
		
		document.body.appendChild(canvas)

		canvas.style.position              = "absolute"
		canvas.style.top                   = canvasTop
		canvas.style.left                  = canvasLeft
		canvas.style.zIndex                = "0"
		canvas.width                       = fieldWidth
		canvas.height                      = fieldHeight
		canvas.style.backgroundColor       = "black"

		val ctx2D = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]

		// draw tile grid
		for {
		  x <- 0 to fieldWidth
		  if x % tileSize == 0 
		}  {
			ctx2D.moveTo(0.5 + x, 0)
      ctx2D.lineTo(0.5 + x, fieldHeight)
		}

		for {
		  y <- 0 to fieldHeight
		  if y % tileSize == 0 
		}  {
			ctx2D.moveTo(0, 0.5 + y)
      ctx2D.lineTo(fieldWidth, 0.5 + y)
		}
		
    ctx2D.strokeStyle = "#555"
    ctx2D.stroke()
	}

	def render(ctx2D:dom.CanvasRenderingContext2D): Unit = {

		// draw wood
		woods.foreach { wood =>
			ctx2D.fillStyle= "#33CC33" // green
			ctx2D.fillRect(wood.current.x, wood.current.y, wood.current.width, wood.current.height)
		}

		// calculatePlayerPosition
		// if position will change, clear player rect
		player.will match {
			case Some(newPlayer) => {
				val current = player.current
				ctx2D.clearRect(current.x, current.y, current.width, current.height)
				// change player rect
				player = Player(newPlayer, None)
			}
			case None => 
		}
		
		// draw player
		ctx2D.fillStyle= "#FF0000" // red
		ctx2D.fillRect(player.current.x, player.current.y, player.current.width, player.current.height)

	}

	def initCanvas() :dom.CanvasRenderingContext2D  = {
		val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
		
		document.body.appendChild(canvas)

		canvas.style.position = "absolute"
		canvas.style.top      = canvasTop
		canvas.style.left     = canvasLeft
		canvas.style.zIndex   = "1"
		canvas.width          = fieldWidth
		canvas.height         = fieldHeight
		
		canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
	}

	def bindingKeyEvent(): Unit = {
		document.body.onkeydown = {

			(e: dom.KeyboardEvent) =>
		    val keyCode       = e.keyCode
		    val currentPlayer = player.current

		    val newPlayer = keyCode match {
		    	// up
		    	case 38 => Some(getNewPlayer(currentPlayer, 0, -movingInterval))
		    	// down 
		    	case 40 => Some(getNewPlayer(currentPlayer, 0, movingInterval))
		    	// left
		    	case 37 => Some(getNewPlayer(currentPlayer, -movingInterval, 0))
		    	// right
		    	case 39 => Some(getNewPlayer(currentPlayer, movingInterval, 0))
		    	// other
		    	case _  => None
		    }

		    newPlayer.map { newPlayerRect =>
		    	// check if touching wood
			    if(!isTouchingWood(newPlayerRect)) {
			    	updatePlayer(newPlayer)
			    }
		    }
		}

	}

	def updatePlayer(newPlayer: Option[Rect2D]) = player = Player(player.current, newPlayer)

	private def getNewPlayer(player:Rect2D, xIncrease: Int, yIncrease: Int): Rect2D = {

		val willX = player.x + xIncrease
		val maxX  = fieldWidth - player.width

		val newX = 
			if      (willX > maxX) { maxX } 
			else if (willX < 0)    { 0 } 
			else                   { willX }

		val willY = player.y + yIncrease
		val maxY  = fieldHeight - player.height

		val newY = 
			if      (willY > maxY) { maxY } 
			else if (willY < 0)    { 0 } 
			else                   { willY }

		Rect2D(newX, newY, player.width, player.height)
	}


	private def isTouchingWood(newPlayer: Rect2D): Boolean = {

		def check(wood: Wood): Boolean = {
			val playerRadius = newPlayer.width / 2
			val woodRadius   = wood.current.width / 2

			val woodCenter   = Point(wood.current.x + woodRadius, wood.current.y + woodRadius) 
			val playerCenter = Point(newPlayer.x + playerRadius, newPlayer.y + playerRadius)

			val distanceLeft  = pow((woodCenter.x - playerCenter.x), 2) + pow((woodCenter.y - playerCenter.y), 2)
			val distanceRight = pow((playerRadius + woodRadius), 2)

			if(distanceLeft < distanceRight) {
				true
			} else {
				false
			}
		}

		for {
		  wood <- woods
		  if check(wood) == true
		} return true

		false

	}

}