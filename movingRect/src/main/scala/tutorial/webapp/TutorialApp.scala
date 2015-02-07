package tutorial.webapp

import scala.scalajs.js.JSApp
import org.scalajs.dom
import dom.document
import dom.html

case class Point(x: Int, y: Int)
case class Rect2D(x: Int, y: Int, width: Int, height: Int)
case class Player(current: Rect2D, will: Option[Rect2D])

object TutorialApp extends JSApp {

	val fieldWidth     = 480
	val fieldHeight    = 360

	val playerWidth    = 25
	val playerHeight   = 25

	val movingInterval = 50

	// player object
	var player:Player = {
		// center
		val initialPosition:Point = Point(fieldWidth / 2 - playerWidth / 2, fieldHeight / 2 - playerHeight / 2)
		Player(Rect2D(initialPosition.x, initialPosition.y, playerWidth, playerHeight), None)
	}

	def main() :Unit = {

		val ctx2D:dom.CanvasRenderingContext2D = initCanvas()

		// binding events
		bindingKeyEvent()

		// start render loop
		val famerate = 60
		dom.setInterval(() => { render(ctx2D) }, 1000 / famerate)
	}

	def render(ctx2D:dom.CanvasRenderingContext2D): Unit = {
		
		// render player
		ctx2D.fillStyle= "#FF0000" // red

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
		ctx2D.fillRect(player.current.x, player.current.y, player.current.width, player.current.height)
	}

	def initCanvas() :dom.CanvasRenderingContext2D  = {
		val canvas = dom.document.createElement("canvas").asInstanceOf[html.Canvas]
		
		document.body.appendChild(canvas)

		canvas.width                 = fieldWidth
		canvas.height                = fieldHeight
		canvas.style.backgroundColor = "black"

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

		    updatePlayer(newPlayer)
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

}