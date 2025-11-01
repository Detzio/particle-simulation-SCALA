import scalafx.animation.{KeyFrame, Timeline}
import scalafx.application.JFXApp3
import scalafx.application.JFXApp3.PrimaryStage
import scalafx.beans.property.ObjectProperty
import scalafx.scene.Scene

import scala.util.Random
import scalafx.stage.Screen
import scalafx.util.Duration

object Main extends JFXApp3 {

  override def start(): Unit = {
    val (screenWidth, screenHeight) = (
      Screen.primary.visualBounds.width.toInt,
      Screen.primary.visualBounds.height.toInt
    )

    val numberOfParticles: Int = 1500

    val particles = ObjectProperty(
      (1 to numberOfParticles).map { _ =>
        Particle(
          x = Random.nextInt(screenWidth),
          y = Random.nextInt(screenHeight)
        )
      }
    )

    val sceneRef = new Scene {
      content = particles.value.map(_.draw)
    }

    stage = new PrimaryStage {
      title = "Particle Simulation"
      width = screenWidth
      height = screenHeight
      scene = sceneRef
    }

    def resolveCollisionsAndMove(current: Seq[Particle]): IndexedSeq[Particle] = {
      val occupied = scala.collection.mutable.Set.empty[(Int, Int)]

      val nextParticles: Seq[Particle] = current.map { p =>
        val preferred = p.nextPositionFor(p.dir, screenWidth, screenHeight)

        if (!occupied.contains(preferred)) {
          occupied.add(preferred)
          p.withPositionAndDir(preferred._1, preferred._2, p.dir)
        } else {
          val shuffled = Random.shuffle(Direction.values.toList)
          val available = shuffled.find { d =>
            val np = p.nextPositionFor(d, screenWidth, screenHeight)
            !occupied.contains(np)
          }

          available match {
            case Some(d) =>
              val np = p.nextPositionFor(d, screenWidth, screenHeight)
              occupied.add(np)
              p.withPositionAndDir(np._1, np._2, d)
            case None =>
              val newDir = shuffled.head
              p.withPositionAndDir(p.x, p.y, newDir)
          }
        }
      }

      nextParticles.toIndexedSeq
    }

    val timeline = new Timeline {
      keyFrames = List(
        KeyFrame(
          time = Duration(25),
          onFinished = _ => {
            particles.value = resolveCollisionsAndMove(particles.value)
            sceneRef.content = particles.value.map(_.draw)
          }
        )
      )
      cycleCount = Timeline.Indefinite
    }

    timeline.play()
  }
}