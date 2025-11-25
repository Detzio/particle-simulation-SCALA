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
      val collisionRadius = 6.5
      val nextPositions = current.map(p => p -> p.nextPositionFor(p.dir, screenWidth, screenHeight))
      val result = scala.collection.mutable.Map.empty[Particle, Particle]

      for ((particle, newPos) <- nextPositions) {
        if (!result.contains(particle)) {
          val willCollide = current.exists { other =>
            other != particle &&
            !result.contains(other) &&
            distance(newPos._1, newPos._2, other.x, other.y) <= collisionRadius
          }

          if (willCollide) {
            result(particle) = particle.withPositionAndDir(particle.x, particle.y, particle.dir.reverse)
          } else {
            result(particle) = particle.withPositionAndDir(newPos._1, newPos._2, particle.dir)
          }
        }
      }

      current.map(result.apply).toIndexedSeq
    }

    def distance(x1: Int, y1: Int, x2: Int, y2: Int): Double = {
      Math.sqrt(Math.pow(x2 - x1, 2) + Math.pow(y2 - y1, 2))
    }

    val timeline = new Timeline {
      keyFrames = List(
        KeyFrame(
          time = Duration(50),
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