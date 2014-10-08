import java.awt._
import java.awt.geom.AffineTransform
import java.awt.image.{DataBufferInt, BufferedImage}
import java.io.{PrintWriter, IOException, File}
import java.util.NoSuchElementException
import javax.imageio.ImageIO
import javax.swing._

import scala.collection.mutable.ArrayBuffer
import scala.collection.{immutable, mutable}

/**
 *
 * Created by Tobin on 10/4/2014.
 */
object AsciiDriver {
  def main(args: Array[String]):Unit = {
    // grab an image from the user
    setBestLAF()
    val inputImage = getImage
    if(inputImage == null) {
      System.exit(0)
    }

    println("Building a char map")
    val charBrightness = buildBrightnessTable
    println("Converting the image")
    val height = 12
    val width = height * 5 / 9
    saveAsciiImage(inputImage, width, width, height, 13.0/16.0, charBrightness, "output.png")
    println("Done")
  }

  /**
   * Set the best available look-and-feel into use.
   */
  def setBestLAF():Unit = {
    /*
     * Set the look-and-feel.  On Linux, Motif/Metal is sometimes incorrectly used
     * which is butt-ugly, so if the system l&f is Motif/Metal, we search for a few
     * other alternatives.
     */
    try {
      // Set system L&F
      UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

      // Check whether we have an ugly L&F
      val laf = UIManager.getLookAndFeel
      if (laf == null || laf.getName.matches(".*[mM][oO][tT][iI][fF].*") ||
        laf.getName.matches(".*[mM][eE][tT][aA][lL].*")) {

        // Search for better LAF
        val info = UIManager.getInstalledLookAndFeels

        val lafNames = Array(".*[gG][tT][kK].*", ".*[wW][iI][nN].*", ".*[mM][aA][cC].*",
          ".*[aA][qQ][uU][aA].*", ".*[nN][iI][mM][bB].*")

        for (lafName <- lafNames) {
          for (l <- info) {
            if (l.getName.matches(lafName)) {
              UIManager.setLookAndFeel(l.getClassName)
              return
            }
          }
        }
      }
    }
    catch {
      case e:Exception => System.err.println("Error setting LAF: " + e)
    }
  }

  /**
   * Gets an image from a file using a FileDialog.  This implementation
   * provides image previews, but no file filtering.  The user can select any
   * file, even a non image file and press ok.  If this happens there is an
   * error message and the user is asked to select a valid image file.  If the
   * user presses cancel then this will return null.
   * @return The image, or null if the user pressed cancel
   */
  def getImage:BufferedImage = {
    val chooser = new FileDialog(null:Frame, "Open Image", FileDialog.LOAD)
    chooser.setVisible(true)
    // The user pressed cancel
    if(chooser.getFile == null)
      return null

    val f = new File(chooser.getDirectory + chooser.getFile)
    chooser.dispose()
    // Try to read the file, and loop this method if there is an error
    try {
      ImageIO.read(f)
    }
    catch {
      case ioe: IOException =>
        JOptionPane.showMessageDialog(null, "Error reading image.  Choose a valid image.", "Image", JOptionPane.ERROR_MESSAGE)
        getImage
    }
  }

  /**
   * Builds a mapping from the number of colored pixels to a character. In the case of a tie the character with the
   * highest spread is used.
   * @return The character mapping
   */
  def buildBrightnessTable:Map[Int, Char] = {
    val h = 12
    val w = (h * 5) / 9
    val cursorHeight = (h * 13) / 16

    val i = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    val g = i.createGraphics

    val f = new Font("Monospaced", Font.PLAIN, h)
    println("\n" + f.getFontName)
    g.setFont(f)

    val variance = mutable.HashMap.empty[Char,Double]
    var output = Map[Int,Char]()

    for(char:Char <- ' ' to '~')
    {
      g.setColor(Color.WHITE)
      g.fillRect(0, 0, w, h)

      g.setColor(Color.BLACK)
      g.drawString(char.toString, 0, cursorHeight)

      // find the variance of the character
      val locations = whereMatch(i, 0xFF000000)
      var brightness = 0
      if(locations.isEmpty)
      {
        brightness = 0
        variance.put(char, 0)
      }
      else
      {
        val points = locations.length
        val avg = locations.foldLeft((0, 0))((sum, loc) => tuple2Op(sum, loc, (t:Int,l:Int) => l/points + t))
        val sumSqDiffs = locations.foldLeft((0, 0))((sumd, loc) => tuple2Op(sumd, tuple2Op(loc, avg, (_: Int) - (_: Int)),
          (s: Int, diff: Int) => (diff * diff)/points + s))
        val l2variance = math.sqrt(math.pow(sumSqDiffs._1, 2) + math.pow(sumSqDiffs._2, 2))

        brightness = locations.length
        variance.put(char, l2variance)
      }

      if(!output.contains(brightness) || variance(char) > variance(output(brightness)))
        output += (brightness -> char)
    }
    g.dispose()

    output
  }

  /**
   * Performs a binary operation, element-wise, on a tuple of two identical things.
   * equivalent to ((a._1 op b._1), (a._2 op b._2))
   * @param a The first tuple
   * @param b The second tuple
   * @param op The operator
   * @tparam T The type of things in the tuples
   * @return A tuple with the result of the operation
   */
  def tuple2Op[T](a:(T,T), b:(T,T), op:(T,T) => T):(T,T) = (op(a._1, b._1), op(a._2, b._2))

  /**
   * Gets a list of locations in the given image that match the given color. This is used to find where a character
   * is drawn onto some background image.
   * @param image The image to test
   * @param color The color to look for
   * @return The locations of all instances of the given color in the given image
   */
  def whereMatch(image:BufferedImage, color:Int):immutable.List[(Int, Int)] = {
    val pixelBytes = image.getRaster.getDataBuffer.asInstanceOf[DataBufferInt].getData
    val width = image.getWidth
    var locations = immutable.List[(Int,Int)]()
    var row = 0
    var col = 0
    for(pixel <- 0 until pixelBytes.length) {
      if(pixelBytes(pixel) == color) {
        locations = (row,col) :: locations
      }
      col += 1
      if(col == width) {
        col = 0
        row += 1
      }
    }
    locations
  }

  /**
   * Saves the given image as ascii. If charWidth is not a multiple of binWidth
   * there may be some stretching due to the requirement that all bins have the
   * same number of pixels.
   *
   * @param image The image to convert
   * @param binWidth The width of a single character on the input image, in pixels
   * @param charWidth The width of a character
   * @param charHeight The height of a character
   * @param charBase The ratio of the height of a character that the baseline appears
   * @param charMap The mapping of colors to characters
   * @param fileName The name of the output file (should end with .png)
   */
  def saveAsciiImage(image:BufferedImage, binWidth:Int, charWidth:Int, charHeight:Int, charBase:Double,
                     charMap:Map[Int,Char], fileName:String) =
  {
    // scale the image so the bins work out
    val binHeight = binWidth * charHeight / charWidth
    val charsWide = (image.getWidth + binWidth/2) / binWidth
    val charsTall = (image.getHeight + binHeight/2) / binHeight
    val bw = bwImage(image)
    val normalImage = scaleImage(bw, charsWide * binWidth, charsTall * binHeight)

    savePng(normalImage, "normal.png")

    println("\tNormalized: " + normalImage.getWidth + " " + normalImage.getHeight)
    println("\tChar Size: " + charWidth + " " + charHeight)
    println("\tBins: " + binWidth + " " + binHeight)
    println("\tChars: " + charsWide + " " + charsTall)
    println("\tChars * Size: " + (charsWide * charWidth) + " " + (charsTall * charHeight))
    println("\tAspect Change: " + ((charsWide * charWidth) / (charsTall * charHeight).asInstanceOf[Double]) + " -> " + (image.getWidth / image.getHeight.asInstanceOf[Double]))

    val charVals = getBinValues(normalImage, charsWide, charsTall, binWidth, binHeight, charMap)

    println("\tBuilding output image")

    // make the output image to draw on
    val out = new BufferedImage(charsWide*charWidth, charsTall*charHeight, BufferedImage.TYPE_INT_ARGB)
    val g = out.createGraphics()
    g.setColor(Color.WHITE)
    g.fillRect(0, 0, out.getWidth, out.getHeight)
    g.setColor(Color.BLACK)
    g.setFont(new Font("Monospaced", Font.PLAIN, charHeight))

    // make an output string
    val outs = new StringBuilder

    val yOffset = (charHeight * charBase).asInstanceOf[Int]

    for (i <- 0 until charsWide*charsTall) {
      val x = i % charsWide
      val y = i / charsWide

      val char = getClosestChar(charVals(i), charMap)

      // draw the character at the right spot
      g.drawString(char.toString, x*charWidth, y*charHeight + yOffset)

      outs += char
      if(x == charsWide - 1)
        outs += '\n'
    }

    println("\tSaving text")
//    println(outs.toString())
    val textFile = new PrintWriter(new File("output.txt"))
    textFile.write(outs.toString())
    textFile.close()

    println("\tSaving image")

    // save the image
    savePng(out, fileName)
  }

  /**
   * Gets a normalized greyscale value for each bin. The values are all
   * normalized to the maximum value appearing in the given character mapping.
   *
   * @param normalImage The image to inspect
   * @param charsWide The number of bins in a row
   * @param charsTall The number of bins in a column
   * @param binWidth The width of a single bin
   * @param binHeight The height of a single bin
   * @param charMap The map from greyscale values to characters
   * @return A list of normalized values
   */
  def getBinValues(normalImage:BufferedImage, charsWide:Int, charsTall:Int, binWidth:Int, binHeight:Int, charMap:Map[Int,Char]):mutable.ArrayBuffer[Double] = {
    // build a list of normalized darknesses
    var maxDarkness = -1.0
    var charVals = new ArrayBuffer[Double](charsWide * charsTall)
    for (i <- 0 until charsWide*charsTall) {
      val x = i % charsWide
      val y = i / charsWide

      val pixels = normalImage.getRGB(x*binWidth, y*binHeight, binWidth, binHeight, null, 0, binWidth).map(bwColor => 0xFF - (bwColor & 0xFF))

      val avg = pixels.sum / pixels.length.asInstanceOf[Double]

      if(avg > maxDarkness)
        maxDarkness = avg

      charVals += avg
    }
    val maxKey = charMap.keys.max
    charVals = charVals.map(x => x / maxDarkness * maxKey)
    charVals
  }

  /**
   * Saves the given image as a png with the given filename. The filename should end with .png
   * If there is an error writing the file an error message is printed to the console.
   * @param image The image to save
   * @param name The name of the output file
   */
  def savePng(image:BufferedImage, name:String) = {
    try {
      if(!ImageIO.write(image, "png", new File(name)))
        println("No Writer Found!")
    } catch {
      case ioe:IOException => println(ioe)
      case e:Throwable => println(e)
    }
  }

  /**
   * Gets the closest char in the given char mapping.
   * @param darkness The requested darkness
   * @param charMap A mapping of darknesses to characters
   * @return The closest available character to darkness
   */
  def getClosestChar(darkness:Double, charMap:Map[Int,Char]):Char = {
    val target = darkness.round.asInstanceOf[Int]
    for(offset <- 0 to 255) {
      // TODO: fix the ordering here
      getOffsetChar(target, offset, charMap) match {
        case Some(c) => return c
        case None =>
      }
    }
    throw new NoSuchElementException()
  }

  def getOffsetChar(darkness:Int, offset:Int, charMap:Map[Int,Char]):Option[Char] = {
    try { Some(charMap(darkness + offset)) }
    catch {
      case nsee: NoSuchElementException => try { Some(charMap(darkness - offset)) }
        catch {
          case nsee: NoSuchElementException => None
        }
    }
  }

  /**
   * Scales a BufferedImage to the given size.
   * @param in The image to scale.
   * @param width The width of the resulting image
   * @param height The height of the resulting image
   * @return The scaled image
   */
  def scaleImage(in:BufferedImage, width:Int, height:Int):BufferedImage = {
    val out = new BufferedImage(width, height, in.getType)
    val g = out.createGraphics()
    setHighRenderingHints(g)
    g.drawRenderedImage(in, AffineTransform.getScaleInstance(width/in.getWidth.asInstanceOf[Double], height/in.getHeight.asInstanceOf[Double]))
    g.dispose()
    out
  }

  /**
   * Transforms an image to greyscale.
   * @param in The image to transform
   * @return The greyscale version of the given image
   */
  def bwImage(in:BufferedImage):BufferedImage = {
    val out = new BufferedImage(in.getWidth, in.getHeight, BufferedImage.TYPE_BYTE_GRAY)
    val g = out.createGraphics()
    setHighRenderingHints(g)
    g.drawImage(in, 0, 0, null)
    out
  }

  /**
   * Sets rendering hints for the given graphics to high quality.
   * @param g The graphics object to set the hints
   */
  def setHighRenderingHints(g:Graphics2D) = {
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setRenderingHint(RenderingHints.KEY_COLOR_RENDERING, RenderingHints.VALUE_COLOR_RENDER_QUALITY)
    g.setRenderingHint(RenderingHints.KEY_INTERPOLATION, RenderingHints.VALUE_INTERPOLATION_BICUBIC)
    g.setRenderingHint(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY)
  }
}
