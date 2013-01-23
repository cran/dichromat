\name{dichromat}
\alias{dichromat}
\encoding{latin1}

\title{Remove Red-Green or Green-Blue Contrasts from Colors}

\description{
  Collapses red-green or green-blue color distinctions to approximate the effect of
  the three forms of dichromacy: protanopia and deuteranopia (red-green color blindness),
  and tritanopia (green-blue color blindness).
  deuteranopia. 
}

\usage{
dichromat(colours, type = c("deutan", "protan", "tritan"))
}

\arguments{
  \item{colours}{A vector of R colors, either color names or color hex strings.}
  \item{type}{Type of color-blindness to simulate,}
}

\details{
  Someone with the specified form of color blindness will
  find that the transformation has little effect on the appearance of
  colors.  Colors that are indistinguishable after transformation were
  likely indistinguishable to them before transformation.  About 10\% of
  men (and almost no women) have some degree of red-green color blindness.
  Tritanopia is much less common but occurs in both males and females.
  
  The mapping from the original color vector to the dichromatic counterpart
  is based on a sequence of papers by Brettel, Mollon, and Viénot. For more
  details, see the references and also the underlying data set \code{\link{dalton}}.
}

\value{
  A vector of R colors.
}

\author{Thomas Lumley}

\references{
Brettel H, Viénot F, Mollon, JD (1997).
  Computerized Simulation of Color Appearance for Dichromats.
  \emph{Journal of the Optical Society of America A}, \bold{14}, 2647--2655.

Lumley T (2006). Color-Coding and Color Blindness in Statistical Graphics.
  \emph{ASA Statistical Computing & Graphics Newsletter}, \bold{17}(2), 4--7.
  \url{http://www.amstat-online.org/sections/graphics/newsletter/Volumes/v172.pdf}

Viénot F, Brettel H, Ott L, M'Barek AB, Mollon JD (1995).
  What Do Colour-Blind People See?
  \emph{Nature}, \bold{376}, 127--128.

Viénot F, Brettel H, Mollon JD (1999).
  Digital Video Colourmaps for Checking the Legibility of Displays by Dichromats.
  \emph{Color Research and Application}, \bold{24}(4), 243--252.

Wikipedia (2013). \emph{Color Blindness -- Wikipedia, The Free Encyclopedia}.
  \url{http://en.wikipedia.org/wiki/Color_blindness}, accessed 2013-01-16.

Wikipedia (2013). \emph{Dichromacy -- Wikipedia, The Free Encyclopedia}.
  \url{http://en.wikipedia.org/wiki/Dichromacy}, accessed 2013-01-16.
}

\seealso{\code{\link{dalton}}, \code{\link[grDevices]{rgb}}}

\examples{
## from example(pie)
 par(mfrow = c(2, 2))
 pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
 names(pie.sales) <- c("Blueberry", "Cherry",
   "Apple", "Boston Cream", "Other", "Vanilla Cream")
 pie(pie.sales, # default colors
   col = c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk"))
 pie(pie.sales,
   col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
 pie(pie.sales, col = dichromat(
   c("white", "lightblue", "mistyrose", "lightcyan", "lavender", "cornsilk")))
 pie(pie.sales, col = dichromat(
   c("purple", "violetred1", "green3", "cornsilk", "cyan", "white")))


## standard color schemes
pie(rep(1,10), col = heat.colors(10))
pie(rep(1,10), col = dichromat(heat.colors(10)))
pie(rep(1,8),  col = palette())
pie(rep(1,8),  col = dichromat(palette()))

pie(rep(1,15), col = topo.colors(15))
pie(rep(1,15), col = dichromat(topo.colors(15)))
pie(rep(1,15), col = terrain.colors(15))
pie(rep(1,15), col = dichromat(terrain.colors(15)))

pie(rep(1,15), col = cm.colors(15))
pie(rep(1,15), col = dichromat(cm.colors(15)))

## color ramp schemes
bluescale <- colorRampPalette(c("#FFFFCC", "#C7E9B4", "#7FCDBB",
  "#40B6C4", "#2C7FB8" , "#253494"))
redgreen <- colorRampPalette(c("red", "green3"))
pie(rep(1,15), col = bluescale(15))
pie(rep(1,15), col = dichromat(bluescale(15)))

par(mfrow = c(2, 4))
x <- matrix(rnorm(10 * 10), 10)
image(1:10, 1:10, x, col = bluescale(10), main = "blue-yellow scale")
image(1:10, 1:10, x, col = dichromat(bluescale(10), "deutan"), main = "deutan")
image(1:10, 1:10, x, col = dichromat(bluescale(10), "protan"), main = "protan")
image(1:10, 1:10, x, col = dichromat(bluescale(10), "tritan"), main = "tritan")

image(1:10, 1:10, x, col = redgreen(10), main = "red-green scale")
image(1:10, 1:10, x, col = dichromat(redgreen(10), "deutan"), main = "deutan")
image(1:10, 1:10, x, col = dichromat(redgreen(10), "protan"), main = "protan")
image(1:10, 1:10, x, col = dichromat(redgreen(10), "tritan"), main = "tritan")
}

\keyword{color}
