\name{dalton}
\alias{dalton}
\alias{dalton.colors}
\encoding{latin1}

\title{Effects of Daltonism (Red-Green Color Blindness)}

\description{
  A 256-color palette as it would appear with normal vision, and with
  two types of red-green color blindness: protanopia and deuteronopia.
  Furthermore, a color palette for a type of green-blue color blindness
  (tritanopia) is available.
}

\usage{data("dalton")}

\format{
  \code{dalton} A 256 x 3 x 4 array. The columns index the red, green and blue color
    values, the layers index the vision type.
  
  \code{dalton.colors}: A list of colors, with names
    \code{normal}, \code{protan}, \code{deutan}, \code{tritan}.
}

\details{
  The \code{dalton} array lists a grid of 256 colors in RGB coordinates for
  \code{"normal"} vision as well as corresponding RGB coordinates in which
  certain red-green (\code{"deutan"} and \code{"protan"}) or green-blue
  contrasts (\code{"tritan"}) are collapsed.
  
  The formulas for mapping the RGB coordinates to the collapsed coordinates
  have been developed in a series of papers by Brettel, Mollon, and Viénot.
  The protan/deutan mapping is provided in Table 2 of Viénot et al. (1999)
  while the tritan case is discussed in Brettel at al. (1997).
  
  The \code{dalton.colors} list contains the sRGB colors (as R color strings)
  as computed with \code{\link[grDevices]{rgb}} from \code{dalton}.
  
  For an automatic mapping of a given color vector to its dichromatic
  counterpart, see \code{\link{dichromat}}.
}

\source{
The data were kindly provided by Françoise Viénot.

The deutan/protan data (rounded to integers) is also available in Table 2 of
Viénot et al. (1999). See also
\url{http://tsi.enst.fr/~brettel/CRA24/table2.html}.
}

\references{
Brettel H, Viénot F, Mollon, JD (1997).
  Computerized Simulation of Color Appearance for Dichromats.
  \emph{Journal of the Optical Society of America A}, \bold{14}, 2647--2655.

Viénot F, Brettel H, Ott L, M'Barek AB, Mollon JD (1995).
  What Do Colour-Blind People See?
  \emph{Nature}, \bold{376}, 127--128.

Viénot F, Brettel H, Mollon JD (1999).
  Digital Video Colourmaps for Checking the Legibility of Displays by Dichromats.
  \emph{Color Research and Application}, \bold{24}(4), 243--252.
}

\seealso{\code{\link{dichromat}}, \code{\link[grDevices]{rgb}}}

\examples{
 data("dalton", package = "dichromat")
 par(mfrow = c(4, 1))
 image(matrix(1:256, 128), col = dalton.colors$normal)
 image(matrix(1:256, 128), col = dalton.colors$deutan)
 image(matrix(1:256, 128), col = dalton.colors$protan)
 image(matrix(1:256, 128), col = dalton.colors$tritan)
}

\keyword{datasets}
