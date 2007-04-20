\name{dalton}
\alias{dalton}
\alias{dalton.colors}
\alias{redp}
\alias{redd}
\alias{greenp}
\alias{greend}
\alias{bluep}
\alias{blued}
\non_function{}
\title{Effects of daltonism (red-green color blindness)}
\description{
  A 256-color palette as it would appear with normal vision, and with
  two types of red-green colorblindness: protanopia and deuteronopia.
}
\usage{data(dalton)}
\format{
  \code{dalton}{  A 256x3x3 array. The columns index the red, green and blue color
    values, the layers index the vision type.}
  \code{dalton.colors}{A data frame of colors, with column names
    \code{normal}, \code{protan}, \code{deutan}.}
}
\source{
\url{http://tsi.enst.fr/~brettel/CRA24/table2.html}
}
\references{
F. Viénot, H. Brettel and J. D. Mollon (1999) Digital video colourmaps for checking the legibility of displays by dichromats. Color Research and Application 24, 243-252.
}
\examples{
data(dalton)
 par(mfrow=c(3,1))
 image(matrix(1:256,128),col=dalton.colors$deutan)
 image(matrix(1:256,128),col=dalton.colors$protan)
 image(matrix(1:256,128),col=dalton.colors$normal)


}
\keyword{datasets}
