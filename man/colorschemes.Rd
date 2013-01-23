\name{colorschemes}
\alias{colorschemes}

\title{Color Schemes}

\description{
17 color schemes suitable for people with deficient or anomalous
red-green vision.
}

\usage{colorschemes}

\format{
 \itemize{
 \item BrowntoBlue.10 
 \item BrowntoBlue.12 
 \item BluetoDarkOrange.12
 \item BluetoDarkOrange.18
 \item DarkRedtoBlue.12   
 \item DarkRedtoBlue.18   
 \item BluetoGreen.14     
 \item BluetoGray.8       
 \item BluetoOrangeRed.14   
 \item BluetoOrange.10      
 \item BluetoOrange.12      
 \item BluetoOrange.8       
 \item LightBluetoDarkBlue.10
 \item LightBluetoDarkBlue.7 
 \item Categorical.12        
 \item GreentoMagenta.16     
 \item SteppedSequential.5
 }
}

\author{Scott Waichler <scott.waichler@pnl.gov>}

\source{
\url{http://geography.uoregon.edu/datagraphics/color_scales.htm}
}

\references{
Light A, Bartlein PJ (2004).
  The End of the Rainbow? Color Schemes for Improved Data Graphics.
  \emph{EOS Transactions of the American Geophysical Union}, \bold{85}(40), 385.
}

\examples{
pal <- function(col, ...)
  image(seq_along(col), 1, matrix(seq_along(col), ncol = 1),
  col = col, axes = FALSE, ...)

opar <- par(mar = c(1, 2, 1, 1))
layout(matrix(1:6, ncol = 1))
pal(colorschemes$BrowntoBlue.10, main = "Brown to Blue (10)")
pal(colorRampPalette(colorschemes$BrowntoBlue.10, space = "Lab")(100),
  main = "Brown to Blue Ramp")
pal(dichromat(colorschemes$BrowntoBlue.10),
  main = "Brown to Blue (10) -- deuteranopia")
pal(colorschemes$Categorical.12, main = "Categorical (12)")
pal(dichromat(colorschemes$Categorical.12),
  main = "Categorical (12) -- deuteranopia")
pal(dichromat(colorschemes$Categorical.12, "protan"),
  main = "Categorical (12) -- protanopia")
par(opar)
}

\keyword{datasets}
