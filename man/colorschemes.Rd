\name{colorschemes}
\alias{colorschemes}
\docType{data}
\title{Color schemes}
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
\author{R code by Scott Waichler <scott.waichler@pnl.gov>}

\source{
Light A. and P.J. Bartlein, 2004.  "The end of the rainbow?  Color schemes
for improved data graphics," EOS Transactions of the American Geophysical Union
85(40):385.
}
\references{
\url{http://geography.uoregon.edu/datagraphics/color_scales.htm}
}
\examples{
opar <- par(mar=c(1,2,1,1))
layout(matrix(1:6,ncol=1))
image(1:10,1,matrix(1:10, ncol=1),col=colorschemes$BrowntoBlue.10,
    main="Brown to Blue (10)", axes=FALSE)
image(1:100,1,matrix(1:100 ,ncol=1),col=colorRampPalette(colorschemes$BrowntoBlue.10,space="Lab")(100),
    main="Brown to Blue Ramp", axes=FALSE)
image(1:10,1,matrix(1:10, ncol=1),col=dichromat(colorschemes$BrowntoBlue.10),
   main="Brown to Blue (10) -- deuteranopia", axes=FALSE)
image(1:12,1,matrix(1:12, ncol=1),col=colorschemes$Categorical.12,main="Categorical (12)", axes=FALSE)
image(1:12,1,matrix(1:12, ncol=1),col=dichromat(colorschemes$Categorical.12),
   main="Categorical (12) -- deuteranopia", axes=FALSE)
image(1:12,1,matrix(1:12,ncol=1),col=dichromat(colorschemes$Categorical.12, "protan"),
   main="Categorical (12) -- protanopia", axes=FALSE)
par(opar)
}


}
\keyword{datasets}
