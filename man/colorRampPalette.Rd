\name{colorRampPalette}
\alias{colorRampPalette}
\alias{colorRamp}
\title{Extend a color palette to a color ramp }
\description{
These functions are useful for converting hand-designed `sequential' or
`diverging' color schemes into continous color ramps eg for image and
filled contour plots. Unfortunately the interpolation is still in RGB space.
}
\usage{
colorRampPalette(palette, bias = 1,method=c("spline","linear"))
colorRamp(palette, bias=1,method=c("spline","linear"))
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{palette}{A vector of colors}
  \item{bias}{Bias >1 puts more colors at high values, <1 puts more at low values}
\item{method}{Interpolation method. Spline interpolation gives better results but can end up outside the valid RGB space with highly saturated palettes or those based on a small starting set.}
}

\value{
\code{colorRampPalette} returns a palette generating function like \code{\link{terrain.colors}} or \code{\link{heat.colors}} that takes an integer argument and generates a palette with that many colors.

\code{colorRamp} returns a color generating function, that takes a number between 0 and 1 and generates the corresponding color.

}
\author{Thomas Lumley}

\seealso{\url{www.colorbrewer.org} }

\examples{
   m<-outer(1:20,1:20,function(x,y) sin(sqrt(x*y)/3))

   redblue<-colorRampPalette(c("red","orange","blue"),method="linear")
   filled.contour(m,color.palette=redblue)

   yb<-colorRampPalette(c("yellow2","goldenrod","darkred"))
   filled.contour(m,color.palette=yb)

   yb<-colorRampPalette(c("yellow2","goldenrod","darkred"),bias=0.5)
   filled.contour(m,color.palette=yb)

   ## Two ColorBrewer sequential palettes
   ylOrBn4<-c("#FFFFD4", "#FED98E", "#FE9929","#CC4C02")
   ylGnBl5<-c("#FFFFCC","#C7E9B4","#7FCDBB","#40B6C4","#2C7FB8" ,"#253494")

   ylOrBn<-colorRampPalette(ylOrBn4,method="linear")
   ylGnBl<-colorRampPalette(ylGnBl5)
   filled.contour(m,color.palette=ylOrBn)
   filled.contour(m,color.palette=ylGnBl)


}
\keyword{color}% at least one, from doc/KEYWORDS
