\name{dichromat}
\alias{dichromat}
%- Also NEED an `\alias' for EACH other topic documented here.
\title{Remove red-green distinctions from colours}
\description{
  Collapses red-green color distinctions to approximate the effect of
  the two common forms of red-green colour blindness, protanopia and
  deuteranopia. 
}
\usage{
dichromat(colours, type = c("deutan", "protan"))
}
%- maybe also `usage' for other objects documented here.
\arguments{
  \item{colours}{A vector of R colors, either color names or color hex strings}
  \item{type}{Type of color-blindness to simulate}
}
\details{
  Someone with the specified form of red-green colour blindness will
  find that the transformation has little effect on the appearance of
  colours.  Colours that are indistinguishable after transformation were
  likely indistinguishable to them before transformation.  About 10\% of
  men (and almost no women) have red-green colour blindness.
}
\value{
  A vector of R colors
}
\author{Thomas Lumley}
\references{
F. Vi�not, H. Brettel and J. D. Mollon (1999) Digital video colourmaps for checking the legibility of displays by dichromats. Color Research and Application 24, 243-252.
}
\examples{
## from example(pie)
 par(mfrow=c(2,2))
 pie.sales <- c(0.12, 0.3, 0.26, 0.16, 0.04, 0.12)
 names(pie.sales) <- c("Blueberry", "Cherry",
         "Apple", "Boston Cream", "Other", "Vanilla Cream")
 pie(pie.sales) # default colours
 pie(pie.sales,
       col = c("purple", "violetred1", "green3", "cornsilk", "cyan", "white"))
 pie(pie.sales,col=dichromat( c("white", "lightblue", "mistyrose", "lightcyan", 
                "lavender", "cornsilk"))) # default colours
 pie(pie.sales,
       col =dichromat(c("purple", "violetred1", "green3", "cornsilk", "cyan", "white")))


## standard color schemes
pie(rep(1,10),col=heat.colors(10))
pie(rep(1,10),col=dichromat(heat.colors(10)))
pie(rep(1,8),col=palette())
pie(rep(1,8),col=dichromat(palette()))

pie(rep(1,15),col=topo.colors(15))
pie(rep(1,15),col=dichromat(topo.colors(15)))
pie(rep(1,15),col=terrain.colors(15))
pie(rep(1,15),col=dichromat(terrain.colors(15)))

pie(rep(1,15),col=cm.colors(15))
pie(rep(1,15),col=dichromat(cm.colors(15)))

bluescale<-function(n) rev(rainbow(n,1,1,1/6,4/6,gamma=0.5))
pie(rep(1,15),col=bluescale(15))
pie(rep(1,15),col=dichromat(bluescale(15)))

image(1:20,1:20,outer(1:20,1:20,"+"),col=bluescale(20))
image(1:20,1:20,outer(1:20,1:20,"+"),col=dichromat(bluescale(20)))
image(1:20,1:20,outer(1:20,1:20,"+"),col=dichromat(bluescale(20),"protan"))

}
\keyword{color}% at least one, from doc/KEYWORDS
