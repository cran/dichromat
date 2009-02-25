
dichromat<- function(colours,type=c("deutan","protan")){
    ##data(dichromat, envir=environment())
    require("stats")
    colours<-col2rgb(colours)
    colours<-t(colours)
    colnames(colours)<-c("r","g","b")
    type<-match.arg(type)
    
    if(type=="deutan"){
        nred<-predict(redd,newdata=colours)
        ngreen<-predict(greend,newdata=colours)
        nblue<-predict(blued,newdata=colours)
    } else if (type=="protan"){
        nred<-predict(redp,newdata=colours)
        ngreen<-predict(greenp,newdata=colours)
        nblue<-predict(bluep,newdata=colours)
    }
    
    nred<-pmax(0,pmin(1,nred/255))
    ngreen<-pmax(0,pmin(1,ngreen/255))
    nblue<-pmax(0,pmin(1,nblue/255))
        rgb(nred,ngreen,nblue)
    
}
