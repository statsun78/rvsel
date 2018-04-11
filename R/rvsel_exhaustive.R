rvsel_exhaustive<-function(x,ry,method=c("asum","sum","cmc"),weight,ad.alpha) {
   method=match.arg(method)
   if (method=="asum") adapted=TRUE 
   else adapted=FALSE
   x<-as.matrix(x)
   n<-as.integer(nrow(x))
   p<-as.integer(ncol(x))
   lq=as.integer(2^p-1)
   jerr=as.integer(0)
   re=rep(0,p)
   re[1]<-1
   y=as.double(scale(ry))
   zs=as.double(0)
   x[x!=0]<-1
   if (method=="cmc") {
      h<-.Fortran("maxcorr2",n,p,as.double(x),y,re=as.integer(re),lq,zs=zs,
                      out=as.double(re),jerr=jerr)
      if (!is.null(weight)) warning("The method 'cmc' cannot have weights.")
      wts=as.double(rep(1,p))
   }   
   else {
     if (is.null(weight)) wts=as.double(rep(1,p))
     else wts=as.double(weight)
     if (adapted) {
        coef<-apply(x,2,function(w) summary(lm(y~w))$coef[2,1])
        pval<-apply(x,2,function(w) summary(lm(y~w))$coef[2,4])
        sgn<-sign(as.numeric(coef))
        ww<-which(pval<=ad.alpha)
        if (length(ww)>0) {
            wp<-which(sgn[ww]<0)
          if (length(wp)>0) x[1:n,ww[wp]]<-1-x[1:n,ww[wp]]
        }
     }
     h<-.Fortran("maxcorr",n,p,as.double(x),y,re=as.integer(re),lq,wts,zs=zs,
                      out=as.double(re),jerr=jerr)
   }  
   jerr<-h$jerr
   if (h$jerr==0) selection=as.vector(h$out)
   else stop("Memorry allocation errors from Fortan code. Try with a different selection method.")
   sel.result <- cbind(selection, wts)
return(list(selection=sel.result,score=h$zs,jerr=h$jerr))
}
