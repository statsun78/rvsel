rvsel=function(x,y,cx=NULL,weight=NULL,family=c("gaussian","binomial"),method=c("asum","sum","cmc"),
          selection=c("exhaustive","forward","backward","Fsel"),ad.alpha=0.1,lambda=0){
          
   family=match.arg(family)
   method=match.arg(method)
   selection=match.arg(selection)
   rvsel_check(x,y,cx,family,method,selection)
   x=as.matrix(x)
   p=ncol(x)
   x[x!=0]=1
   if(!is.null(cx)){
      cx=as.matrix(cx)
      if(family=="gaussian") y=lm(y~cx)$residuals
      if(family=="binomial") y=residuals(glm(y~cx,family=family),"response")
   }
   if (selection=="exhaustive") {
       result = rvsel_exhaustive(x,y,method,weight,ad.alpha)
       sel.result = result$selection
   }
   else {
       if (is.null(weight)) wts=rep(1,p)
       else wts=as.double(weight)
       x <- t(apply(x,1,function(x) x*wts))
       if (method=="asum") x = rvsel_adapted(x,y,ad.alpha)
       if (selection=="forward") result = rvsel_forward(x,y,method,lambda)
       if (selection=="backward")	result = rvsel_backward(x,y,method,lambda)
       if (selection=="Fsel") result = rvsel_Fsel(x,y,method,lambda)
       sel.result <- cbind(result$selection,wts) 
   }
   colnames(sel.result) <- c("selection", "weights")
   if (is.null(colnames(x))) rownames(sel.result) <- paste("V",1:p,sep="")
   else rownames(sel.result) <- colnames(x)
   model <- paste("family = ",family,", method = ",method,", selection = ",selection,sep="")
   outlist <- list(model=model,selection=sel.result,score=result$score) 
   if (selection!="exhaustive") { 
       outlist <- c(outlist, list(sequence=result$sequence))
   }    
   return(outlist)
}
