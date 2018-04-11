rvsel_adapted <- function(x,y,ad.alpha){
	n=nrow(x)
	coef=apply(x,2,function(one.var) summary(lm(y~one.var))$coef[2,1])
	coef.sign=sign(as.numeric(coef))
	coef.pval=apply(x,2,function(one.var) summary(lm(y~one.var))$coef[2,4])
	
	prot=which(coef.pval<=ad.alpha & coef.sign<0)
  x[,prot]=1-x[,prot]
	out=as.matrix(x)
return(out)
}
