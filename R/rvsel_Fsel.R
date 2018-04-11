rvsel_Fsel <- function(x,y,method,lambda){

p = ncol(x)

x = cbind(x,1-x)

out3 = rep(0,2*p)
pre.max = NULL

	for(step.i in 1:p){

	if(step.i!=1){pre.max <- post.max}

	if(step.i==1){CB=diag(1,2*p)
	} else {CB=diag(step.i,2*p);CB[,choosed]=rep(out3[choosed],each=2*p);CB=CB[-c(choosed,pair),]}

	if(is.vector(CB)){CB=t(as.matrix(CB))}

	step.score=rep(NA,nrow(CB))

		for(k in 1:nrow(CB)){
		stepCM = as.matrix(x[,which(CB[k,]!=0)])
			if(method=="cmc")	{step.score[k]=rvsel_cmc(stepCM,y)
			} else {step.score[k]=rvsel_sum(stepCM,y)}

		}
	post.max = max(step.score)

	if(step.i!=1){if(post.max - pre.max < lambda ) break}

	out3 = CB[which.max(step.score),]
	choosed = which(out3!=0)

	pair = ifelse(choosed > p, choosed - p, choosed + p)

	out2 = post.max
	}
out3 = out3[1:p] - out3[-(1:p)]
out1 = as.numeric(out3!=0)

out3 = which(out3!=0)[order(abs(out3[which(out3!=0)]))]

return(list(selection=out1,score=out2,sequence=out3))
}