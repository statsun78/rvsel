rvsel_backward <- function(x,y,method,lambda){

p = ncol(x)

	out3 = rep(1,p)
	pre.max = NULL

	for(step.i in 1:p){

	if(step.i!=1){pre.max <- post.max}

	deleted = which(out3 < 0)

	if(step.i==1){CB=rep(1,p)
	} else if(step.i==2){CB=matrix(1,p,p)-diag(step.i,p)
	} else {CB=matrix(1,p,p)-diag(step.i,p);CB[,deleted]=rep(out3[deleted],each=p);CB=CB[-deleted,]}

	if(is.vector(CB)){CB=t(as.matrix(CB))}

	step.score=rep(NA,nrow(CB))

		for(k in 1:nrow(CB)){
		stepCM = as.matrix(x[,which(CB[k,]==1)])
			if(method=="cmc"){step.score[k]=rvsel_cmc(stepCM,y)
			} else {step.score[k]=rvsel_sum(stepCM,y)}

		}
	post.max = max(step.score)

	if(step.i!=1){if(post.max - pre.max < lambda ) break}

	out3 = CB[which.max(step.score),]
	out2 = post.max
	out1 = as.numeric(out3 > 0)
	}
out3 = which(out3<0)[order(out3[which(out3<0)],decreasing=T)]

return(list(selection=out1,score=out2,sequence=out3))
}