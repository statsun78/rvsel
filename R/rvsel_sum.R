rvsel_sum <- function(CM,y){
score=abs(cor(y,apply(CM,1,sum)))
return(score)
}
