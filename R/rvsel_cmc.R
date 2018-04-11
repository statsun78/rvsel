rvsel_cmc <- function(CM,y){
score=abs(cor(y,apply(CM,1,sum)!=0))
return(score)
}
