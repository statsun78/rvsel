rvsel_check <- function(x,y,cx,family,method,selection){

  if (sum(duplicated(x, MARGIN = 2))>0)
    stop("At least one variant is exactly the same as one of the other variants.")

  # x as matrix or data frame
  if(!is.matrix(x) & !is.data.frame(x))
    stop("Argument 'x' must be a matrix or data.frame.")    

  # dimension of x
  if(ncol(x)<2)
    stop("'x' must have more than one variant for selection.")    

  # no misssing values in y
  if (any(is.na(y))) 
    stop("No missing data is allowed in argument 'y'.")	

  # binary values (0, 1) in y
  if (family=="binomial" & !all(y %in% c(0, 1)))
    stop("Argument 'y' must have either 0 or 1 when family is binomial.")

  # compatibility between x and y
  if (nrow(x) != length(y)) 
    stop("'x' and 'y' have different lengths.")

  # results
}
