pcme <- function(actual, cl){
  x <- table(actual, cl);
  tbl <- cbind(round(x/length(actual), 2),
               Error=round(c(x[1,2]/sum(x[1,]),
                             x[2,1]/sum(x[2,])), 2));
  names(attr(tbl, "dimnames")) <- c("Actual", "Predicted");
  return(tbl);
}