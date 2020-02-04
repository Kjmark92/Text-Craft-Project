


test_api <- function(apilink = ""){

  if (apilink == "yes")  {
c1 <- c("yes","no","maybe")
c2 <- c(1,2,3)
c3 <- c("SD","LA","SF")
df <- as.data.frame(cbind(c1,c2,c3))
}

  else {
  df <- "no"
}  
  return(df)
}
