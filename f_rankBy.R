rankBy <- function(df, svn, gpv="sic") {
  df$index <- 1:dim(df)[1]
  df <- df[order(df[, gpv]),]
  df$rnk <- unlist(by(df, df[,gpv], 
                      function(x) {
                        grp.cnt <- sum(complete.cases(x))
                        if (grp.cnt<2) {
                          0
                        } else {
                          rk <- rank(x[,svn], ties.method = "random", na.last = "keep")/grp.cnt
                          net.rk <- rk - median(rk, na.rm = T)
                          net.rk*(net.rk>0)/sum(net.rk*(net.rk>0), na.rm=T)-net.rk*(net.rk<0)/sum(net.rk*(net.rk<0), na.rm=T)
                          
                        }
                      }
  ))
  df <- df[order(df$index),]
  return(df$rnk)
}

# df <- as.data.frame(cbind(sample(1:14), c(1,1,1,2,2,2,2,3,3,3,3,3,4,5),c(5,3,8,18,14,14,17,20,28,24,20,NA,1,NA)))
# names(df) <- c("permno", "sic", "yhat")
# df[order(df$permno),]
# df$w <- rankBy(df, "yhat")
# print(df[order(df$sic),])


# df <- df[order(df$sic),]
# df$rnk <- unlist(by(df, df[,"sic"], 
#                     function(x) {
#                       grp.cnt <- sum(complete.cases(x))
#                       if (grp.cnt<2) {
#                         0
#                       } else {
#                         rk <- rank(x[,"yhat"], ties.method = "random", na.last = "keep")/grp.cnt
#                         net.rk <- rk - median(rk, na.rm = T)
#                         net.rk*(net.rk>0)/sum(net.rk*(net.rk>0), na.rm=T)-net.rk*(net.rk<0)/sum(net.rk*(net.rk<0), na.rm=T)
#                         
#                       }
#                     }
#                     ))
# df <- df[order(df$permno),]
# 
# df2 <- df %.% group_by(sic) 
# 
# # %>% group_by(df[,"sic"]) %>% 
# #               mutate(rk = rank(df[, "yhat"], ties.method = "random", na.last = "keep"))
#   
#   