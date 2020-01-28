# Beispiel Code, Funktion um fehlende Daten zu impurtieren

library(mice)
library(mgcv)

# lade Daten
data <- load("/data.RData")

# check missing data
md.pattern(data)

# imputation 

imp <- mice(data,m=1,maxit=1,method="mean")
pred <- imp$predictorMatrix
pred[, "altitude_cat_300"] <- 0

reps <- 31
maxit <- 25
setseed <- 123

start_time <- Sys.time()
impdata <- mice(data,pred=pred,m=reps,maxit=maxit, method=c("norm.nob","norm.nob","norm.nob","pmm","polyreg","pmm","","","",
                                                              "norm.nob","","","","polr","","",""),seed=setseed)

end_time <- Sys.time()
Alltime <- end_time - start_time


# save(impdata,file="/Imputed.RData")



# Funktion sumstatistics() um Ergbenisse der imputierten Daten zusammen zufügen #


sumstatistics <- function(reps,Imp_analysis) {
  
  ## bhat
  bhat <- Imp_analysis$analyses[[1]]$coeff
  for (i in 2:reps){
    bhat<-bhat+Imp_analysis$analyses[[i]]$coeff
  }
  bhat <- bhat/reps
  
  # b
  W <- Imp_analysis$analyses[[1]]$Vp
  for (i in 2:reps){
    W <- W+Imp_analysis$analyses[[i]]$Vp
  }
  W <- W/reps
  B <-  (Imp_analysis$analyses[[1]]$coeff-bhat) %*% t(Imp_analysis$analyses[[1]]$coeff-bhat)
  for (i in 2:reps){
    B <- B+(Imp_analysis$analyses[[i]]$coeff-bhat) %*% t(Imp_analysis$analyses[[i]]$coeff-bhat)
  }
  B <- B/(reps-1)
  
  # Vb
  Vb <- W+(1+1/reps)*B
  
  # dfr
  dfr <- Imp_analysis$analyses[[1]]$df.residual
  for (i in 2:reps){
    dfr <- dfr+Imp_analysis$analyses[[i]]$df.residual
  }
  dfr <- dfr/reps
  
  MI <-  Imp_analysis$analyses[[1]]
  
  MI$coefficients <- bhat
  MI$Vp <- Vb
  MI$df.residual <-  dfr
  return(MI)
}


# Regression mit imputieren Modeln #


Model1 <- with(impdata,gam(VO2max~s(altitude,k=20)))
Model1.Results <- sumstatistics(reps,Model1) 
summary(Model1.Results)
Table.Results <- data.frame(summary(Model1.Results)$s.table)

# write.table(Table.Results,"/Table.Results.xls",sep="\t")


