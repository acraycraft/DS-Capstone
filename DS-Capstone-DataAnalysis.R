setwd("C:/Users/ashto/Documents/Wellesley/2023_Spring/DS Capstone/Data")

# reading the data
bos_clean <- read.csv('bos_clean.csv',header=TRUE)
sf_clean <- read.csv('sf_clean.csv',header=TRUE)
dc_clean <- read.csv('dc_clean.csv',header=TRUE)

#cross-validation function
cross_val <- function(d) {
  CV1<- 0
  n <- dim(d)[1]
  set.seed(54)
  n.shuffle = sample(1:n,n,replace=FALSE)
  id.cv <- list()
  id.cv[[1]] <- n.shuffle[1:141]
  id.cv[[2]] <- n.shuffle[142:282]
  id.cv[[3]] <- n.shuffle[283:423]
  id.cv[[4]] <- n.shuffle[424:564]
  id.cv[[5]] <- n.shuffle[565:709]
  
  for(i in 1:5)
  {
    fit1 <- lm(formula = Price ~ Review.Scores.Cleanliness + Beds + Bathrooms + 
                 Reviews.per.Month + Room.Type + Neighbourhood.Cleansed + 
                 Cancellation.Policy,
               data = d[-id.cv[[i]],])
    CV1 <- CV1 + (1/n)*sum(d$Price[id.cv[[i]]] - predict(fit1, d[id.cv[[i]],]))^2
  }
  return(CV1)
}


# BOSTON WITH NEIGHBORHOOD ==================================================
  # modeling AIC and BIC
  bos.lm.all = lm(Price~., data=bos_clean)
  step(bos.lm.all, direction="both") #aic backward
  step(bos.lm.all, direction="both", k=log(nrow(bos_clean))) #bic backward
  
  # models
  bos.lm.best = lm(formula = Price ~ Review.Scores.Cleanliness + Minimum.Nights + 
                 Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                 Room.Type + Neighbourhood.Cleansed + Cancellation.Policy, 
                 data = bos_clean)
  
  # checking the residual plot
  dev.off()
  par(mfrow=c(2,2))
  plot(bos.lm.best)
  
  # applying box-cox
  library(MASS)
  dev.off()
  boxcox(bos.lm.best)
  bos.lm.trans = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Minimum.Nights + 
                      Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                      Room.Type + Neighbourhood.Cleansed + Cancellation.Policy, 
                      data = bos_clean)
  
  # finding influential outlying observations
  par(mfrow=c(2,2))
  plot(bos.lm.trans)
  bos.res=cooks.distance(bos.lm.trans)
  
  bos.res[1787] #1217 from graph, observation #1787
  bos.res[2078] #2084 from graph, observation #2078
  bos.res[315] #317 from graph, observation #315
  bos.res[1217] #1220 from graph, observation #1217
  bos.res[2260] #2266 from graph, observation #2260
  
  
  bos_clean[c(1787, 2078, 315, 1217, 2260),] #looking at the outliers
  
  # removing influential outliers above from the model
  bos.lm.final = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Minimum.Nights + 
                  Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                  Room.Type + Neighbourhood.Cleansed + Cancellation.Policy, 
                  data = bos_clean[-c(1787, 2078, 315, 1217, 2260),])
  
  # checking residual plots again
  par(mfrow=c(2,2))
  plot(bos.lm.final)
  summary(bos.lm.final)
  anova(bos.lm.final)
  
# BOSTON WITHOUT NEIGHBORHOOD ===============================================
  bos_non = bos_clean[,-10]
  
  # modeling AIC and BIC
  bos.lm.non = lm(Price~., data=bos_non)
  step(bos.lm.non, direction="both") #aic backward
  step(bos.lm.non, direction="both", k=log(nrow(bos_non))) #bic backward
  
  # models
  bos.lm.best.non = lm(formula = Price ~ Review.Scores.Cleanliness + Minimum.Nights + 
                     Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                     Room.Type + Cancellation.Policy, data = bos_non)
  
  # checking the residual plot
  dev.off()
  par(mfrow=c(2,2))
  plot(bos.lm.best.non)
  
  # applying box-cox
  dev.off()
  boxcox(bos.lm.best.non)
  bos.lm.trans.non = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Minimum.Nights + 
                      Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                      Room.Type + Cancellation.Policy, 
                    data = bos_non)
  
  # finding influential outlying observations
  par(mfrow=c(2,2))
  plot(bos.lm.trans.non)
  bos.res=cooks.distance(bos.lm.trans.non)
  
  bos.res[1189] 
  bos.res[2078]
  bos.res[315]
  bos.res[2188]

  bos_clean[c(1189, 2078, 315, 2188),] #looking at the outliers
  
  # removing influential outliers above from the model
  bos.lm.final.non = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Minimum.Nights + 
                      Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                      Room.Type + Cancellation.Policy, 
                    data = bos_non[-c(1189, 2078, 315, 2188),])
  
  # checking residual plots again
  par(mfrow=c(2,2))
  plot(bos.lm.final.non)
  summary(bos.lm.final.non)
  anova(bos.lm.final.non)
  
# D.C.WITH NEIGHBORHOOD ====================================================
  # modeling AIC and BIC
  dc.lm.all = lm(Price~., data=dc_clean)
  step(dc.lm.all, direction="both") #aic backward
  step(dc.lm.all, direction="both", k=log(nrow(dc_clean))) #bic backward
  
  # models
  dc.lm.aic = lm(formula = Price ~ Review.Scores.Cleanliness + Review.Scores.Communication + 
                 Minimum.Nights + Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                 Room.Type + Neighbourhood.Cleansed + Cancellation.Policy, 
                 data = dc_clean)
  dc.lm.bic = lm(formula = Price ~ Review.Scores.Cleanliness + Review.Scores.Communication + 
                 Minimum.Nights + Beds + Bathrooms + Reviews.per.Month + 
                 Room.Type + Neighbourhood.Cleansed + Cancellation.Policy, 
                 data = dc_clean)
  
  # cross-validation
  cross_val(dc_clean)
      # aic: 3818.8 -> use aic
      # bic: 4139.9
  dc.lm.best = dc.lm.aic
  
  #checking the residual plot
  par(mfrow=c(2,2))
  plot(dc.lm.best)
  
  #applying box-cox
  library(MASS)
  dev.off()
  boxcox(dc.lm.best)
  dc.lm.trans = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Review.Scores.Communication + 
                     Minimum.Nights + Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                     Room.Type + Neighbourhood.Cleansed + Cancellation.Policy, 
                     data = dc_clean)
  
  #finding influential outlying observations
  par(mfrow=c(2,2))
  plot(dc.lm.trans)
  dc.res=cooks.distance(dc.lm.trans)
  
  dc.res[17] 
  dc.res[3253] 
  dc.res[1064] 
  dc.res[3219]

  dc_clean[c(17,3253, 1064, 3219),] #looking at the outliers
  
  #removing influential outliers above from the model
  dc.lm.final = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Review.Scores.Communication + 
                     Minimum.Nights + Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                     Room.Type + Neighbourhood.Cleansed + Cancellation.Policy, 
                data = dc_clean[-c(17,3253, 1064, 3219),])
  
  #checking residual plots again
  par(mfrow=c(2,2))
  plot(dc.lm.final)
  summary(dc.lm.final)
  anova(dc.lm.final)
  
# D.C. WITHOUT NEIGHBORHOOD ===============================================
  dc_non = dc_clean[,-10]
  
  # modeling AIC and BIC
  dc.lm.non = lm(Price~., data=dc_non)
  step(dc.lm.non, direction="both") #aic backward
  step(dc.lm.non, direction="both", k=log(nrow(dc_non))) #bic backward
  
  # model (aic and bic were the same)
  dc.lm.best.non = lm(formula = Price ~ Review.Scores.Cleanliness + Review.Scores.Communication + 
                   Minimum.Nights + Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                   Room.Type + Cancellation.Policy, 
                 data = dc_non)
  
  #checking the residual plot
  par(mfrow=c(2,2))
  plot(dc.lm.best.non)
  
  #applying box-cox
  library(MASS)
  dev.off()
  boxcox(dc.lm.best.non)
  dc.lm.trans.non = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Review.Scores.Communication + 
                     Minimum.Nights + Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                     Room.Type + Cancellation.Policy, 
                   data = dc_non)
  
  #finding influential outlying observations
  par(mfrow=c(2,2))
  plot(dc.lm.trans.non)
  dc.res.non=cooks.distance(dc.lm.trans.non)
  
  dc.res.non[17] 
  dc.res.non[1064] 
  dc.res.non[3219]
  
  dc_clean[c(17,1064, 3219),] #looking at the outliers
  
  #removing influential outliers above from the model
  dc.lm.final.non = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Review.Scores.Communication + 
                     Minimum.Nights + Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                     Room.Type + Cancellation.Policy, 
                   data = dc_clean[-c(17,1064, 3219),])
  
  #checking residual plots again
  par(mfrow=c(2,2))
  plot(dc.lm.final.non)
  summary(dc.lm.final.non)
  anova(dc.lm.final.non)
  

# SAN FRANCISCO WITH NEIGHBORHOOD =============================================
  # modeling AIC and BIC
  sf.lm.all = lm(Price~., data=sf_clean)
  step(sf.lm.all, direction="both") #aic backward
  step(sf.lm.all, direction="both", k=log(nrow(sf_clean))) #bic backward
  
  # models
  sf.lm.aic = lm(formula = Price ~ Review.Scores.Cleanliness + Review.Scores.Communication + 
                  Beds + Bathrooms + Guests.Included + Reviews.per.Month + 
                  Room.Type + Neighbourhood.Cleansed + Cancellation.Policy, 
                  data = sf_clean)
  sf.lm.bic = lm(formula = Price ~ Review.Scores.Cleanliness + Beds + Bathrooms + 
                  Reviews.per.Month + Room.Type + Neighbourhood.Cleansed + 
                  Cancellation.Policy, data = sf_clean)  
  
  # cross-validation
  cross_val(sf_clean)
    #aic: 5164.1
    #bic: 5107.5
  sf.lm.best=sf.lm.bic
  
  #checking the residual plot
  par(mfrow=c(2,2))
  plot(sf.lm.best)
  
  #applying box-cox
  library(MASS)
  dev.off()
  boxcox(sf.lm.best)
  sf.lm.trans = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Beds + Bathrooms + 
                          Reviews.per.Month + Room.Type + Neighbourhood.Cleansed + 
                          Cancellation.Policy,
                     data = sf_clean)
  
  #finding influential outlying observations
  par(mfrow=c(2,2))
  plot(sf.lm.trans)
  sf.res=cooks.distance(sf.lm.trans)
  
  sf.res[483]
  sf.res[2125]
  sf.res[2515]
  sf.res[4340]
  sf.res[1295]
  sf.res[3134]
  
  sf_clean[c(483, 2125, 2515, 4340, 1295, 3134),] #looking at the outliers
  
  #removing influential outliers above from the model
  sf.lm.final = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Beds + Bathrooms + 
                  Reviews.per.Month + Room.Type + Neighbourhood.Cleansed + 
                  Cancellation.Policy,
                data = sf_clean[-c(483, 2125, 2515, 4340, 1295, 3134),])
  
  #checking residual plots again
  par(mfrow=c(2,2))
  plot(sf.lm.final)
  summary(sf.lm.final)
  anova(sf.lm.final)
  
# SAN FRANCISCO WITHOUT NEIGHBORHOOD =============================================
  sf_non = sf_clean[,-10]
  
  # modeling AIC and BIC
  sf.lm.non = lm(Price~., data=sf_non)
  step(sf.lm.non, direction="both") #aic backward
  step(sf.lm.non, direction="both", k=log(nrow(sf_non))) #bic backward
  
  # models
  sf.lm.best.non = lm(formula = Price ~ Review.Scores.Cleanliness + Beds + Bathrooms + 
                       Reviews.per.Month + Room.Type + Cancellation.Policy, 
                 data = sf_non)
  
  #checking the residual plot
  par(mfrow=c(2,2))
  plot(sf.lm.best.non)
  
  #applying box-cox
  library(MASS)
  dev.off()
  boxcox(sf.lm.best.non)
  sf.lm.trans.non = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Beds + Bathrooms + 
                     Reviews.per.Month + Room.Type + Cancellation.Policy,
                   data = sf_non)
  
  #finding influential outlying observations
  par(mfrow=c(2,2))
  plot(sf.lm.trans.non)
  sf.res.non=cooks.distance(sf.lm.trans.non)
  
  sf.res.non[2125]
  sf.res.non[483]
  sf.res.non[2515]

  sf_non[c(483, 2125, 2515),] #looking at the outliers
  
  #removing influential outliers above from the model
  sf.lm.final.non = lm(formula = log(Price) ~ Review.Scores.Cleanliness + Beds + Bathrooms + 
                     Reviews.per.Month + Room.Type + Cancellation.Policy,
                   data = sf_non[-c(483, 2125, 2515),])
  
  #checking residual plots again
  par(mfrow=c(2,2))
  plot(sf.lm.final.non)
  summary(sf.lm.final.non)
  anova(sf.lm.final.non)
  
# VISUALIZING
library(ggplot2)
library("ggiraphExtra")
library(plyr)
ggPredict(sf.lm.final, digits = 1, show.point = TRUE, se = TRUE, xpos = 0.5)
  
  