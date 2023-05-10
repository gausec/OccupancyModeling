# 2022 KIRA Occupancy Modeling


## 1. Load libraries


```{r}
library(RPresence) #load RPresence
ls("package:RPresence")
library(ggplot2) #load ggplot package

```

## 2. Set working directory

setwd("C:/Users/Carol's PC/Documents/KIRAoccModel/Data")
getwd()
```{r}
setwd("C:/Users/Carol's PC/Documents/KIRAoccModel/Data")

```


## 3. Detection Histories 

```{r}

## Read in detection history (0/1 data)

DetectHist<-read.csv("Detections.csv", header = TRUE)

head(DetectHist) # make sure it looks okay
```

## 4. Sample Covariates 

1. Add csv files
```{r}
### Read in sample covariates – these vary temporally
Wind<-read.csv("Wind.csv", header = TRUE)
Sky<-read.csv("Sky.csv", header = TRUE)
Noise<-read.csv("Noise.csv", header = TRUE)
Temp<-read.csv("Temp.csv", header = TRUE)


### Checking how it looks
head(Wind)
head(Noise)
head(Temp)
head(Sky)
```

2. Reshape sampling covariate data from wide --> long (required for occupancy analysis in R).

```{r}

#### 1
Wind_long <- reshape(Wind, 
                     varying= list(colnames(Wind)[seq(2,4, by= 1)]),
                     v.names = c("Wind"),
                     timevar="Survey",
                     direction="long",
                     idvar="SiteID")

head(Wind_long)

#### 2
Noise_long <- reshape(Noise, 
                      varying= list(colnames(Noise)[seq(2,4, by= 1)]),
                      v.names = c("Noise"),
                      timevar="Survey",
                      direction="long",
                      idvar="SiteID")

head(Noise_long)

#### 3
Temp_long <- reshape(Temp, 
                     varying= list(colnames(Temp)[seq(2,4, by= 1)]),
                     v.names = c("Temp"),
                     timevar="Survey",
                     direction="long",
                     idvar="SiteID")

head(Temp_long)

#### 4
Sky_long <- reshape(Sky, 
                    varying= list(colnames(Sky)[seq(2,4, by= 1)]),
                    v.names = c("Sky"),
                    timevar="Survey",
                    direction="long",
                    idvar="SiteID")

head(Sky_long)
```

3. Combine the sample covariate data 

```{r}

SampleCovar_all<-cbind(Wind_long, Noise_long$Noise, Temp_long$Temp,Sky_long$Sky)

head(SampleCovar_all)

### Change the column names (for clarity)
colnames(SampleCovar_all)[4]<-"Noise"
colnames(SampleCovar_all)[5]<-"Temp"
colnames(SampleCovar_all)[6]<-"Sky"

head(SampleCovar_all)
```

4. Scale survey covariates so that they have unit variance and mean of zero (z score). Required for comparison.

```{r}

SampleCovar_all$Wind<-scale(SampleCovar_all$Wind)
SampleCovar_all$Noise<-scale(SampleCovar_all$Noise)
SampleCovar_all$Temp<-scale(SampleCovar_all$Temp)
SampleCovar_all$Sky<-scale(SampleCovar_all$Sky)

head(SampleCovar_all)
---

### Save combined sample covariate data as a csv file 
#write.csv(SampleCovar_all, "SampleCovar_all.csv")
```

## 5. Save time later by uploading long format sample covariates 

```{r}
SampleCovar_all<-read.csv("SampleCovar_all.csv", header = TRUE)
```


## 6. Site Covariates 

```{r}

### Read in CSV file
Vegetation<-read.csv("Vegetation.csv", header = TRUE)
Vegetation_scaled<-read.csv("VegetationScaled.csv", header = TRUE)


head (Vegetation) # These are already in long format since this data was collected once (not on 3 separate occasions)

---
### Scale site covariate data so it has unit variance and mean of zero (z score)
Vegetation$Juncus.sp.<-scale(Vegetation$Juncus.sp.)
Vegetation$Typha.sp.<-scale(Vegetation$Typha.sp.)
Vegetation$Phragmites.australis.var..australis<-
  scale(Vegetation$Phragmites.australis.var..australis)
Vegetation$Grass.Sp.<-scale(Vegetation$Grass.Sp.)
Vegetation$Schoenoplectus.americanus<-scale(Vegetation$Schoenoplectus.americanus)
Vegetation$Trees.and.shrubs<-scale(Vegetation$Trees.and.shrubs)
Vegetation$Mixed.Emergents<-scale(Vegetation$Mixed.Emergents)
Vegetation$Management0_1<-scale(Vegetation$Management0_1)


head (Vegetation) 


```

## 7. Visualizing the data to see what distributions look like 

```{r}

hist(SampleCovar_all$Wind)
hist(SampleCovar_all$Noise)
hist(SampleCovar_all$Temp)
hist(SampleCovar_all$Sky)

hist(Vegetation$Juncus.sp.)
hist(Vegetation$Management0_1)
hist(Vegetation$Typha.sp.)
hist(Vegetation$Phragmites.australis.var..australis)
hist(Vegetation$Grass.Sp.)
hist(Vegetation$Schoenoplectus.americanus)
hist(Vegetation$Trees.and.shrubs)
hist(Vegetation$Mixed.Emergents) #none of the covariates are like a bell curve. Hopefully that is okay.

```



## 8. Create PAO file 

1. Remove unnecessary columns

```{r}
SampleCovar_all<-(SampleCovar_all[,-c(1,2)]) # removing survey and SiteID column since sequence is assumed
Vegetation_scaled<-(Vegetation_scaled[,-1])
DetectHist<-( DetectHist[,-1])
SampleCovar_all<-(SampleCovar_all[,-1])

head(SampleCovar_all)
head(Vegetation)
head(DetectHist)

```

2.Create PAO data object using detection history and covariates

```{r}
# PAO file

KIRApao<-createPao(data= DetectHist, 
                   unitcov = Vegetation_scaled, 
                   survcov = SampleCovar_all
)


#### Check the first five rows of the detection data from PAO file
head(KIRApao$det.data) 
#### Check the site covariate data from PAO file
head(KIRApao$unitcov) 
#### Check survey covariates data from PAO file
head(KIRApao$survcov)

#---------------------------------------------------------------------#

### (optional - unhash) removing the survey column that isn't needed
#KIRApao$survcov<-(KIRApao$survcov[,-5])

#---------------------------------------------------------------------#

### Looking at the individual parts of the pao file to check correctness 
det.data<-(KIRApao$det.data)
unitcov<-(KIRApao$nunitcov)
survcov<-(KIRApao$survcov)
```

## 9. Fit psi(.)p(.) aka the null model 

```{r}

### Null model
KIRA_null<-occMod(model = list(psi~1, p~1), data = KIRApao, type = "so")

### Results
summary(KIRA_null)

#---------------------------------------------------------------------#

#### Probability of sites being occupied
unique(fitted(KIRA_null, "psi")) # why are the values repeating even though I used unique?

#---------------------------------------------------------------------#

#### Probability of detection
unique(fitted(KIRA_null, "p"))
```

## 10. Estimate Naive Occupancy 

```{r}

Naive_occ<-sum(ifelse(rowSums(KIRApao$det.data[2:ncol(KIRApao$det.data)], na.rm=TRUE)>0,1,0))/
  nrow(KIRApao$det.data)

Naive_occ
```

## 11. Model detection probability as a function of covariates 

1. psi is constant

```{r}

## psi(.)p(Wind+Noise+Temp+Sky)
KIRA_AllSampleCovars<-occMod(model = list(psi~1, p~Wind+Noise+Temp+Sky), data = KIRApao, type = "so")

summary(KIRA_AllSampleCovars)

unique(fitted(KIRA_AllSampleCovars, "psi"))
unique(fitted(KIRA_AllSampleCovars, "p"))

coef(KIRA_AllSampleCovars, "psi")
coef(KIRA_AllSampleCovars, "p")

#---------------------------------------------------------------------#

## psi(.)p(Wind+Noise+Temp)
KIRA_Wind.Noise.Temp<-occMod(model = list(psi~1, p~Wind+Noise+Temp), data = KIRApao, type = "so")
summary(KIRA_Wind.Noise.Temp)

#---------------------------------------------------------------------#

## psi(.)p(Wind+Noise)
KIRA_Wind.Noise<-occMod(model = list(psi~1, p~Wind+Noise), data = KIRApao, type = "so")
summary(KIRA_Wind.Noise)

#---------------------------------------------------------------------#

## psi(.)p(Wind) 
KIRA_Wind<-occMod(model = list(psi~1, p~Wind), data = KIRApao, type = "so")
summary(KIRA_Wind)

#---------------------------------------------------------------------#

## psi(.)p(Noise)
KIRA_Noise<-occMod(model = list(psi~1, p~Noise), data = KIRApao, type = "so")
summary(KIRA_Noise)

#---------------------------------------------------------------------#

## psi(.)p(Sky)
KIRA_Sky<-occMod(model = list(psi~1, p~Sky), data = KIRApao, type = "so")
summary(KIRA_Sky)

#---------------------------------------------------------------------#

## psi(.)p(Temp)
KIRA_Temp<-occMod(model = list(psi~1, p~Temp), data = KIRApao, type = "so")
summary(KIRA_Temp)
```

2. Global model

```{r}

KIRA_AllCovars<-occMod(model = list(psi~Juncus.sp.+
                                            Typha.sp.+
                                  Phragmites.australis.var..australis +
                                            Grass.Sp.+
                                            Schoenoplectus.americanus+
                                            Trees.and.shrubs+
                                            Mixed.Emergents,
                                          p~Wind+Noise+Temp+Sky), 
                             data = KIRApao, type = "so")
summary(KIRA_AllCovars)

coef(KIRA_AllCovars, "psi")
coef(KIRA_AllCovars, "p")
```

3. p is constant


```{r}

## psi(Juncus Typha Phrag Grass Schoenoplectus Trees MixedEmergents)p(.) p is constant.
KIRA_J.T.P.G.S.Tree.M<-occMod(model = list(psi~Juncus.sp.+
                                      Typha.sp.+ 
                                  Phragmites.australis.var..australis+
                                      Grass.Sp.+
                                      Schoenoplectus.americanus+
                                      Trees.and.shrubs+
                                      Mixed.Emergents,
                                    p~1), 
                       data = KIRApao, type = "so")
summary(KIRA_J.T.P.G.S.Tree.M)

#---------------------------------------------------------------------#

## psi(Juncus Typha Phrag Grass Schoenoplectus Trees)p(.) 
KIRA_J.T.P.G.S.Tree<-occMod(model = list(psi~Juncus.sp.+
                                             Typha.sp.+
                                 Phragmites.australis.var..australis+
                                             Grass.Sp.+
                                             Schoenoplectus.americanus+
                                             Trees.and.shrubs,
                                           p~1), 
                              data = KIRApao, type = "so")
summary(KIRA_J.T.P.G.S.Tree)

#---------------------------------------------------------------------#

## psi(Juncus Typha Phrag Grass Schoenoplectus) p(.)
KIRA_J.T.P.G.S<-occMod(model = list(psi~Juncus.sp.+
                                           Typha.sp.+
                                  Phragmites.australis.var..australis+
                                           Grass.Sp.+
                                           Schoenoplectus.americanus,
                                         p~1), 
                            data = KIRApao, type = "so")
summary(KIRA_J.T.P.G.S)

#---------------------------------------------------------------------#

## psi(Juncus, Typha, Phrag, Grasses)p(.)

KIRA_J.T.P.G<-occMod(model = list(psi~Juncus.sp.+
                                      Typha.sp.+
                                  Phragmites.australis.var..australis+
                                      Grass.Sp.,
                                    p~1), 
                       data = KIRApao, type = "so")

summary(KIRA_J.T.P.G)

fitted(KIRA_J.T.P.G, "psi")
fitted(KIRA_J.T.P.G, "p")

coef(KIRA_J.T.P.G, "psi")
coef(KIRA_J.T.P.G, "p")

#---------------------------------------------------------------------#

## psi(Juncus, Typha, Phrag. management)p(.)

KIRA_J.T.P.Mgmt<-occMod(model = list(psi~Juncus.sp.+
                                  Typha.sp.+
                                  Phragmites.australis.var..australis+
                                  Management0_1,
                                p~1), 
                   data = KIRApao, type = "so")
summary(KIRA_J.T.P.Mgmt)

#---------------------------------------------------------------------#

## psi(Juncus, Typha, Phrag)p(.)

KIRA_J.T.P<-occMod(model = list(psi~Juncus.sp.+
                                    Typha.sp.+
                                    Phragmites.australis.var..australis,
                                  p~1), 
                     data = KIRApao, type = "so")
summary(KIRA_J.T.P)

#---------------------------------------------------------------------#

## psi(Juncus Typha)p(.)

KIRA_J.T<-occMod(model = list(psi~Juncus.sp.+
                                    Typha.sp.,
                                  p~1), 
                     data = KIRApao, type = "so")
summary(KIRA_J.T)

#---------------------------------------------------------------------#

## psi(Juncus, Phrag)p(.)

KIRA_J.P<-occMod(model = list(psi~Juncus.sp.+
                                  Phragmites.australis.var..australis,
                                  p~1), 
                     data = KIRApao, type = "so")
summary(KIRA_J.P)

#---------------------------------------------------------------------#

## psi(Management)p(.)

KIRA_mgmt<-occMod(model = list(psi~Management0_1,
                              p~1), 
                 data = KIRApao, type = "so")
summary(KIRA_mgmt)

#---------------------------------------------------------------------#

## psi(Juncus, Management)p(.)

KIRA_J.mgmt<-occMod(model = list(psi~ Juncus.sp.+
                               Management0_1,
                               p~1), 
                  data = KIRApao, type = "so")
summary(KIRA_J.mgmt)

#---------------------------------------------------------------------#

## psi(Juncus, Typha, Management)p(.)

KIRA_J.T.mgmt<-occMod(model = list(psi~ Juncus.sp.+
                                   Typha.sp.+
                                   Management0_1,
                                 p~1), 
                    data = KIRApao, type = "so")
summary(KIRA_J.T.mgmt)

#---------------------------------------------------------------------#

## psi(Juncus, Phrag, Management)p(.)

KIRA_J.P.mgmt<-occMod(model = list(psi~ Juncus.sp.+
                                  Phragmites.australis.var..australis+
                                     Management0_1,
                                   p~1), 
                      data = KIRApao, type = "so")
summary(KIRA_J.P.mgmt)

#---------------------------------------------------------------------#

## psi(Phrag, Management)p(.)

KIRA_P.mgmt<-occMod(model = list(psi~ Phragmites.australis.var..australis+
                                     Management0_1,
                                   p~1), 
                      data = KIRApao, type = "so")
summary(KIRA_P.mgmt)
```

# 12. Compare AIC ------------------------

1. Compile models already run  

```{r}

All_KIRA_Models<-list(KIRA_AllCovars,
                      KIRA_AllSampleCovars,
                      KIRA_null,
                      KIRA_J.P,
                      KIRA_J.T,
                      KIRA_J.mgmt,
                      KIRA_J.P.mgmt,
                      KIRA_J.T.mgmt,
                      KIRA_P.mgmt,
                      KIRA_mgmt,
                      KIRA_J.T.P,
                      KIRA_J.T.P.Mgmt,
                      KIRA_J.T.P.G,
                      KIRA_J.T.P.G.S,
                      KIRA_J.T.P.G.S.Tree,
                      KIRA_J.T.P.G.S.Tree.M,
                      KIRA_Noise,
                      KIRA_Sky,
                      KIRA_Temp,
                      KIRA_Wind,
                      KIRA_Wind.Noise,
                      KIRA_Wind.Noise.Temp
                      )

#---------------------------------------------------------------------#

## Create a summary table of AIC values & compare. These are ranked by minimum AIC value
KIRA_AIC_Table<-createAicTable(All_KIRA_Models) 
AIC<-summary(KIRA_AIC_Table) 
#write.csv(AIC, "AIC.CSV")
AIC

#---------------------------------------------------------------------#

# Extract results from the top three models
top.model<-KIRA_AIC_Table$models[[1]]
top2.model<-KIRA_AIC_Table$models[[2]]
top3.model<-KIRA_AIC_Table$models[[3]]

#---------------------------------------------------------------------#

## Check coefficients (beta values) of psi and p from the top model
coef(top.model, "psi")
coef(top.model, "p")

```

# 13. Negative % bias ------------------------
13.1 Estimate negative % bias between naive and null-model occupancy estimates
```{r}
Bias<-(unique(fitted(KIRA_null, "psi")$est)-Naive_occ)/unique(fitted(KIRA_null,"psi")$est)*100
Bias
``` 

# 14.  Create a grid to map occupancy ----------------


# 15. Highest likelihood model GoF ----------------
15.1 Run a model estimating goodness of fit with the model that has the highest likelihood


# 16. Estimate occupancy
16.1 Create data with all possible values of habitat covariate (This is one possible method to estimate occupancy)

# 18. Chi-Square Goodness of Fit Test for Poisson Distribution ------------------------

1. Using df=1 and detection history frequencies (0/1)

```{r}
# Data

frq0_1<-read.csv("Frequency_0_1.csv", header = TRUE) # 0/1 detection data as frequencies 

#---------------------------------------------------------------------#
  
# Compute Poisson distribution

poisson <- function(x, lambda) {
  probfn <- exp(-lambda) * (lambda ^ x) / factorial(x)
  return(probfn)
}

#---------------------------------------------------------------------#


# Prep for Chi sq
  
attach(frq0_1) # variables can be accessed
N <- sum(Frequency)
RF <- Frequency/N
DF <- data.frame(frq0_1, round(RF, 1))
MEAN <- sum(RF * Detections)
VAR <- (sum(Detections^2*Frequency) - N*MEAN^2)/(N-1) # else use (MEAN+MEAN^2/R)
DISP <- VAR/MEAN # over dispersion
THETA <- 1/DISP
R <- MEAN*THETA/(1-THETA) # MEAN = R(1-THETA)/THETA
cbind(MEAN,VAR,DISP,THETA,R)

x = Detections
(E_poi = round(N * dpois(x, lambda=MEAN),5))

#---------------------------------------------------------------------#

# View distributions

par(mfrow=c(1,1))
barplot(matrix(c(Frequency,E_poi),nr=2, byrow = TRUE), beside=T, 
        col=c("aquamarine3","coral"), 
        names.arg=x)# graph obs vs exp

legend("topright", c("Observed","Expected Poisson"), pch=15, 
      col=c("aquamarine3","coral"), 
      bty="Detections") # add a legend

#---------------------------------------------------------------------#

# View the Chi sq test outputs

CT <- chisq.test(rbind(Frequency,E_poi)) 
CT


```


```{r}
# This is a different package for the same chi square test to double check my results from above.

#install.packages("rhep")
library(rhep)

#devtools::install_github("reyzaguirre/rhep") #alternative chi square test

#---------------------------------------------------------------------#

attach(frq0_1)
x <- 0:1
f <- c(76,98)
chi<-chisq.pois(x, f)
summary(chi)
chi # same answer

```



2. Using df=2 and count data frequencies

```{r}
# Data 
CountFreq<-read.csv("Frequency.csv", header = TRUE) # count data as frequencies 

#---------------------------------------------------------------------#

# Compute Poisson distribution
  
poisson <- function(x, lambda) {
  probfn <- exp(-lambda) * (lambda ^ x) / factorial(x)
  return(probfn)
}

#---------------------------------------------------------------------#

# Chi sq prep
  
attach(CountFreq) # variables can be accessed
N <- sum(Frequency)
RF <- Frequency/N
DF <- data.frame(CountFreq, round(RF, 1))
MEAN <- sum(RF * Detections)
VAR <- (sum(Detections^2*Frequency) - N*MEAN^2)/(N-1) # else use (MEAN+MEAN^2/R)
DISP <- VAR/MEAN # over dispersion
THETA <- 1/DISP
R <- MEAN*THETA/(1-THETA) # MEAN = R(1-THETA)/THETA
cbind(MEAN,VAR,DISP,THETA,R)

x <- Detections
(E_poi = round(N * dpois(x, lambda=MEAN),5))

#---------------------------------------------------------------------#

# View distributions

par(mfrow=c(1,1))
barplot(matrix(c(Frequency,E_poi),nr=2, byrow = TRUE), beside=T, 
        col=c("aquamarine3","coral"), 
        names.arg=x)# graph obs vs exp

legend("topright", c("Observed","Expected Poisson"), pch=15, 
      col=c("aquamarine3","coral"), 
      bty="Detections") # add a legend

#---------------------------------------------------------------------#


# View the Chi sq test outputs
CT <- chisq.test(rbind(Frequency,E_poi))
CT

```


3. Zero-inflated Poisson (ZIP) model

```{r}

# Load libraries
library(fitdistrplus)    # fits distributions using maximum likelihood
library(gamlss)          # defines pdf, cdf of ZIP

#---------------------------------------------------------------------#

# Raw counts to vector
  
i.vec <- c(0,0,1,1,0,2,0,2,3,1,0,2,0,0,0,3,5,5,0,0,0,0,2,4,2,3,1,0,2,2,1,1,3,2,1,5,4,5,0,1,2,3,0,0,4,0,1,2,1,2,0,0,0,0,1,0,1,1,0,0,0,3,2,0,4,1,1,0,1,2,0,0,0,0,0,0,0,0,1,0,1,5,0,0,1,1,3,1,1,0,2,1,1,3,6,0,0,2,1,1,1,6,0,2,2,0,5,0,0,0,0,0,0,0,1,3,0,0,0,0,1,2,3,3,3,1,0,4,2,1,0,0,0,0,0,0,0,0,0,2,2,2,4,2,2,3,2,2,2,3,0,3,3,4,0,0,0,2,1,3,2,2,0,4,2,2,0,0,0,0,0,0,0,1)

mean(i.vec)
# 1.3 mean

sd(i.vec)
# 1.5 standard dev

#---------------------------------------------------------------------#

# Fit the distribution (mu = mean of poisson, sigma = P(X = 0)
help("fitdistrplus")
class("fit_zip")

fit_zip <- fitdist(i.vec, 'ZIP', start = list(mu = 2, sigma = 0.5))


#---------------------------------------------------------------------#

# Visualize distribution & find GoF   

plot(fit_zip)
gof_ZIP<-gofstat(fit_zip, discrete = TRUE)
print(gof_ZIP)

```


# 19. Royle-Nicols Model ------------------------

1. Create PAO file

```{r}
# Files
RNHDetectHist<-read.csv("RNHDetectHist.csv", header = TRUE)
RNHScaledVeg<-read.csv("NRHScaledVeg.csv", header = TRUE)
RNHSampleCovar_all<-read.csv("RNHSampleCovar_all.csv", header = TRUE)

#Remove un-needed columns
Vegetation_scaled<-Vegetation_scaled[,-1] 

#Create one PAO file to rule them all
RNHpao<-createPao(data= RNHDetectHist, 
                   unitcov = RNHScaledVeg, 
                   survcov = RNHSampleCovar_all
)


modCombos(RNHDetectHist,RNHScaledVeg)

# remove rows
RNHpao$survcov <- (RNHSampleCovar_all[,-1])

#### Check the first five rows of the detection data from PAO file
head(RNHpao$det.data) 
#### Check the site covariate data from PAO file
head(RNHpao$unitcov) 
#### Check survey covariates data from PAO file
head(RNHpao$survcov)

#---------------------------------------------------------------------#

### (optional - unhash) removing the survey column that isn't needed
#NRHpao$survcov<-(NRHpao$survcov[,-5])

#---------------------------------------------------------------------#

### Looking at the individual parts of the pao file to check correctness 
RNHdet.data<-(RNHpao$det.data)
RNHunitcov<-(RNHpao$nunitcov)
RNHsurvcov<-(RNHpao$survcov)

```

2. Null model

```{r}
RNH_null<-occMod(model = list(lambda~1, c~1), data = KIRApao, type = "AHO", link = logit)


summary(RNH_null)
```

2. Lambda (Management), r()
```{r}
RNH_mgmnt<-occMod(model = list(lambda~Juncus.sp., c~1), data = KIRApao, type = "RNH")


#summary(RNH_mgmnt)


#nrow(RNHpao) #NULL
#length(RNHpao) #16

#---------------------------------------------------------------------#

# help("RPresence")


#RNH_mgmnt<- occMod_RPC(
#  lambda = call(),
#  lambda.cov = KIRApao$unitcov,
#  r = call(),
#  r.cov = cbind(KIRApao$unitcov, KIRApao$survcov),
#  nic = TRUE,
#  model = 100,
#  data = KIRApao
# )
#summary(RNH_mgmnt)


```




```{r}

# count <- read.csv("Detection.counts.csv", header = TRUE)
#---------------------------------------------------------------------#

#pao=createPao(count)
#---------------------------------------------------------------------#

#m2=occMod(list(lambda~Management0_1, r~1), data=KIRApao, type='RNH', outfile='modname')
#print(print_one_site_estimates(m2))
#print(print_one_site_estimates(m2,derived=T))
```




3. Using a different package (wiqid). Output is checked against output from PRESENCE v.6.9

```{r}
# help("wiqid-package")

# SiteCovar <- read.csv("data_wiqid.csv", header = TRUE)

#---------------------------------------------------------------------#

#occSSrn(DetectHist,
#        model = NULL,
#        data = SiteCovar, 
#        ci = 0.95, 
#        link=c("logit","probit"), 
#        verify = TRUE,)
#---------------------------------------------------------------------#

# Null nodel
#occSSrn0(58, 3, ci=0.95, link = c("logit","probit") )

#occSSrnSite(58, 3, model = NULL, data = NULL, link = c("logit","probit") )
```
