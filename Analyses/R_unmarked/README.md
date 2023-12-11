##  SINGLE-SPECIES, SINGLE-SEASON OCCUPANCY MODELS :star:
&nbsp; 
#### Validating results using R package unmarked
&nbsp;
&nbsp;

#### 1. Library
```{r}
library(unmarked)
library(AICcmodavg)
library(MuMIn)
```
&nbsp;
#### 2. Load data
```{r}
# Read in detection history (0/1 data)
DetectHist<-read.csv("Detections.csv", header = TRUE)

# sanity check
head(DetectHist) 
```
```{r}
# Read in site covariates (z-transformed-- I used the `scale` function in R to do this)
mngmnt<-read.csv("mngmnt.csv", header = TRUE)

# sanity check
head(mngmnt) 
```

```{r}
# Read in observational covariates (z-transformed-- I used the `scale` function in R to do this)
Noise <- read.csv("Noise.csv", header = TRUE)
Sky <- read.csv("Sky.csv", header = TRUE)
Wind <- read.csv("Wind.csv", header = TRUE)
Temp <- read.csv("Temp.csv", header = TRUE)

# sanity check
head(Noise)
head(Sky)
head(Wind)
head(Temp)
```
&nbsp;
#### 3. Create unmarkedFrameOccu object

```{r}
unmarkedFrame_cov <- unmarkedFrameOccu( 
                                      y = as.matrix(DetectHist),
                                      obsCovs = list(Noise = Noise,
                                                     Sky = Sky,
                                                     Wind = Wind,
                                                     Temp = Temp),
                                      siteCovs = mngmnt) 
# sanity check
head(unmarkedFrame_cov)
```
&nbsp;
#### 4. Summarize detection histories
```{r}
detHist(unmarkedFrame_cov)
```
&nbsp;
#### 5. Create models
```{r}
# Null model
occ_model <- occu(~ 1
                  ~ 1, 
                  data = unmarkedFrame_cov)
# Look at regression coefficients from the model
summary(occ_model)

```
```{r}
# Global model
occ_model.2 <- occu(~ Sky+
                       Wind+
                       Noise+
                       Temp
                  ~Management+
                    Juncus+
                    Typha+
                    Phragmites+
                    Schoenoplectus+
                    Trees+
                    MixedEmergents, 
                  data = unmarkedFrame_cov)
# Look at regression coefficients from the model
summary(occ_model)

```
```{r}
# Top model from PRESENCE output
occ_model1 <- occu(~ 1
                  ~ Management0_1 ,
                  data = unmarkedFrame_cov)
# regression coefficients
summary(occ_model1)
```
&nbsp;
#### 6. MacKenzie-Bailey GOF test for top model ([reasoning](https://www.uvm.edu/~tdonovan/Occupancy%20Exercises/Exercise3/Exercise%203.%20%20Single-Species,%20Single-Season%20Occupancy%20Models.pdf): see page 25)
```{r}
occ_gof1<-mb.gof.test(occ_model1, nsim = 99999, plot.hist = FALSE)
# hide the chisq table to give simpler output
occ_gof1$chisq.table <- NULL
print(occ_gof1)
```
*I am using a Mackenzie-Bailey Goodness of Fit test over Pearson's Chi-Square test because the Pearson test can produce inflated & misleading results, particularly when assessing fit in occupancy models with covariates.*

&nbsp;
#### 7. Estimate occupancy and detection probability

```{r}
# To get real estimate of occupancy (with 95% CI)
predict(occ_model1, 
        newdata = data.frame(site = 1),
        type = "state")

```
```{r}
# To get real estimate of detection (with 95% CI)
predict(occ_model1, 
        newdata = data.frame(site = 1),
        type = "det")
```
&nbsp;





---
## Royle-Nichols model for abundance-induced heterogeneity :baby_chick:
&nbsp;

#### 8. Fit null & global model


```{r}
# Null model
null_occuRN <- occuRN(~ 1
                  ~ 1,
data = sample.unmarkedFrame_cov)

# Look at regression coefficients from the model
summary(null_occuRN)

```
```{r}
# Global model
global_occuRN <- occuRN(~ Sky+
                       Wind+
                       Noise+
                       Temp
                  ~Management+
                    Juncus+
                    Typha+
                    Phragmites+
                    Schoenoplectus+
                    Trees+
                    MixedEmergents,
data = sample.unmarkedFrame_cov)

# Look at regression coefficients from the model
summary(global_occuRN)

```
&nbsp;

#### 9. I am using the `dredge` function from the package, [MuMIn](https://cran.r-project.org/web/packages/MuMIn/index.html), to test all possible models and rank them by AIC.

```{r}
RN_List <- dredge(global_occuRN, beta = "sd", rank = "AICc")
```


#### 10. GOF test for top model


```{r}
# Create top model in the global environment
Top_occuRN <- occuRN(~ 1
                  ~Management0_1+
                    Juncus+
                    Typha+
                    Phragmites,
data = sample.unmarkedFrame_cov)

# Check regression coefficients from the model
summary(Top_occuRN)

```

```{r}
# MB GOF Test
Top_occuRN_GOF <- mb.gof.test(Top_occuRN, nsim = 1000, plot.hist = FALSE)

# hide the chisq table to give simpler output
Top_occuRN_GOF$chisq.table <- NULL

print(Top_occuRN_GOF)
```


#### 11. I am using MuMIn for model averaging 
```
# Create a list of your top fitted model objects (remember they should all have ΔAICc < 2)
Top_Occu_List <- list(Top_model, Model2, Model3)

# Average models
avg <- model.avg(Top_Occu_List, beta = "none", full = TRUE, rank = "AICc")

# Save results if you want

# write.csv(avg$msTable, "avg.msTable.csv") 
# write.csv(avg$coefficients, "avg.coefficients.csv")
# write.csv(avg$coefArray, "avg.coefArray.csv")
```
How the heck do I interpret the output? [crtl+F this article for "full-model averaging"](https://link.springer.com/article/10.1007/s00265-010-1037-6).


&nbsp;

---

&nbsp;
&nbsp;







## Next, I am removing potential outliers from my dataset to see if that impacts my results :bird:
 
&nbsp;



#### 1. Libraries

```
library(AICcmodavg)
library(MuMIn)
library(unmarked)
```
&nbsp;



#### 2. Data
- &nbsp; &nbsp; 2.1 Read in data
```{r}
DetectHist <- read.csv("Detections.csv", header = TRUE)
siteCovs <- read.csv("Vegetation.csv", header = TRUE)
Noise <- read.csv("Noise.csv", header = TRUE)
Sky <- read.csv("Sky.csv", header = TRUE)
Wind <- read.csv("Wind.csv", header = TRUE)
Temp <- read.csv("Temp.csv", header = TRUE)

```
- &nbsp; &nbsp; 2.2 Remove outliers
```{r}
# Remove outliers
DetectHist.NO <- DetectHist[-c(3, 12, 13), ]
siteCovs.NO <- siteCovs[-c(3, 12, 13), ]
Noise.NO <- Noise[-c(3, 12, 13), ]
Sky.NO <- Sky[-c(3, 12, 13), ]
Wind.NO <- Wind[-c(3, 12, 13), ]
Temp.NO <- Temp[-c(3, 12, 13), ]
Obs.NO <- Obs[-c(3, 12, 13), ]

```

- &nbsp; &nbsp; 2.3 Scale
```{r}
# scale
siteCovs.NO <- as.data.frame(scale(siteCovs.NO[, 2:9])) # I'm excluding some irrelevant columns here, too.
Noise.NO <- as.data.frame(scale(Noise.NO))
Sky.NO <- as.data.frame(scale(Sky.NO))
Wind.NO <- as.data.frame(scale(Wind.NO))
Temp.NO <- as.data.frame(scale(Temp.NO))
Obs.NO <- as.data.frame(scale(Obs.NO))

```
&nbsp;



#### 3. Unmarked model object
```{r}
# Build new unmarkedFramOccu
unmarkedFrame_NO <- unmarkedFrameOccu( y = as.matrix(DetectHist.NO),
                                        obsCovs = list(Noise = Noise.NO,
                                                     Sky = Sky.NO,
                                                     Wind = Wind.NO,
                                                     Temp = Temp.NO,
                                                     Obs = Obs.NO),
                                        siteCovs = siteCovs.NO)


summary(unmarkedFrame_NO)
```
&nbsp;




#### 4. Global model
```{r}
global_model.NO <- occuRN(~ Sky+
                       Wind+
                       Noise+
                       Temp+
                        Obs
                  ~ Management+
                    Schoenoplectus+
                    Grass+
                    MixedEmergents+
                    Juncus+
                    Phragmites+
                    Typha+
                    Trees,
                  data = unmarkedFrame_NO)

# Regression coefficients
summary(global_model.NO)
```
&nbsp;




#### 5. Dredge
```{r}
RN_NO.List <- dredge(global_model.NO, rank = "AICc", evaluate = TRUE)
```

&nbsp;



#### 6. Create top model
```{r}
Top_occuRN.NO <- occuRN(~ 1
                  ~Management +

data = unmarkedFrame_NO)

summary(Top_occuRN.NO)
```
&nbsp;




#### 7. Model averaging
- &nbsp; &nbsp; 7.1 Create other top models (all have ΔAICc < 2)
```
# Top model 2
Top_occuRN.NO.2 <- <- occuRN(~ 1 ~Management , data = unmarkedFrame_NO)

# Top model 3
Top_occuRN.NO.3 <- <- occuRN(~ 1 ~Management , data = unmarkedFrame_NO)

# Top model 4
Top_occuRN.NO.4 <- <- occuRN(~ 1 ~Management , data = unmarkedFrame_NO)

# Top model 5
Top_occuRN.NO.5 <- <- occuRN(~ 1 ~Management , data = unmarkedFrame_NO)
```

- &nbsp; &nbsp; 7.2 Make a list of top models
```
Top_OccuRN.NO_List <- list(Top_occuRN.NO, Top_occuRN.NO.2, Top_occuRN.NO.3, Top_occuRN.NO.4, Top_occuRN.NO.5)
```
- &nbsp; &nbsp; 7.3 Model average
```
# Average models
avg.NO <- model.avg(Top_OccuRN.NO_List, beta = "none", full = TRUE, rank = "AICc")

# Save results

# write.csv(avg.NO$msTable, "avg.NO.msTable.csv") 
# write.csv(avg.NO$coefficients, "avg.NO.coefficients.csv")
# write.csv(avg.NO$coefArray, "avg.NO.coefArray.csv")
```

&nbsp;



#### 8. MB GOF test
```{r}
# MB GOF Test
Top_occuRN.NO_GOF <- mb.gof.test(Top_occuRN.NO, nsim = 1000, plot.hist = FALSE)

# hide the chisq table for simpler output
Top_occuRN.NO_GOF$chisq.table <- NULL

print(Top_occuRN.NO_GOF)
```
