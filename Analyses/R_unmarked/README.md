## SINGLE-SPECIES, SINGLE-SEASON OCCUPANCY MODELS 
> Validating results using R package unmarked
&nbsp;
#### 1. Library
```{r}
library(unmarked)
library(AICcmodavg)
library(MuMIn)
library(ggplot2)
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
# Read in site covariates
mngmnt<-read.csv("mngmnt.csv", header = TRUE)

# sanity check
head(mngmnt) 
```
&nbsp;
#### 3. Create unmarkedFrameOccu object

```{r}
sample.unmarkedFrame_cov <- unmarkedFrameOccu( 
                                      y = as.matrix(DetectHist),
                                      siteCovs = mngmnt) 
# sanity check
head(sample.unmarkedFrame_cov)
```
&nbsp;
#### 4. Summarize detection histories
```{r}
detHist(sample.unmarkedFrame_cov)
```
&nbsp;
#### 5. Create models
```{r}
# Null model
occ_model <- occu(~ 1
                  ~ 1, 
                  data = sample.unmarkedFrame_cov)
# Look at regression coefficients from the model
summary(occ_model)

```
```{r}
# Global model
occ_model.2 <- occu(~ Sky+
                       Wind+
                       Noise+
                       Temp
                  ~Management0_1+
                    Juncus+
                    Typha+
                    Phragmites+
                    Schoenoplectus+
                    Trees.and.shrubs+
                    Mixed.Emergents, 
                  data = sample.unmarkedFrame_cov)
# Look at regression coefficients from the model
summary(occ_model)

```
```{r}
# Top model from PRESENCE output
occ_model1 <- occu(~ 1
                  ~ Management0_1 ,
                  data = sample.unmarkedFrame_cov)
# regression coefficients
summary(occ_model1)
```
&nbsp;
#### 6. MacKenzie-Bailey GOF test for top model ([reasoning](https://www.uvm.edu/~tdonovan/Occupancy%20Exercises/Exercise3/Exercise%203.%20%20Single-Species,%20Single-Season%20Occupancy%20Models.pdf): see page 25)
```{r}
occ_gof1<-mb.chisq(occ_model1, print.table = TRUE)
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
## Royle-Nichols model for abundance-induced heterogeneity
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
                  ~Management0_1+
                    Juncus+
                    Typha+
                    Phragmites+
                    Schoenoplectus+
                    Trees.and.shrubs+
                    Mixed.Emergents,
data = sample.unmarkedFrame_cov)

# Look at regression coefficients from the model
summary(global_occuRN)

```
&nbsp;

#### 9. I am using the `dredge` function from the package, [MuMIn](https://cran.r-project.org/web/packages/MuMIn/index.html), to test all possible models and rank them by AIC.

```{r}
RN_List <- dredge(global_occuRN, rank = "AIC")
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
