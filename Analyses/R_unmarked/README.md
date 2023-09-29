## Validating results using R package unmarked
&nbsp;
#### 1. Library
```{r}
library(unmarked)
library(AICcmodavg)
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
occ_model.2 <- occu(~ 1
                  ~ Management0_1 +
                    Juncus.sp.+
                    Typha.sp.+
                    Phragmites.australis.var..australis+
                    Grass.Sp.+
                    Schoenoplectus.americanus+
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
#### 6. MacKenzie-Bailey GOF test for top model
```{r}
occ_gof1<-mb.chisq(occ_model1, print.table = TRUE)
print(occ_gof1)
```

&nbsp;
#### 7. Estimate occupancy and detection probability

```{r}
# To get real estimate of occupancy (with 95% CI)
predict(occ_model1, 
        newdata = data.frame(site = 1),
        type = "state")

```
&nbsp;

```{r}
# To get real estimate of detection (with 95% CI)
predict(occ_model1, 
        newdata = data.frame(site = 1),
        type = "det")
```

