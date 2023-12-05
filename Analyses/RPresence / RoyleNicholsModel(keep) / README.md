# Royle-Nichols Model 

---

### 1. Create PAO file

```{r}
# Files
RNHDetectHist<-read.csv("RNHDetectHist.csv", header = TRUE)
RNHScaledVeg<-read.csv("NRHScaledVeg.csv", header = TRUE)
RNHSampleCovar_all<-read.csv("RNHSampleCovar_all.csv", header = TRUE)
```
```{r}
#Remove un-needed columns
Vegetation_scaled<-Vegetation_scaled[,-1] 
```
```{r}
# Create PAO file
RNHpao<-createPao(data= RNHDetectHist, 
                   unitcov = RNHScaledVeg, 
                   survcov = RNHSampleCovar_all
)


modCombos(RNHDetectHist,RNHScaledVeg)
```
```{r}
# remove rows
RNHpao$survcov <- (RNHSampleCovar_all[,-1])
```
Sanity checks
```{r}
# Check the first five rows of the detection data from PAO file
head(RNHpao$det.data) 
# Check the site covariate data from PAO file
head(RNHpao$unitcov) 
# Check survey covariates data from PAO file
head(RNHpao$survcov)
```
```{r}
# (optional - unhash) removing the survey column that isn't needed
#NRHpao$survcov<-(NRHpao$survcov[,-5])

```
```{r}

# Looking at the individual parts of the pao file to check correctness 
RNHdet.data<-(RNHpao$det.data)
RNHunitcov<-(RNHpao$nunitcov)
RNHsurvcov<-(RNHpao$survcov)

```
---
### 2. Null model

```{r}
RNH_null<-occMod(model = list(lambda~1, c~1), data = KIRApao, type = "AHO", link = logit)


summary(RNH_null)
```
---
### 3. Lambda (Management),r(.)
```{r}
RNH_mgmnt<-occMod(model = list(lambda~Management, c~1), data = KIRApao, type = "RNH")


#summary(RNH_mgmnt)


#nrow(RNHpao) #NULL
#length(RNHpao) #16

```
---
### 4. Lambda (Juncus),r(.)
```{r}
RNH_mgmnt<-occMod(model = list(lambda~Juncus.sp., c~1), data = KIRApao, type = "RNH")


#summary(RNH_mgmnt)


#nrow(RNHpao) #NULL
#length(RNHpao) #16

```
---

### 5. Lambda (Phrag),r(.)
```{r}
RNH_mgmnt<-occMod(model = list(lambda~Phragmites, c~1), data = KIRApao, type = "RNH")


#summary(RNH_mgmnt)


#nrow(RNHpao) #NULL
#length(RNHpao) #16

```


