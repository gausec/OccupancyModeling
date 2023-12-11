# Libraries
library(unmarked)

# User input 
readNumericInput <- function(prompt) {
  cat(prompt)
  input <- readline(prompt = "")
  matrix(as.numeric(strsplit(input, " ")[[1]]), nrow = 1)
}

# Data
detectHistName <- readline("Enter the name of the detection history data frame: ")
siteCovsName <- readline("Enter the name of the site covariates data frame: ")

# 3. Detections
DetectHist <- readNumericInput(paste("Enter detection history (0/1 data) from", detectHistName, ":\n"))

# 4. Site covs
SiteCovs <- readNumericInput(paste("Enter site covariates (z-transformed) from", siteCovsName, ":\n"))

# 5. UnmarkedFrameOccu object
unmarkedFrame_cov <- unmarkedFrameOccu(
  y = DetectHist,
  siteCovs = SiteCovs
)

# 6. Summarize detections
detHist(unmarkedFrame_cov)

