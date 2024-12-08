## Simple violin plots 

1. Library
```{r}
library(ggplot2)
```
&nbsp;

2. Read in the data

```{r}
KRD<- read.csv(file="data.csv", header=TRUE)
```
&nbsp;

3. Plot
```{r}
# Custom colors
MyColors <- list("#B54141", "#288076")

#Save
png(file="Woody.png", width = 700, height = 600)


KRD_violin <- ggplot(KRD, aes(x = Words, y = Woody, fill = Detected_Y.N)) +
  geom_violin(colour = "black", alpha = 0.5) +
  labs(
    y = bquote("Woody vegetation"),
    x = bquote(""),
    size = 14,
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(margin = margin(l = 25))  # Adjust the right margin for y-axis text
  )

KRD_violin + scale_fill_manual(values = MyColors)



dev.off()

```


## Bar Plot for displaying breakdown of surveyed vegetation
&nbsp;

1. Load necessary libraries
```
library(ggplot2)
library(dplyr)
library(tidyr)
```
&nbsp;

2. Read in the data

```{r}
veg<- read.csv(file="veg_data.csv", header=TRUE)
```
&nbsp;

3. Reshape data to long format
```
long_data <- veg %>%
  pivot_longer(
    cols = -Site, 
    names_to = "Vegetation_Type", 
    values_to = "Percentage"
  )
```
&nbsp;

4. Colors
```
colors <- c(
  "Black_Needlerush" = "#1b9e77",
  "Cattail" = "#d95f02",
  "Phragmites" = "#7570b3",
  "Grasses_and_Shrubs" = "#e7298a",
  "Common_Threesquare_Bulrush" = "#66a61e",
  "Woody_Vegetation" = "#e6ab02",
  "Mixed_Emergents" = "#a6761d"
)
```
&nbsp;

5. Plot
```
ggplot(long_data, aes(x = Percentage, y = Site, fill = Vegetation_Type)) +
  geom_bar(stat = "identity", position = "stack", width = 0.8) +  # Adjust bar width
  scale_fill_manual(
    values = colors, 
    labels = c(
      "Black_Needlerush" = "Black needlerush",
      "Cattail" = "Cattail",
      "Phragmites" = "Invasive common reed",
      "Grasses_and_Shrubs" = "Grasses and shrubs",
      "Common_Threesquare_Bulrush" = "Three-square bulrush",
      "Woody_Vegetation" = "Woody vegetation",
      "Mixed_Emergents" = "Mixed emergents"
    )
  ) +
  labs(
    x = "Total Vegetation Type Across Survey Plots (%)", 
    y = NULL, 
    fill = "Vegetation Type"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 12),      
    axis.text.x = element_text(size = 12),       
    legend.title = element_text(size = 14),      
    legend.text = element_text(size = 12),       
    panel.grid.major.y = element_blank()         
  )
```

