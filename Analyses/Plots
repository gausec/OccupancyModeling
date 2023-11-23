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



library(ggplot2)

KRD_violin <- ggplot(KRD, aes(x = Words, y = Woody, fill = Detected_Y.N)) +
  geom_violin(colour = "black", alpha = 0.5) +
  labs(
    y = bquote("Woody vegetation"),
    x = bquote(italic("")),
    size = 14,
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(margin = margin(l = 25))  # Adjust the right margin for y-axis text
  )

KRD_violin + scale_fill_manual(values = MyColors)



dev.off()

```
