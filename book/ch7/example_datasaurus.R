# example_datasaurus.R
# illustrates datasets with the same mean, sd, corr can be quite different
# see https://cran.r-project.org/web/packages/datasauRus/vignettes/Datasaurus.html

# load libraries
library(datasauRus)
library(dplyr)
library(ggplot2)

# generate a summary of the data using dplyr
# notice that %>% means to pipe, so data %>% summarize() is same as summarize(data)
datasaurus_dozen %>% 
  group_by(dataset) %>% 
  summarize(
    mean_x    = mean(x),
    mean_y    = mean(y),
    std_dev_x = sd(x),
    std_dev_y = sd(y),
    corr_x_y  = cor(x, y)
  )

# plot each of the 13 datasets
ggplot(datasaurus_dozen, aes(x=x, y=y, colour=dataset))+
  geom_point()+
  theme_void()+
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol=7)
