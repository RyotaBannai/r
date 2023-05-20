library(tidyverse)
library(magrittr)
library(dplyr)
library(jsonlite)

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy))

mpg

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

ggplot(data = mpg) +
  geom_point(mapping = aes(x = displ, y = hwy, alpha = class))

pp <- function(any) {
  any %>%
    toJSON() %>%
    prettify()
}
