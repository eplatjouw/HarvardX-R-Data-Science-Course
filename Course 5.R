### Instaling packages

install.packages("tidyverse")
install.packages(c("tidyverse", "dslabs"))

### Running commands while editing scripts
    # Ctrl + Shift + Enter to Source with Echo
library(tidyverse)
library(dslabs)
data(murders)

murders %>% 
  ggplot(aes(population, total, label = abb, color = region)) +
  geom_label()

