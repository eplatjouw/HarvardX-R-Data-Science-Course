data(murders)
head(murders)

library(dslabs)
data(heights)
head(heights)
x <- heights$height
length(unique(x))
tab <- table(x)
help(table)
tab

#frequency table
prop.table(table(heights$sex))

# Normal Distributions
average <- sum(x) / length(x)
SD <- sqrt( sum( (x-average)^2 / length(x)))

  #Average and SD of Male heigths
  index <- heights$sex =="Male"
  x<- heights$height[index]
  x
  index

average <-mean(x)
SD <- sd(x)
c(average=average, SD=SD)

  #standard units
z<- scale(x)
mean(abs(z) <2)


library(dslabs)
data(heights)
x <- heights$height[heights$sex=="Male"]
avg <- mean(x)
stdev <- sd(x)
pnorm(72,mean=avg,sd=stdev)-pnorm(69,mean=avg, sd=stdev)

#Quantiles
p<- seq(0.05,0.95, 0.05)
observed_quantiles <-quantile(x,p)
theoretical_quantiles <- qnorm(p, mean=mean(x), sd=sd(x))
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

#Percentiles
#Quartiles
#Boxplots

#exercises
x <- Galton$child
error_avg <- function(k){
  x <- x[1]=k
  mean(x)
}
k=10000
error_avg(x)

#ggplot (load it together with dplyr and other usefull packages with tidyverse)
install.packages("tidyverse")
library(tidyverse)

library(dslabs)
data(murders)

ggplot(data=murders)
murders %>% ggplot() #pipe the data in the ggplot
p <- ggplot(data=murders)
class(p)
print(p)
p

#add mapping or aestethic mapping meaning the kind of plot
help(geom_point)

murders %>% ggplot()  +
 geom_point(aes(x=population/10^6, y = total))

p + geom_point(aes(x=population/10^6, y = total))

#add labels
#geom_label (adds with a little rectangle)
#geom_text( just adds text)

help("geom_text")

murders %>% ggplot() +
  geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

#tinkering 

murders %>% ggplot() +
  geom_point(aes(population/10^6, total), size=3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x= 1)

args(ggplot) #define mapping in ggplot, all geom added will default to this mapping

p <- murders %>% ggplot(aes(population/10^6, total, label = abb))
p + geom_point(size = 3) +
  geom_text(nudge_x= 1.5)

#??? Local mappings override the global ones, e.g.
p+geom_point(size=3) +
  geom_text (aes(x =10, y=800, label = "Hello there!"))

#scales lalbels colors lines

  #apply log10transformation
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

p + geom_point(size = 3) +
  geom_text(nudge_x= 0.075)+
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10")

p + geom_point(size = 3) +
  geom_text(nudge_x= 0.075)+
  scale_x_log10() +
  scale_y_log10()

  # add labels and titles
p + geom_point(size = 3) +
  geom_text(nudge_x= 0.075)+
  scale_x_log10() +
  scale_y_log10()+
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)")+
  ggtitle("US Gun Murders in US 2010")

  #Add colour legend and styles
  #??? first redefine this code to look at the effects of different colours
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))+geom_text(nudge_x= 0.075)+
  scale_x_log10() +
  scale_y_log10()+
  xlab("Populations in millions (log scale)") +
  ylab("Total number of murders (log scale)")+
  ggtitle("US Gun Murders in US 2010")

p+ geom_point(size =3, color ="blue")
p+ geom_point(aes(col=region),size = 3)

  # add average line

r <- murders %>% summarize(rate = sum(total)/sum(population)*10^6)  %>% .$rate
 
p + geom_point(aes(col= region), size=3)+
  geom_abline(intercept = log10(r))

  # change properties of the line and put it behind points

p <- p + geom_abline(intercept = log10(r), lty =2, color ="darkgrey") +
  geom_point(aes(col=region), size=3)

  # Capitalize the R in Region in the legend
p <- p + scale_color_discrete(name = "Region")

# Add-on packages 
#ggthemes allows to change the style of a ggplot
install.packages("ggthemes")
library(ggthemes)
p+theme_economist()
p+theme_fivethirtyeight()

#ggrepel adds labels ensuring they don't fall on top of each other
install.packages("ggrepel")
library(ggrepel)

#### Now the whole plot step by step
###Add libraries
library(ggplot2)
library(ggthemes)
library(ggrepel)
##First define the slope of the line
r <- murders %>% summarize(rate = sum(total)/sum(population)*10^6)  %>% .$rate
#Now make the plot
murders %>% ggplot(aes(population/10^6, total, label = abb))+
  geom_abline(intercept=log10(r),lty=2, color="darkgray")+
  geom_point(aes(col=region), size=3)+
  geom_text_repel()+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Populations in millions (log scale)")+
  ylab("Total number of murders (log scale)")+
  ggtitle("US Gun Murders in US 2010")+
  scale_color_discrete(name="Region")+
  theme_economist()


### Other Plots
##histogram for male heights
heights %>% filter(sex=="Male")

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))

p+geom_histogram()

  #add binwidth
p+geom_histogram(binwidth = 1)

  #add color and title
p+geom_histogram(binwidth = 1, fill ="blue", col ="black")+
  xlab("Male heights in inches")+
  ggtitle("Histogram")

## Smooth Density plots
heights %>% filter(sex=="Male")

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))

p+geom_density()

  #add color and title
p+geom_density(fill ="red", col ="black", linetype=1, size =1)+
  xlab("Male heights in inches")+
  ggtitle("Smooth Density")

help("geom_density")

##Q-Q plot (specify sample instead of x!!)
heights %>% filter(sex=="Male")

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(sample=height))

p+geom_qq()

  # By default Q-Q plot is compared to standard normal distribution
  # with avg 0 and SD 1 (need to change those parameters)
params <- heights %>% filter(sex=="Male") %>% summarize (mean =mean(height), sd= sd(height))

p + geom_qq(dparams = params)

  # add identity lines
p + geom_qq(dparams = params)+
  geom_abline()

  #First scale data to make it follow standard normale distribution
heights %>% filter(sex=="Male") %>% ggplot(aes(sample=scale(height)))+
  geom_qq()+
  geom_abline()


## Make grids of plots
install.packages("gridExtra")
library(gridExtra)

p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p +geom_histogram(binwidth= 1, fill ="blue", col="black")
p2 <- p +geom_histogram(binwidth= 2, fill ="red", col="black")
p3 <- p +geom_histogram(binwidth= 3, fill ="purple", col="black")

grid.arrange(p1,p2,p3, ncol =3)




### Summarizing Data
library(tidyverse)

##Summarize function computes summary statistics with intuitive and readable code

#summarize() function
library(dslabs)
data(heights)
  
s  <- heights %>% filter(sex == "Male") %>% summarize(average=mean(height), standard_deviation= sd(height)) 
s
class(s)

s$average
s$standard_deviation

heights %>% filter (sex=="Male") %>% summarize (median = median(height),
                                                minimum =min(height),
                                                maximum = max(height))
quantile(heights$height, c(0,0.5,1))


#dot placeholder (%>% .$rate): make dplyr return vectors instead of data.frames
# refresh, use dplyr to add column
data(murders)
murders <- murders %>% mutate(murder_rate=total/population*100000)

summarize(murders,mean(murder_rate))

us_murder_rate <- murders %>% summarize(rate = sum(total)/sum(population)*100000)
us_murder_rate        
class(us_murder_rate)

us_murder_rate %>% .$rate
class(us_murder_rate %>% .$rate)

us_murder_rate <- murders %>% summarize(rate = sum(total)/sum(population)*100000) %>% .$rate

#group_by: group then summarize

heights %>% group_by(sex)
class(heights %>% group_by(sex))

heights %>% group_by(sex) %>% summarize (avg =mean(height), stdev= sd(height))

murders %>% group_by(region) %>% summarize( median_rate =median(murder_rate))

#Sorting data tables: ordering entire tables using the arrange() function in dplyr
 
 #default is sorting in ascending order
murders %>% arrange(population) %>% head()
murders %>% arrange(murder_rate)%>% head()
 
  #arrange in descending order
murders %>% arrange(desc(murder_rate)) %>% head()

  #Nested Sorting
murders %>% arrange(region, murder_rate) %>% head()

  #top_n() function shows the first n rows
murders %>% top_n(10, murder_rate)

  #now to order the top 10
murders %>% arrange(desc(murder_rate)) %>% top_n(10)



### Gapminder Dataset
library(dslabs)
data(gapminder)
head(gapminder)
summary(gapminder)

gapminder %>% filter(year==2015, country %in% c("Sri Lanka", "Turkey")) %>% select(country, infant_mortality)

ds_theme_set()
filter(gapminder, year==1962) %>% ggplot(aes(fertility, life_expectancy))+
  geom_point()

filter(gapminder, year==1962) %>% ggplot(aes(fertility, life_expectancy, color=continent))+
  geom_point()

## Faceting: stratify the data for some variable and make the same plot for each strata
#function facet_grid()

filter(gapminder, year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, col=continent))+
  geom_point()+
  facet_grid(continent~year)

  # More than what we want, no need to stratify by continent so . instead 
filter(gapminder, year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility, life_expectancy, col=continent))+
  geom_point()+
  facet_grid(.~year)

#function facet_wrap same but automatically adapts so that most displays have viewable dimensions with more plots
years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
filter(gapminder, year %in% years, continent %in% continents) %>% 
  ggplot(aes(fertility, life_expectancy, col=continent))+
  geom_point()+
  facet_wrap(.~year)


## Time series plots
gapminder %>% filter(country=="United States") %>%
  ggplot(aes(year, fertility))+
  geom_point()

  #geom_line instead of geom_point to connect the dots
gapminder %>% filter(country=="United States") %>%
  ggplot(aes(year, fertility))+
  geom_line()

  #Use more countries (next example not what we want)
countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility))+
  geom_line()

  # Seperate the lines for each country
countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, group=country))+
  geom_line()

  #Distinguish countries
countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, fertility, col=country))+
  geom_line()

  #Labelling curves (preferred over legends)
labels <- data.frame(country = countries, x=c(1977,1965), y=c(60,72))
countries <- c("South Korea","Germany")
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col=country))+
  geom_line()+
  geom_text(data = labels, aes(x,y,label=country), size =5)+
  theme(legend.position = "none")


##Transformations

gapminder <- gapminder %>% mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>% 
  filter(year == past_year, !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color ="black")
  
  #log transformations change multiplicative changes in additive ones
gapminder %>% 
  filter(year == past_year, !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day)))+
  geom_histogram(binwidth = 1, color ="black")
  # 2 bumps are shown, referred as modes
  #modes are the values with the highest frequency (e.g mode normal distribution is average)

  #Transform the scales so we get transformed data in the plot but original data on the x-axis for easier interpretation
gapminder %>% 
  filter(year == past_year, !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color ="black")+
  scale_x_continuous(trans="log2")
  
  
##Stratify and Boxplot
#Previous histogram did not show to which regions the modes belonged to
#To see distributions bu regions, we stratify data into regions and examine the distribution for each

length(levels(gapminder$region))
p <- gapminder %>% filter(year== past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))

p+geom_boxplot()

  #rotate labels through a theme
p+geom_boxplot()+
  theme(axis.text.x= element_text(angle=90, hjust = 1))

  #Order them other than the standard alphabethical function
  #reorder() function
  #First example to understand how the function works

fac <- factor(c("Asia", "Asia", "West", "West", "West"))
levels(fac) #ordered alphabethically
value <- c(10,11,12,6,4)
fac <- reorder(fac, value, FUN=mean)
levels (fac) #test first because it has a smaller mean value in the value vector

  # Back to boxplot
p <- gapminder %>% 
  filter(year== past_year & !is.na(gdp)) %>%
  mutate(region =reorder(region,dollars_per_day, FUN=median))%>%
  ggplot(aes(region, dollars_per_day, fill=continent))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  xlab("")
p
  #change scale to log scale

p + scale_y_continuous(trans ="log2")

  #Show data

p + scale_y_continuous(trans="log2") +geom_point(show.legend =FALSE)


## Comparing Distributions
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

gapminder %>%
  filter(year == past_year & !is.na(gdp))%>%
  mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color ="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(. ~group)

  # Comparing years by faceting by both region and year
past_year <- 1970
present_year <- 2010

gapminder %>%
  filter(year %in% c(past_year,present_year) & !is.na(gdp)) %>%
  mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color ="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group)

  #More countries in present year than past year so we need to adjust
country_list_1 <- gapminder %>%
  filter(year==past_year & !is.na(dollars_per_day))%>% .$country
country_list_2 <- gapminder %>%
  filter(year==present_year & !is.na(dollars_per_day))%>% .$country
country_list <- intersect(country_list_1, country_list_2)

gapminder %>%
  filter(year %in% c(past_year,present_year) & country %in% country_list) %>%
  mutate(group = ifelse(region%in%west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1, color ="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group)

  #See which countries we can use the boxplots we used before 
p <- gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(region =reorder(region,dollars_per_day, FUN=median))%>%
  ggplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  xlab("") +scale_y_continuous(trans="log2")

p + geom_boxplot(aes(region, dollars_per_day, fill= continent))+
  facet_grid(year~.)

  #Hard to compare -> ease the comparisons through another feature
  # Keep the data together but color them by year

p+ geom_boxplot(aes(region, dollars_per_day, fill =factor(year)))


## Density plots
#Smooth Density Plots

gapminder %>%
  filter (year == past_year & country %in% country_list) %>%
  mutate(group= ifelse(region %in%west, "West", "Developing")) %>%
  group_by(group) %>%
  summarize(n=n()) %>%
  knitr::kable()
  #the density plots make it seem there is a same number of values as they are relative to a density of 1
  #remedy this with the geom_density() function
help(geom_density)

  # putting a variable on the y-axis you surround it by ..

aes(x=dollars_per_day, y = ..count..)  

p <- gapminder %>%
  filter (year %in% c(past_year, present_year) & country %in% country_list) %>%
  mutate(group= ifelse(region %in%west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y=..count.., fill=group))+
  scale_x_continuous(trans="log2")

p + geom_density(alpha =0.2) + facet_grid(year ~.)

  #smoother density
p + geom_density(alpha =0.2, bw= 0.75) + facet_grid(year ~.)

  # Show key regions seperately with case_when() function
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~"The West",
    .$region %in% "Northern Africa" ~"Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~"East Asia",
    .$region == "Southern Asia" ~"Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~"Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~"Sub-Saharan Africa",
    TRUE ~"Others"))

  #Make group a factor
gapminder <- gapminder %>% 
  mutate(group = factor(group, levels = c("Other", "Latin America",
                                          "East Asia", "Sub-Saharan Africa",
                                          "West")))

p + geom_density(alpha=0.2, bw= 0.75, position ="stack")+
  facet_grid(year ~.)

  #Weigth Smooth Densities using weight mapping 

gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year)%>%
  mutate(weight=population/sum(population)*2) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill =group, weight =weight))+
  scale_x_continuous(trans ="log2")+
  geom_density(alpha =0.2, bw =0.75, position ="stack")+
  facet_grid(year~.)


##Ecological Fallacy (when you conclude about every individual value within a group when comparing group averages, (all sub-saharan countries higher mortality then Southern Asia ones))

  # Find a few more regions 
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~"The West",
    .$region %in% "Northern Africa" ~"Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~"East Asia",
    .$region == "Southern Asia" ~"Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~"Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~"Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia")~"Pacific Islands"
  ))

surv_income <- gapminder %>%
  filter(year %in% c(2017) & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income=sum(gdp)/sum(population)/365,
            infant_survival_rate= 1- sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

surv_income %>% ggplot(aes(income, infant_survival_rate, label =group, color =group))+
  scale_x_continuous(trans= "log2", limit=c(0.25, 150))+
  scale_y_continuous(trans="logit", limit =c(0.875, .9981), breaks =c(0.85,0.90,.95,.99,.995,.998))+
  geom_label(size =3, show.legend=FALSE)


## Color Blind Friendly colour
colorblind_pal()
color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
p1 <- data.frame(x=1:8, y=1:8, col=as.character(1:8)) %>% ggplot(aes(x,y,color=col)) + geom_point(size=6)
p1 +scale_color_colorblind()
p1+scale_color_manual(values=color_blind_friendly_cols)


## Slope Charts

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
dat <- gapminder %>% filter(year%in%c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location= ifelse(year == 2010, 1, 2),
         location= ifelse(year==2015 & country %in% c("United Kingdom", "Portugal"),
                          location+ 0.22, location),
         hjust = ifelse (year == 2010, 1,0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group =country))+
  geom_line(aes(color=country), show.legend= FALSE) +
  geom_text(aes(x=location, label=country, hjust= hjust),
            show.legend =FALSE) +
  xlab("")+ ylab("Life Expectancy")

## shape() function lets you change shape of the points in the scatterplot

data(us_contagious_diseases)
str(us_contagious_diseases)

the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Alaska", "Hawaii") & disease== the_disease) %>%
  mutate(rate= count/population *10000) %>%
  mutate(state =reorder(state,rate))

dat %>% filter (state=="California") %>% ggplot(aes(year,rate)) +
  geom_line()+ylab("Cases per 10,000") +
  geom_vline(xintercept= 1963, col ="blue")

## sequential color hues can be used for values from high to low
# Example from a R library
library(RColorBrewer)
display.brewer.all(type="seq")
## Divergent color hues are used for values divergin from the center
#Example from a R library
library(RColorBrewer)
display.brewer.all(type="div")

#use geom_tile()

dat %>% ggplot(aes(year, state, fill=rate))+
  geom_tile(color="grey50")+
  scale_x_continuous(expand=c(0,0))+
  scale_fill_gradientn(colors = brewer.pal(9, "Reds"), trans ="sqrt")+
  geom_vline(xintercept=1963, col="blue")+
  theme_minimal() +theme(panel.grid = element_blank())+
  ggtitle(the_disease)+
  ylab("")+
  xlab("")

## Time series plot (from exercise)
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)

the_disease = "Smallpox"
dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

###Change the number of significant numbers digits or to round numbers 
### With functions signif() and round()
###Define the number of digits used globally using the following code
options(digits=n)



