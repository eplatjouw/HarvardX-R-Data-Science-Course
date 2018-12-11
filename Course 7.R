library("tidyverse")
library("dslabs")
library("dplyr")
library("ggplot2")
# install.packages("Lahman")
library("Lahman")

head(Teams)
summary(Teams)
?Teams

# More Home runs = more runs?
ds_theme_set()
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game =R/G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha =0.5)

# Relationship runs per game vs stolen ases per game
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G, R_per_game =R/G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha =0.5)

# Base on balls vs runs

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G, R_per_game =R/G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha =0.5)

### Correlation
# install.packages("HistData")
library("HistData")
data("GaltonFamilies")
galton_heights <- GaltonFamilies %>%  # How far can we predict height son from father height
  filter(childNum == 1 & gender =="male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

  # Both are well approximated by normal distributions o we can use avg and var of both
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))
  # Fails to describe a very important characteristic of the data
galton_heights %>% ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)
    #Trend the taller the father the taller the son is not described by summariy of average and sd 
    # Correlation coefficient is the summary of this trend

  ## Correlation between Father and son
galton_heights <- GaltonFamilies %>%  
  filter(childNum == 1 & gender =="male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

galton_heights %>% summarize(cor(father, son))


### Sample Correlation, correlation of a sample of the total population
set.seed(0)
R <- sample_n(galton_heights, 25, replace = TRUE) %>% #Random variable
  summarize(cor(father, son))

  # Monte Carlo simulation to see distribution of this random variable
B <- 1000
N <- 25
R <- replicate(B,{
  sample_n(galton_heights, 25, replace = TRUE) %>% #Random variable
    summarize(r= cor(father, son)) %>% .$r
  })
data.frame(R) %>% ggplot(aes(R)) + geom_histogram(binwidth = 0.05, color = "black")

mean(R)
sd(R)

  # CLT applies

### Anscombe's quarter/Stratification
## Correlation is not always a good summary of the relationsship between two variables
## Visualize it
## Only meaningfull in particular context

  ## Stratifying, contitional average
contitional_avg <- galton_heights %>% filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>% .$avg
contitional_avg

    # Stratification followed by boxplots
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()
  
    # Means of the groups appear to follow a linear relationship
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()
    # Slope of line appeared to be followed appears to be 0.5

    # See connection by plot standardized heights against each other, son versus father
    # with line with slope equal to correlation
r <- galton_heights %>% summarize(r= cor(father, son)) %>% .$r
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son = mean(son)) %>%
  mutate(z_father = scale(father), z_son = scale(son)) %>%
  ggplot(aes(z_father, z_son)) +
  geom_point() +
  geom_abline(intercept = 0, slope = r) # <- Regression line

  ## Look at original data, father and son data and add regression line
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y / s_x
b <- mu_y - m*mu_x

galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope =m)

  # If plot in standard units then regression line intercept 0 and slope rho
galton_heights %>%
  ggplot(aes(scale(father), scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)

### Bivariate Normal Distrubtion
    ## Correlation and regression often misused
  ## Bivariate normal distribution is defined by pairs and have paired values

galton_heights %>%
  mutate(z_father = round((father - mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)

###  There are Two Regression Lines
  ## Regression line to predict son's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y / s_x
b <- mu_y - m*mu_x

  ## Regression line to predict fathers height based on son's height?
    # Need to compute expected value of x given y
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_x / s_y
b <- mu_x - m*mu_y
    # Different regression line


### Confounding, is BB more predictive?
  ## Association is not causation
    # BB higher, singles lower, 
    # Note correlation between homeruns, bases on balls and singles
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles =(H-HR-X2B -X3B)/G, BB = BB/G, HR =HR/G) %>%
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB, Singles))
    # BB are caused by homeruns, they are confounded by homeruns
    # but do they still help? Adjust for homerun effect -> regression


### Stratification and Multivariate regression
## Fix homeruns and examine relationship between runs and bases on balls
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1),
         BB_per_game = BB/G,
         R_per_game = R/G) %>%
  filter(HR_strata >= 0.4 & HR_strata <= 1.2)

  # scatterplot for each data
dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method ="lm") +
  facet_wrap(~HR_strata)

  # See slopes
dat %>%
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))
    # Values closer to singles, should have the same predictive power

  # After stratifying by base on balls, still see a home run effect?
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1),
         HR_per_game = HR/G,
         R_per_game = R/G) %>%
  filter(BB_strata >= 2.8 & BB_strata <= 3.9)

dat %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method ="lm") +
  facet_wrap(~BB_strata)

dat%>% group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)* sd(R_per_game)/sd(HR_per_game))


### Linear models
  ## A model that describes the relationship between two or more variables 


### Least Squares Estimates (LSE)
  ## Residual Sum of Squares (RSS)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
    # 3D with beta0 and beta1 as x and y and RSS as z
    # 2D by keeping beta0 fixed at 25
beta1= seq(0,1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) +geom_line() +
  geom_line(aes(beta1, rss), col=2)

  ##$ LSE or least squares estimate using the lm() function
fit <- lm(son ~ father, data = galton_heights) # left side of ~ value we are predicting and on the right side of the ~ which one we use to predict
fit

  # summary() to extract more info
summary(fit)

  # Monte Carlo if we assume data we have is entire population
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    lm(son ~ father, data = . ) %>% .$coef
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,])
lse
  # See variablity of estimates by plotting the distribution
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color ="black")
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color ="black")
grid.arrange(p1, p2, ncol = 2)

sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>% summary

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta1))


### LSE can be strongly correlated
lse %>% summarize(cor(beta_0, beta_1))
  ##correlation depends on how the predictors are defined or transformed.
    ##Here we standardize the father heights
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 

### Predicted Variables are Random Variables
  ## Plot confidence intervals around ^y
galton_heights %>% ggplot(aes(son, father)) +
  geom_point()+
  geom_smooth(method ="lm")

  ## R function predict() takes lm object as input and returns these predictions
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data = .))) %>%
  ggplot(aes(father, Y_hat)) +
  geom_line()
    ## Request SE qnd other info from predict function to construct CI
fit <- galton_heights %>% lm(son ~ father, data = .)
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)


### Advanced dplyr: Tibbles
  ## Estimate regressions lines to predict runs from BB in different HR strata
    # First construct data frame
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1),
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR <= 1.2)
dat
    # Compute regression line in each strata (since we didn't know lm() function 
dat %>%
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))
      # argued that slopes are +/- the same and differences due to random variance -< led to multivariate regression model
      # CI for each slope
      # lm() function provides enough information to construct them

dat %>%
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef # doesn't get what we want
        # lm() function ignored the group_by

      # lm() not part of the tidyverse package so doesn't know how to handle it
    ## When summarize recieves the output of group_by, it knows which rows of the table go with which groups
      # Storred where in data frame?
dat %>% group_by(HR) %>% head()# no columns with info needed to define groups
dat %>% group_by(HR) %>% class()# class is tbl = tibble and is a special kind of data frame
      # group_by returns special kind of tibble, a grouped tibble
      # select(), filter(), mutate(), arrange() return not always a tibble but preserve the class of input
      # Tibble default data frame of tidyverse

### Differences between tibbles and data frames
  ## Print method is more readable

Teams # almost not readable
as_tibble((Teams)) # more readable

  ## subset of tibbles are tibbles
    #subset the columns of a data frame and you may get back an object that is not a data frame.
class(Teams[,20])
class(as_tibble(Teams[,20]))

  ## access specific column with .$
    # if not exist, tibble will give a warning, data.frame not
Teams$hr
as_tibble(Teams)$hr

  ## Tibbles can have complex entries
    # such as losts or functions
    # data frames need a vector of number strings or Boolean
      # create tibbles with the tibble() function
tibble(id = c(1,2,3), func = c(mean, median, sd))

  ## tibbles can be grouped
    # function group_by returns a grouped tibble
    # This class stores information that lets you know which rows are in which groups
    # tidyverse function are aware of these group information
dat %>%
  group_by(HR) %>%
  lm(R ~ BB, data = .)

    # new function do()

### do()
  ## Tidyverse functions know how to interpret tibbles
  ## To facilitate stringing commands through %>% tidyverse functions return data frames
    # assures output of 1 can be accepted as input of other
  ## most other function do not recognize group tibbles nor return data frames

  ## do() function serves as bridge between R functions and tidyverse
    # understands group tibbles and always returns data.frames
dat %>%
  group_by(HR) %>%
  do(fit =lm(R ~BB, data = .))
    # first column strata value, second column not usefull as result of lm() call = lm() objects in the cells
    # name a column, otherwise output is actual output of lm(), not a data frame -> error
dat %>%
  group_by(HR) %>%
  do(lm(R ~BB, data = .))

    # For useful data frame, output of the function, inside do() must be a data frame as well
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficient[2,2])
}

dat %>%
  group_by(HR)%>%
  do(get_slope(.))

      # If we name the output, we get a column containing the data.frame
dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.)) # not usefull

  ## On more feature of do(), if the data frame returned has more than 1 row, these will be concatenated appropriately
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             slope = fit$coefficients,
             se = summary(fit)$coefficient[,2])
}

dat %>%
  group_by(HR) %>%
  do(get_slope(.))

  ## broom package to simplify things

### Broom
  ## 3 functions which all extract information from object returned by the function lm() and returns it tidy friendly
  ## tidy() , glance(), augment()

  ## tidy() function returns estimates and related information as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)
  ## add other important summaries such as CI like this
tidy(fit, conf.int = TRUE)

  # as it is a data frame we can combine with do()
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE))

  # because data frame returned we can filter and select and other tidyverse functions
dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

  # makes visualization with ggplot easy

dat %>%
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data= .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

  # Gives nice visualization that CI overlap and slopes not change with home strata

## glance() relates to model-specific outcomes
## augment() relates to observation-specific outcomes

glance(fit)


### Building a Better Offensive Metric for Baseball
## Multiple regression using + in lm() function to predict 2 variables
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G, HR = HR/G, R = R/G ) %>%
  lm(R ~ BB + HR, data = .)

tidy(fit, conf.int = TRUE)

  ## When fitiing only one model
    # BB slope : 0.735
    # HR slope : 1.844
  ## Multivariate model (both go down)
    # BB slope: 0.387
    # HR slope: 1.56

# Predict runs based also on singles doubles and triples
  # assume they are jointly normal = independant and relationship with outcome is linear

  # find LSE for parameters using lm()
fit <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  lm(R ~BB + singles + doubles + triples + HR, data = .)

    # tidy function to see 
coefs <- tidy(fit, conf.int = TRUE)
coefs

## How well does our metric actually predict?
  # predict runs of each team in 2002 using predict() function
  # we did not use 2002 year to create this metric
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB = BB/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles = X2B/G,
         triples = X3B/G,
         HR = HR/G,
         R = R/G) %>%
  mutate(R_hat = predict(fit, newdata = .)) %>%
  ggplot(aes(R_hat, R, label = teamID)) + 
  geom_point() +
  geom_text(nudge_x=0.1, cex = 2) + 
  geom_abline()
  
  # fits pretty good use 
  
## Defined metric for teams, how to define player specific metric 
  ## Per-plate-appearance rate is the rate of opportunities taken by a player
  ## per-game-team rate compared to per-plate-appearance player rate
      # compute the average number of team plate appearances per game
pa_per_game <- Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game = sum(AB+BB)/max(G)) %>%
  .$pa_per_game %>%
  mean

  ## Compute per_plate_appearance rates for players available in 2002 based on previous data

players <- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA = BB + AB) %>%
  summarize(G = sum(PA)/pa_per_game,
            BB = sum(BB)/G,
            singles = sum(H-X2B-X3B-HR)/G,
            doubles = sum(X2B)/G,
            triples = sum(X3B)/G,
            HR = sum(HR)/G,
            AVG = sum(H)/sum(AB),
            PA = sum(PA)) %>%
  filter(PA >= 300) %>% # avoid sample artifacts
  select(-G) %>%
  mutate(R_hat = predict(fit, newdata = .))

  # interpreted as number of runs if team was exact same players
  # Y variability across players
players %>% ggplot(aes(R_hat)) +
  geom_histogram(binwidth= 0.5, color ="Black")

  ## To build we need salaries and we have limited budget
  ## players position
  ## data wrangling needed
  
    # Add salaries
players <- Salaries %>%
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

    # Add most played position using top_n() function
      # only one in case of ties use first row
      # remove OF (outfielder position) =generalisation of 3 position and pitcher as they don't bat

players <- Fielding %>% filter(yearID == 2002) %>%
  filter(!POS %in% c("OF", "P")) %>%
  group_by(playerID) %>%
  top_n(1, G) %>%
  filter(row_number(G) == 1) %>%
  ungroup() %>%
  select(playerID, POS) %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS) & !is.na(salary))

  # Add names and last names
players <- Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  right_join(players, by="playerID")


players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)

  # Players with high metrics have high salaries
players %>% ggplot(aes(salary, R_hat, color = POS)) +
  geom_point()+
  scale_x_log10()

  # PLOT WITH PLAYERS THAT DEBUTED BEFORE 1997 to remove rokies
players %>% filter(debut < 1998) %>%
  ggplot(aes(salary, R_hat, color = POS)) +
  geom_point() +
  scale_x_log10()


### Linear Programming
  
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= 1997 & debut > 1988)
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

  #   This algorithm chooses these 9 players:
  
  our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)


 #  We note that these players all have above average BB and HR rates while the same is not true for singles.

my_scale <- function(x) (x - median(x))/mad(x)
players %>% mutate(BB = my_scale(BB), 
                   singles = my_scale(singles),
                   doubles = my_scale(doubles),
                   triples = my_scale(triples),
                   HR = my_scale(HR),
                   AVG = my_scale(AVG),
                   R_hat = my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst, nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))


### On base Plus Slugging (OPS)

### Regression Fallacy
  ## sophomore slumps in data?
  ## Create statistics for all rookies of the year
    # create table with player ID, names and most played position
library(Lahman)
playerInfo <- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)

  # Create table with only Rookie of the year Award winners 
    # add batting statistics
    # filter out pitchers (not granted award) and focus on offense
    # batting average
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year") %>% 
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>% 
  mutate(AVG = H/AB) %>% 
  filter(POS != "P")
    # Now only keep rookie and sophomore seasons
    # Remove players that did not play sophomore season
ROY <- ROY %>%  
  filter(yearID == rookie_year | yearID == rookie_year+1) %>% 
  group_by(playerID) %>% 
  mutate(rookie = ifelse(yearID == min(yearID), "rookie", "sophomore")) %>%
  filter(n() == 2) %>% 
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG) 

    # Spread function to split columns
ROY <- ROY %>% spread(rookie, AVG) %>% arrange(desc(rookie))

ROY
mean(ROY$sophomore - ROY$rookie <= 0)
    # Jitters or Jinx?
    # Look back to 2013 2014 season and check all players
    # all players batted 130 times (min requirement)
two_years <- Batting %>% 
  filter(yearID %in% 2013:2014) %>% 
  group_by(playerID, yearID) %>%  
  filter(sum(AB) >= 130) %>% 
  summarize(AVG = sum(H)/sum(AB)) %>% 
  ungroup %>% 
  spread(yearID, AVG) %>% 
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>% 
  filter(POS!="P") %>% 
  select(-POS) %>%
  arrange(desc(`2013`)) %>% 
  select(-playerID)



 #The same pattern arises when we look at the top performers: batting averages go down for the most of the top performers.

two_years


  #But these are not rookies! Also, look at what happens to the worst performers of 2013:
  

arrange(two_years, `2013`)


  #Their batting averages go up! Is this some sort of reverse sophomore slump? 
  # It is not. There is no such thing as the sophomore slump. 
  # This is all explained with a simple statistical fact: the correlation for performance in two separate years is high, but not perfect:
  
 
two_years %>% 
  ggplot(aes(`2013`, `2014`)) + 
  geom_point() 



#The correlation is: 

summarize(two_years, cor(`2013`,`2014`))


# The data look very much like a bivariate normal distribution which means we predict a 2014 batting average $Y$ for any given player that had a 2013 batting average $X$ with:
# Because the correlation is not perfect, regression tells us that, on average, expect high performers from 2013 to do a bit worse in 2014. 
# It's not a jinx; it's just due to chance. The ROY are selected from the top values of $X$ so it is expected that $Y$ will regress to the mean.


### Measurement error models

  # Up to now, all our linear regression examples have been applied to two or more random variables. 
  # We assume the pairs are bivariate normal and use this to motivate a linear model. 
  # This approach covers most real life examples of linear regression. 
  #The other major application comes from measurement errors models.
  # In these applications, it is common to have a non-random covariate, such as time, and 
  # randomness is introduced from measurement error rather than sampling or natural variability.

  #To understand these models, imagine you are Galileo in the 16th century trying to describe the velocity of a falling object. 
  #An assistant climbs the Tower of Pisa and drops a ball, while several other assistants record the position at different times.
  #Let's simulate some data using the equations we know today and adding some measurement error. 
  # The dslabs function `rfalling_object` generates these simulations:

falling_object <- rfalling_object()


# The assistants hand the data to Galileo and this is what he sees:


falling_object %>% 
ggplot(aes(time, observed_distance)) + 
geom_point() +
ylab("Distance in meters") + 
xlab("Time in seconds")

  # Galileo does not know the exact equation, but by looking at the plot above,
  # he deduces that the position should follow a parabola, which we can write like this:

    # f(x) = \beta_0 + \beta_1 x + \beta_2 x^2$$

  # The data does not fall exactly on a parabola. Galileo knows this is due to measurement error.
    #His helpers make mistakes when measuring the distance. To account for this, he models the data with:

    # Y_i = \beta_0 + \beta_1 x_i + \beta_2 x_i^2 + \varepsilon_i, i=1,\dots,n $$

      #with $Y_i$ representing distance in meters, 
      #$x_i$ representing time in seconds, and $\varepsilon$ accounting for measurement error. 
      #The measurement error is assumed to be random, independent from each other, 
      #and having the same distribution for each $i$. We also assume that there is no bias,
      # which means the expected value $\mbox{E}[\varepsilon] = 0$.

      # Note that this is a linear model because it is a linear combination of known quantities ($x$ and $x^2$ are known) 
      # and unknown parameters (the $\beta$ s are unknown parameters to Galileo). Unlike our previous examples, 
      # here $x$ is a fixed quantity; we are not conditioning.

      #To pose a new physical theory and start making predictions about other falling objects, Galileo needs actual numbers, 
      #rather than unknown parameters. 
      #The LSE seem like a reasonable approach. How do we find the LSE?

      # LSE calculations do not require the errors to be approximately normal. 
      # The `lm` function will find the $\beta$ s that will minimize the residual sum of squares:


fit <- falling_object %>% 
mutate(time_sq = time^2) %>% 
lm(observed_distance~time+time_sq, data=.)
tidy(fit)


  # Let's check if the estimated parabola fits the data. The __broom__ function `augment` lets us do this easily:
  

augment(fit) %>% 
  ggplot() +
  geom_point(aes(time, observed_distance)) + 
  geom_line(aes(time, .fitted), col = "blue")



  # Thanks to my high school physics teacher, I know that the equation for the trajectory of a falling object is: 
  
     # d = h_0 + v_0 t -  0.5 \times 9.8 t^2$$
  
      # with $h_0$ and $v_0$ the starting height and velocity respectively. 
      #The data we simulated above followed this equation and added measurement error to simulate `n` observations for dropping the ball $(v_0=0)$ from the tower of Pisa $(h_0=56.67)$. 

# These are consistent with the parameter estimates:
  

tidy(fit, conf.int = TRUE)


    # The Tower of Pisa height is within the confidence interval for $\beta_0$, the initial velocity 0 
    #is in the confidence interval for $\beta_1$ (note the p-value is larger than 0.05), and the acceleration constant is in a confidence interval for $-2 \times \beta_2$. 



### Correlation is Not Causation: Spurious Correlation

  ## Monte Carlo to show how data dredging can result in finding hig hcorrelations among
  ## theoretically uncorrelated variables
N <- 25
G <- 1000000
sim_data <- tibble(group = rep(1:G, each = N), X = rnorm(N*G), Y = rnorm(N*G))

  ## Compute correaltion between x and y for each group and look for the maximum
res <- sim_data %>%
  group_by(group) %>%
  summarize(r= cor(X, Y)) %>%
  arrange(desc(r))

res

  ## Plot data for thisparticular group
sim_data %>% filter(group == res$group[which.max(res$r)]) %>%
  ggplot(aes(X, Y)) +
  geom_point() +
  geom_smooth(method = "lm")

    ## Correlations number is a random variable
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth =0.1 , color ="black")

    ## shows significant p-value = p-hacking
sim_data %>%
  filter(group == res$group[which.max(res$r)]) %>%
  do(tidy(lm(Y ~ X, data = .)))


### Correlation is Not Causation: Outliers
  ## Simulate outliers
set.seed(1)
x <- rnorm(100,100, 1)
y <- rnorm(100, 84, 1)
x[-23] <- scale(x[-23])
y[-23] <- scale(y[-23])

tibble(x,y) %>% ggplot(aes(x,y)) + geom_point(alpha = 0.5)

cor(x,y)

cor(x[-23], y[-23])

    ## One solution is the spearmann correlation, correlation of ranks
cor(rank(x), rank(y))
cor(x,y, method ="spearman")


###Correlation is Not Causation: Reversing Cause and Effect

library(HistData)
data("GaltonFamilies")
GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) %>%
  do(tidy(lm(father ~ son, data = .)))


###Correlation is Not Causation: Confounder
data("admissions")
admissions

admissions %>% group_by(gender) %>%
  summarize(percentage = 
              round(sum(admitted*applicants)/sum(applicants), 1))

admissions %>% group_by(gender) %>%
  summarize(total_admitted = round(sum(admitted/100*applicants)),
            not_admitted = sum(applicants) - sum(total_admitted)) %>%
  select(-gender) %>%
  do(tidy(chisq.test(.)))

admissions %>% select(major, gender, admitted) %>%
  spread(gender, admitted) %>%
  mutate(women_minus_men = women - men)

admissions %>%
  group_by(major) %>%
  summarize(major_selectivity = sum(admitted*applicants)/sum(applicants),
            percent_women_applicants = sum(applicants*(gender == "women")/sum(applicants))*100) %>%
  ggplot(aes(major_selectivity, percent_women_applicants, label = major)) +
  geom_text()

admissions %>%
  mutate(percent_admitted = admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender, y = percent_admitted, fill = major)) +
  geom_bar(stat = "identity", position = "stack")

admissions %>%
  ggplot(aes(major, admitted, col =gender, size = applicants)) +
  geom_point()

admissions %>% group_by(gender) %>% summarize(average = mean(admitted))
