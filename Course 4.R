##### Inference and Modeling


### Sampling ModelParameters and Estimates
library(tidyverse)
library(dslabs)
ds_theme_set()
take_poll(25)
take_poll(25)
take_poll(25)
take_poll(25)

### The sample Average

### The Central Limit Threorem in Practice
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
se

pnorm(0.01/se) - pnorm(-0.1/se)

2*se

pnorm(2)-pnorm(-2)


### Margin of Error


### A Monte Carlo Simulation for the CLT

B <- 10000
N <- 1000
X_hat <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
  mean(X)
})    # Don't know p

p <- 0.45
N <- 1000
X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
X_hat <- mean(X)

B <- 10000
X_hat <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  mean(x)
})

mean(X_hat)
sd(X_hat)

library(tidyverse)
library(gridExtra)
p1 <- data.frame(X_hat=X_hat) %>% ggplot(aes(X_hat))+
  geom_histogram(binwidth=0.005, color="black")
p2 <- data.frame(X_hat=X_hat) %>% ggplot(aes(sample=X_hat)) +
  stat_qq(dparams= list(mean=mean(X_hat), sd=sd(X_hat)))+
  geom_abline()+
  ylab("X_hat")+
  xlab("Theoretical normal")
grid.arrange(p1, p2, nrow=1)


### The Spread

### Bias: Why Not Run a Very Large Poll?
N <- 10000
p <- seq(0.35, 0.65, length= 100)
SE <- sapply(p, function(x) 2*sqrt(x*(1-x)/N))
data.frame(p=p, SE=SE) %>% ggplot(aes(p,SE))+geom_line()


###Confidence Intervals
##A very commonly seen version from ggplot: geom_smooth()

data("nhtemp")
data.frame(year= as.numeric(time(nhtemp)), temperature=as.numeric(nhtemp)) %>%
  ggplot(aes(year,temperature)) +
  geom_point()+
  geom_smooth()+
  ggtitle("Average Yearly Temperatures in New Haven")


p <- 0.45
N <- 1000

  #If we redo the code twice, the intervals are different due to random variation
X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat +2*SE_hat)

X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p, p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat +2*SE_hat)

z<-qnorm(0.995)
z
pnorm(qnorm(0.995))
pnorm(1-qnorm(0.995))
pnorm(z)-pnorm(-z)
qnorm(0.975)

# A Monte  Carlo Simulation for Confidence Intervals
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size=N, replace=TRUE, prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat +2*SE_hat) #is p included in the interval?
})
mean(inside) # in 0.9513% of the time


### The Correct Language

###Power
  ##Sample size of 25, the confidence interval fot the spread was
N <- 25
X_hat <- 0.48
(2*X_hat -1) + c(-2,2)*2*sqrt(X_hat*(1-X_hat)/sqrt(N))

### p-values
N<- 100
z<- sqrt(N)*0.02/0.5
1-(pnorm(z)-pnorm(-z))


### Poll Aggregators
library(tidyverse)
d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace= TRUE, prob=c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat)-1
})

polls <-data.frame(poll=1:ncol(confidence_intervals), t(confidence_intervals), sample_size=Ns)
names(polls) <- c("poll", "estimate", "low", "high", "sample_size")
polls

sum(polls$sample_size)

d_hat <- polls %>% summarize(avg(estimate*sample_size) /sum(sample_size)) %>% .$avg

p_hat <- (1+d_hat)/2
moe <-2*1.96*sqrt(p_hat*(1-p_hat)/sum(poll$sample_size))
moe

round(d_hat*100,1)
round(moe*100,1)


### Pollsters and Multilevel Models


### Poll Data and Pollster Bias
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

polls <- polls_us_election_2016 %>% filter (state == "U.S." & enddate >= "2016-10-30" & grade %in% c("A+","A","A-","B+") | is.na(grade))

polls <- polls %>% mutate(spread= rawpoll_clinton/100 - rawpoll_trump/100)

d_hat <- polls %>% summarize(d_hat = sum(spread*samplesize)/sum(samplesize)) %>% .$d_hat

p_hat <- (d_hat+1)/2
moe <- 1.96 * 2 * sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))
moe

polls %>% ggplot(aes(spread)) + geom_histogram(color="black", binwidth= .01)

polls %>% group_by(pollster) %>% summarize(n())

polls %>% group_by(pollster) %>%
  filter(n() >=6) %>%
  ggplot(aes(pollster, spread))+
  geom_point()+
  theme(axis.text.x = element_text(angle= 90, hjust=1))

polls %>% group_by(pollster) %>%
  filter(n() >= 6) %>%
  summarize(se=2*sqrt(p_hat * (1-p_hat) /median(samplesize)))


### Data-Driven Models
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>% 
  ungroup()

one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)

sd(one_poll_per_pollster$spread)

results <- one_poll_per_pollster %>%
  summarize(avg= mean(spread), se= sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - 1.96*se, end=avg +1.96* se)
round(results*100,1)



### Bayesian Statistics

###Bayes Theorem
## Cystic fibrosis example
# prevelance of the disease is 1 in 35000
prev <- 0.00025
N <- 100000
outcome <- sample(c("Disease", "Healthy"), size= N, replace=TRUE, prob=c(prev, 1-prev))
N_D <- sum(outcome=="Disease")
N_D
N_H <- sum(outcome == "Healthy")
N_H

#False positives/negatives
accuracy <- 0.99 #accuracy test
test <- vector("character", N)
test[outcome=="Disease"] <- sample(c("+","-"), N_D, replace=TRUE, prob=c(accuracy, 1-accuracy))
test[outcome=="Healthy"] <- sample(c("-","+"), N_H, replace=TRUE, prob=c(accuracy, 1-accuracy))

table(outcome, test)
19/972 #2%


### Bayes in practice


### Hierarchical Model


### Election Forecasting
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B<- sigma^2/(sigma^2+tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/ (1/simga^2 + 1/tau^2))

posterior_mean
posterior_se

  #credibel interval (random interval that has a 95% chance of occuring)
posterior_mean + c(-1.96, 1.96) * posterior_se

  #probability d bigger than 0
1 - pnorm(0,posterior_mean,posterior_se)



### Mathematical representations of Models
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))

  # create data for 5 different pollsters using a sapply loop
I <- 5
J <- 6
N <- 2000
X <- sapply(1:I, function(i) {
  d+rnorm(J,0,2*sqrt(p*(1-p)/N))
})

  # dOES NOT TAKE INTO ACCOUNT DATA TO DATA VARIABILITY
I <- 5
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
h <- rnorm(I, 0,0.25)
X <- sapply(1:I, function(i) {
  d+h[i]+rnorm(J,0,2*sqrt(p*(1-p)/N))
})
 
  # add general bias variability
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B<- sigma^2/(sigma^2+tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1/ (1/simga^2 + 1/tau^2))

1 - pnorm(0,posterior_mean,posterior_se)


### Predicting the Electoral College

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)


  #??? Predict the electoral college result of 2016
  # Aggregate results from polls taken during last week
results <- polls_us_election_2016 %>%
  filter(state!= "U.S." &
           !grepl("CD", state) &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread= rawpoll_clinton/100 -rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n=n()) %>%
  mutate(state= as.character(state))

results %>% arrange(abs(avg))

  # Function left_join() let us easily add the number of electoral votes for each state
results <- left_join(results, results_us_election_2016, by= "state")
results_us_election_2016 %>% filter(!state %in% results$state)

  # assign sd to the states that had just one poll by substituting the missing value
  # by the median of the SD of all other states
results <- results %>%
  mutate(sd= ifelse(is.na(sd), median(results$sd, na.rm=TRUE), sd))

  # Bayesian calculation
mu <- 0
tau <- 0.02
results %>% mutate(sigma= sd/sqrt(n),
                   B= sigma^2/(sigma^2+tau^2),
                   posterior_mean= B*mu + (1-b)*avg,
                   posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

  # monte carlo repeat 10000 times
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma= sd/sqrt(n),
                   B= sigma^2/(sigma^2+tau^2),
                   posterior_mean= B*mu + (1-b)*avg,
                   posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2)),
                   simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                   clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>%
    summarize(clinton = sum(clinton)) %>%
    .$clinton + 7## 7 for Rhode Island and D.C.
})

mean(clinton_EV >269)# chance of Clinton winning

data.frame(clinton_EV) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth =1) +
  geom_vline(xintercept = 269)

  
  # Add general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2<- replicate(1000, {
  results %>% mutate(sigma= sqrt(sd^2/n + bias_sd^2),
                     B= sigma^2/(sigma^2+tau^2),
                     posterior_mean= B*mu + (1-b)*avg,
                     posterior_se = sqrt(1/(1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result>0, electoral_votes, 0)) %>%
    summarize(clinton = sum(clinton)) %>%
    .$clinton + 7## 7 for Rhode Island and D.C.
})

mean(clinton_EV >269)# chance of Clinton winning


### Forecasting
  # stick to one pollster to avoid between pollster varibility
one_pollster<- polls_us_election_2016 %>% 
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical= 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

  # distribution does not look normal as theory predicts
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color ="black")
 
  #extra variaility comes from time variation due to events
polls_us_election_2016 %>%
  filter(state =="U.S." & enddate >= "2016-07-01") %>%
  group_by (pollster) %>%
  filter(n() >=10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color=pollster), show.legend = FALSE, alpha = 0.6)

  # Our model must take into account a time effect
  # Blue line indicates a trend. Mostly they plot a trend for each candidate and not the difference
polls_us_election_2016 %>%
  filter(state =="U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster)%>%
  mutate(candidate= factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by (pollster) %>%
  filter(n() >=10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color= candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30,50))


### The t-Distribution

z <- qt(0.975, nrow(one_poll_per_pollster) -1)
one_poll_per_pollster %>%
  summarize(avg= mean(spread), moe= z*sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg - moe, end = avg + moe)

qt(0.975, 14) # quantile t-distribution with 14 df
qnorm(0.975)


### Association Tests
data("research_funding_rates")
research_funding_rates

  #compute difference between men and women by computing the total succesfull and total weren't
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

totals %>% summarize(percent_men = yes_men/(yes_men+no_men),
                     percent_women = yes_women/(yes_women+no_women))
  # We see larger amount of men rewarded
  
  # Data from milk pooring example is usually summarized like this
tab <- matrix(c(3,1,1,3), 2,2)
rownames(tab) <- c("Poured Before","Poured After")
colnames(tab) <- c("Guessed before","Guessed after")
tab

fisher.test(tab, alternative="greater") # p-value from Fisher's test


### Chi-Squared Tests
  # Compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total =
              (yes_men + yes_women)/
              (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total

funding_rate

  # Would we see as big a difference between the funding of men and women if funding was given at random?
  # Chi-squared test answers this
  # Firt create two-by-two table
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

  # compare this to what we would expect to see at the overall funding rate
tibble(awarded = c("no", "yes"),
       men= (totals$no_men + totals$yes_men) *
         c(1 - funding_rate, funding_rate),
       women= (totals$no_women + totals$yes_women)*
         c(1- funding_rate, funding_rate))

  # the chi.test() function takes a 2x2 table and returns the results of the test
two_by_two %>% 
  select(-awarded) %>%
  chisq.test()

  # odds ratio. Man X=1 and funded Y=1 otherwise woman X=0, non-funded Y=0
  # Odds to be funded
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1]/sum(two_by_two$men))

odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1]/sum(two_by_two$women))

odds_men/odds_women

  # odds_ratio not always correlated to p-value, depends on sample size
two_by_two %>% 
  select(-awarded) %>%
  mutate(men = men*10, women= women*10) %>%
  chisq.test()
