##### Probability

#### Discrete Probability

#### Monte Carlo Simulations
  ##sample() function simulates a random number generator
  ##rep() function generates the urn with two reds and 3 blue beads

beads <- rep(c("red","blue"), times =c(2,3))
beads

sample(beads, 1)

  # A Monte Carlo simulation is a repetition of an experiment in a large enough 
  # amount of times to make the results similar enough to an infinite number of repetitions

  ##replicate() function permits to repeat the task any number of times we want

b <- 10000
events <- replicate(b, sample(beads, 1))

tab <- table(events)
tab

prop.table(tab)

  ## !! The sample() function lets you take more than one sample but does not work with replacement!
  ## !! It does not place the picked up bead in the urn

sample(beads, 5)
sample(beads, 5)
sample(beads, 5)
sample(beads, 6)

  #Sample with replacement
events <- sample(beads, b, replace = TRUE)
tab <- table(events)
tab

prop.table(tab)

sample(beads, 50, replace = TRUE)


### Probability Distributions

### Combinations and Permutations
##Build a Deck of cards using the functions expand.grid() and paste()
  # paste() is used to create strings by joining smaller strings 
number <- "Three"
suit <- "Hearts"
paste(number, suit)

  #also works on paors of vectors
paste(letters[1:5], as.character(1:5))

  # the function expand.grid() gives us all the combinations of 2 lists
expand.grid(pants=c("blue", "black"), shirt = c("white","grey", "plaid"))

  #Generation of a deck of cards
suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten"," Jack", "Queen", "King")
deck <- expand.grid(number=numbers, suit=suits)
deck <- paste(deck$number, deck$suit)
deck

  # Simple probabilities

kings <- paste ("King", suits)
mean(deck %in% kings)
1/13

  #gtools package
  # The permutation() function computes for any list of size n all the different ways we can select R items
install.packages("gtools")
library(gtools)
permutations(5,2)
  
all_phone_numbers <-permutations(10, 7, v=0:9)
n <- nrow(all_phone_numbers)
index <- sample(n, 5)
all_phone_numbers[index, ]

  
  #Compute all possible ways that we can choose 2 cards when the order matters
hands <- permutations(52, 2, v=deck)
first_card <- hands[,1]
second_card <- hands[,2]
   # How many cases have King as a first card
sum(first_card%in% kings)
  #Find conditional probability of what fraction of these 204 also have a king as their second card
sum(first_card %in% kings & second_card %in% kings)/sum(first_card %in% kings)
  

  #If the order doesn't matter we use combinations() function
combinations(3,2)

  #Probability of a natural blackjack
aces <- paste("Ace", suits)

facecard<-c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit=suits)
facecard <- paste(facecard$number, facecard$suit)

hands <-combinations(52, 2, v=deck)
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

  #We considered ace was the first card but to be safe we can use this code
mean((hands[,1] %in% aces & hands[,2] %in% facecard) | (hands[,2] %in% aces & hands[,1] %in% facecard))

  #We can also use the Monte Carlo to estimate this probability
hand <- sample(deck, 2)
hand
B <- 10000
results <- replicate(B, {
  hand <- sample(deck,2)
  (hand[1] %in% aces & hand [2] %in% facecard) |
    (hand[2]%in% aces & hand [1] %in% facecard)
  })
mean(results)


### The Birthday problem
n <- 50
bdays <- sample(1:365, n, replace =TRUE)

  #Probability of two people in this sample having the same birthday
  # function duplicated() returns TRUE if an element in that vector already appeared in that vector

duplicated (c(1,2,3,1,4,3,5))

any(duplicated(bdays))

B <- 10000
results <- replicate (B, {
  sample(1:365, n, replace =TRUE)
  any(duplicated(bdays))
})

mean(results)


### sapply
compute_prob <- function(n, B=10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

n<- seq(1,60)

  #sidenote: arithmetic operations operate on vectors in an element wise fashion
x <- 1:10
sqrt(x)

y <- 1:10
x*y

  # not all functions work this way
compute_prob(n)

  #use sapply() function instead. sapply() permits us to perfomr element-wise operations on any function
x<- 1:10
sapply(x, sqrt)

prob <- sapply(n, compute_prob)

plot(n, prob)

  # Now compute the exact probabilitis instead of Monte Carlo simulations
  #Write an exact function 
exact_prob <- function(n){
  prob_unique <- seq(365,365-n+1)/365
  1- prod( prob_unique)
}

eprob <- sapply(n, exact_prob)

plot(n, prob)
lines(n, eprob, col="red")

### How many Monte Carlo experiments are enough?
 ## Complex to answer
  #One approach is to check the stability of the estimate
  #Example with the birthday problem

B<- 10^seq(1, 5, len= 100)
compute_prob <- function(B, n=22){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)
plot(log10(B), prob, type ="l")


### addition rule


### Monty Hall problem
  ## Simulation of sticking to the same door using Monte Carlo
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample (c("car","goat","goat"))
  prize_door <- doors[prize=="car"]
  my_pick <- sample(doors,1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)

  ## repeat this but switch you pick of door
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample (c("car","goat","goat"))
  prize_door <- doors[prize=="car"]
  my_pick <- sample(doors,1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)],1)
  stick <- my_pick
  switch <- doors[!doors %in% c(my_pick, show)]
  switch == prize_door
})
mean(stick)


    # Chance cavs win the series (4wins) if they lost the first game

n <- 6
l <- list(c(1,0))
possibilities <-expand.grid(rep(sample(l),n))
results <- rowSums(possibilities) >= 4
mean(results)




### Continuos probability
library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% .$height

F <- function(a) mean(x<=a)

  # Probability that a student picked at rand is taller than 70'. This is equal to the proportion of students taller than 70'
1-F(70)


### Theoretical distribution
## Normal distribution
# F(a) = pnorm(a,avg,s)
  #Same example as above
1-pnorm(70.5, mean(x), sd(x))

  #Each specific height in a discrete category
plot(prop.table(table(x)), xlab= "a = Height in inches", ylab= "Pr(X=a)")

  # using actual data
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)

  #Using approximation
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))

  #Not usefull for those that do not include an integer
mean(x <= 68.9) - mean(x <= 68.1)

pnorm(68.9, mean(x), sd(x)) - pnorm(68.1, mean(x), sd(x))
  # This is called discretization: true heigth is continuous but reported heights is more discrete due to reporting and rounding up.


###Probability Density
avg <- mean(x)
s <- sd(x)
1-pnorm(76,avg,s)

  #dnorm() is the probability density function for the normal distribution

### Monte Carlo Simulations

  #rnorm() is a function that generates normally distributed outcomes
  # 3 arguments: size; average (standard: 1); standard deviation (stadard 0)

x <- heights %>% filter(sex=="Male") %>% .$height
n <-length(x)
avg <- mean(x)
s <- sd(x)
simuated_heights <- rnorm(n,avg,s)
ds_theme_set()
data.frame(simuated_heights=simuated_heights) %>% ggplot(aes(simuated_heights))+
  geom_histogram(color=" black", binwidth=2)

B <- 10000
tallest <- replicate(B, {
  simulated_data <- rnorm(800, avg ,s)
  max(simulated_data)
})
mean(tallest >= 7*12)

### Other distributions
x<- seq(-4,4, length.out=100)
data.frame(x,f=dnorm(x)) %>% ggplot(aes(x,f)) + geom_line()


### Random Variables

beads <- rep(c("red","blue"), times =c(2,3))
X <- ifelse(sample(beads,1) == "blue", 1,0)
ifelse(sample(beads,1) == "blue", 1,0)
ifelse(sample(beads,1) == "blue", 1,0)
ifelse(sample(beads,1) == "blue", 1,0)
ifelse(sample(beads,1) == "blue", 1,0)
ifelse(sample(beads,1) == "blue", 1,0)

## Statistical inference

### Sampling models
## Roulette
color <- rep(c("Black","Red", "Green"), c(18,18,2))

#???1000 independant draws, for each red, casino looses 1$ otherwise wins 1

n<- 1000
X<-sample(ifelse(color=="Red", -1, 1), n, replace=TRUE)
X[1:10]

# can be generated in one line of code
X<- sample(c(-1,1), n, replace =TRUE, prob=c(9/19, 10/19)) #=sampling model
S <- sum(X)
S  #random variable

  #Do we loose money? Through the random variable distribution function
  # Use monte carlo simulation
n <-1000
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1,1), n, replace =TRUE, prob=c(9/19, 10/19))
  sum(X)
})

mean(S<0)

mean(S)
sd(S)

s<- seq(min(S), max(S), length =100)
normal_density <- data.frame(s=s, f=dnorm(s, mean(S), sd(S)))
data.frame(S=S) %>% ggplot(aes(S, ..density..))+
  geom_histogram(color="black", binwidth = 1) +
  ylab("Probability")+
  geom_line(data = normal_density, mapping = aes(s,f), color= "blue")


### Distributios versu Probability distributions

avf <- sum(x)/length(x)
s <- sqrt(sum((x - avg)^2)/length(x))

### Notation for Random Variables
## Capital letters for random variables
## Lower case letter are observed values for observed values

###Central Limit Theorem
##When the number of independant draws is large, the sum of the independant draws is normal
B <- 10^6
X <- sample(c(-1,1), B, replace= TRUE, prob=c(9/19, 10/19))
mean(X)

#sd of roulette
2*sqrt(90)/19

# If a thousand people bet on Red in the roulette the asino is expected to win on average 50$ with a standard error of 32$
n <- 1000
sqrt(n) * 2 * sqrt(90/19)

#Using the CLT
n * (20-18)/28
sqrt(n) * 2 * sqrt(90)/19

mean(S)
sd(S)

mu <- n * (20-18)/28
se <- sqrt(n) * 2 * sqrt(90)/19
pnorm(0, mu, se)

mean(S<0)



### Averages and proportions

###La of Large Numbers

### How Large is Large in CLT


### The Big Short: Interest Rates Explained

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

B <- 10000
losses <- replicate(B, {
  defaults <- sample(c(0,1), n, prob=c(1-p, p), replace = TRUE)
  sum(defaults * loss_per_foreclosure)
})

data.frame(losses_in_millions = losses/10^6) %>% ggplot(aes(losses_in_millions)) + geom_histogram(binwidth=0.6, color="black")


n*(p*loss_per_foreclosure +  (1-p)*0)
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))

#produce an interest rate so that we break even: lp+ x(1-p)=0
#l = loss per foreclosure

-loss_per_foreclosure*p/(1-p)

#Still 50% chance bank losses money
#interst rate that makes it unlikely that that happens but not too high so they don't loose customers

qnorm(0.01)

l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*(n*p - z*sqrt(n*p*(1-p))) / (n*(1-p) + z*sqrt(n*p*(1-p)))
x

loss_per_foreclosure*p + x*(1-p)

B<-10000
profit <- replicate(B, {
  draws <- sample(c(x, loss_per_foreclosure), n, prob=c(1-p,p), replace=TRUE)
  sum(draws)
})

mean(profit)
mean(profit <0)


### The Big Short (something wrong in the numbers, doesn't match video)
p<-0.04
r <- 0.05
x<- r*180000
loss_per_foreclosure*p + x*(1-p)

z <- qnorm(0.01)
n <- ceiling((z^2*(x-1)^2*p*(1-p))/(l*p + x*(1-p))^2)
n
n*(loss_per_foreclosure*p + x*(1-p)) #??? expected to earn

#confirm with monte carlo

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, prob=c(1-p, p), replace = TRUE)
  sum(draws)
})

mean(profit <0)

# lets say something happens to every one of the higher risk morgage takers that displaces the percentage of each one by +/- 0.01 => 0.03 - 0.05.
#This event is no longer independant 

p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, prob=c(1-p, p), replace = TRUE)
  sum(draws)
})

mean(profit) # expected profit
mean(profit <0)
mean(profit <- -1000000)
