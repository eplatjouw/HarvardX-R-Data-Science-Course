#Script Functions Part of Course
a <- 1
b <- 1
c <- -1
a
b
c
print(a)
ls()
x
(-b+sqrt(b^2-4*a*c))/(2*a)
(-b-sqrt(b^2-4*a*c))/(2*a)
d <- (-b+sqrt(b^2-4*a*c))/(2*a)
e <- (-b-sqrt(b^2-4*a*c))/(2*a)
a<-2
d
log(8)
a<-1
log(a)
exp(1)
log(2.718)
log(exp(1))
exp(log(1))
help("log")
?log
args(log)
log(8,base=2)
log(8)
log(8,2)
log(base=2,x=8)
2^3
help("+")
data()
pi
Inf
solution_1<-d
solution_2<-e

# Data Types
# class determines type
class (1)
class (ls)
library(dslabs)
data("murders")
class(murders)
#str shows structure 
str(murders)
#head shows first six line
head(murders)

# The $ sign can access variables and is called an accessor
murders$population

#names shows names columns
names(murders)

#length shows numberr of entries of vectors
length(murders$population)

#quotes differentiate numbers with character strings


levels (murders$region)
murders$region


#Section 2
#Vectors

codes <- c(380,124,818)
country <- c("italy","canada","egypt")
codes
country
countrycodes <-c(italy=380,canada=124, egypt=818)
countrycodes
class(country)
class(countrycodes)
countrycodes <-c("italy"=380,"canada"=124, "egypt"=818)
names(codes)
names(country)
names(codes)<-country
codes

seq(1,10)
seq(1,10,2)
help(seq)
1:10
codes[2]
codes[c(1,3)]
codes[c(1,2)]
codes["canada"]
codes["egypt"]
codes[c("egypt","italy")]

#vector Coercion

x <- c(1, "canada", 3)
x
class(x) #it coerced everything into characters

x <- 1:5
x
y<- as.character(x)
y
z<-as.numeric(y)
z

x <- c(1, "canada", 3)
as.numeric(x)
x


#Sort
  #function sort sorts the data in increasing order

library (dslabs)
data(murders)
sort(murders$total)

  #function order returns the indices that sort the vector parameter
w<-c(31,4,15,92,65)
w
sort(w)
index <-order(w)
index
index<-order(murders$total)
murders$abb[index]

max(murders$total)
i_max <- which.max(murders$total)
i_max
murders$state[i_max]

min(murders$total)
i_min <- which.min(murders$total)
i_min
murders$state[i_min]

w
  #rank gives you for any given list a vector with the rank of the first entry, second entry, third entry,...
rank(w)


help(data.frame)


#data wrangling

install.packages("dplyr")
library(dplyr)
library(dslabs)
murders <-mutate(murders, rate=total/population*100000)
murders
head(murders)

filter(murders, rate <=0.71)

new_table <- select(murders,state,region,rate)
filter(new_table, rate <=0.71)

murders %>% select(state,region,rate) %>% filter(rate<=0.72)

# making data frames

grades <- data.frame (names=c("John","Juan","Jean","Yao"),
                      exam_1=c(95,80,90,85),
                      exam_2=c(90,85,85,90))
grades
class(grades$names)

grades <- data.frame (names=c("John","Juan","Jean","Yao"),
                      exam_1=c(95,80,90,85),
                      exam_2=c(90,85,85,90),
                      stringsAsFactors = FALSE)
class(grades$names)
help(mutate)

#Data Visualization
population_in_millions <-murders$population/10^6
total_gun_murders <-murders$total
plot(population_in_millions, total_gun_murders)
hist(murders$rate)
murders$state[which.max(murders$rate)]
boxplot(rate~region, data=murders)


#Basic Conditionals

a<-0
if (a!=0) {
  print(1/a)
}else{
  print("No reciprocal for 0.")
}


library(dslabs)
data(murders)
murder_rate <-murders$total/murders$population*100000

ind <- which.min(murder_rate)
if(murder_rate[ind]<0.75){
  print(murders$state[ind])
}else{
  print("No state has murder rate that low")
}


a <- 0
ifelse(a>0, 1/a, NA)


a <- c(0,1,2,-4,5)
ifelse(a>0, 1/a, NA)

data(na_example)
sum(is.na(na_example))
no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))


z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)
z<- c(FALSE,FALSE,FALSE)
any(z)
all(z)
z <- c(TRUE, TRUE, TRUE)
any(z)
all(z)


# Define a new function
avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
z<-c(3,5,9,8,2)
avg(z)
mean(x)
identical(avg(x),mean(x))

avg <- function(x, arithmetic=TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

#For loops

compute_s_n <- function(n) {
  x <- 1:n
  sum(x)
}
compute_s_n(25)

for(i in 1:5){
  print(i)
}
i


m<-25
  #create an empty vector
s_n <- vector(length = m)
for (n in 1:m){
  s_n[n] <- compute_s_n(n)
}

n <- 1:m
plot(n, s_n)

n
s_n
plot(n, s_n)
lines(n,n*(n+1)/2)

#other functions
apply()
sapply()
tapply()
mapply()
split()
cut()
quantile()
reduce()
identical()
unique()



# Create function called `sum_n`
sum_n <- function(n) {
  x <- 1:n
  sum(x)
}
# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)


compute_s_n <- function(n){
  x <- 1:n
  sum(n^2)
}
compute_s_n(10)
