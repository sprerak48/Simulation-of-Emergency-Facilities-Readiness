#Packages required
require('triangle')
require('psych')

#Random number generated for triangle distribution
n = 5000
sample <- rtriangle(n,20,300,80)
plot(density(sample))
sample
hist(sample, breaks=100, main="Triangle Distribution", xlab="x")
mean(sample)
var(sample)

# Problem 1:-Simulation for samples range 20 - 300
#Part 1:
i = 1
beth <- c()
tufts <- c()
mas <- c()
bos <- c()
bri <- c()

while(i <= 5000){
  beth <- c(beth,sample[i]*0.3)
  tufts <- c(tufts,sample[i]*0.15)
  mas <- c(mas,sample[i]*0.20)
  bos <- c(bos,sample[i]*0.25)
  bri <- c(bri,sample[i]*0.10)
  i = i + 1
}
be_hos <- round(mean(beth))
t_hos <- round(mean(tufts))
m_hos <- round(mean(mas))
b_hos <- round(mean(bos))
br_hos<- round(mean(bri))
print(c(be_hos,t_hos,m_hos,b_hos,br_hos))

#Part 2:
beth_time <- c()
tufts_time <- c()
mas_time <- c()
bos_time <- c()
bri_time <- c()

i=1
while(i<=5000){
  beth_time <- c(beth_time,beth[i]*7/60)
  tufts_time <- c(tufts_time,tufts[i]*10/60)
  mas_time <- c(mas_time,mas[i]*15/60)
  bos_time <- c(bos_time,bos[i]*15/60)
  bri_time <- c(bri_time,bri[i]*20/60)
  i = i + 1
}

b_t <- round(mean(beth_time),digits = 4)
t_t <- round(mean(tufts_time),digits = 4)
m_t <- round(mean(mas_time),digits = 4)
bo_t <- round(mean(bos_time),digits = 4)
br_t <- round(mean(bri_time),digits = 4)
print(c(b_t,t_t,m_t,bo_t,br_t))

#Part 3:
sample
beth
s <- cumsum(beth)
index <- c(1:5000)
cum_mean <- s/index
plot(index,cum_mean,type='l',main='Laws of Large Number - Beth Israel Hospital')
abline(h=be_hos, col = 'red') 

#Part 4:
plot(hist(beth_time))
n<- length(beth_time)
a<- mean(beth_time)
s<- sd(beth_time)
error <- qnorm((1+0.95)/2)*s/sqrt(n)
left <- a-error
right <- a+error
confidence_interval <- right-left
confidence_interval

h <- seq(min(beth_time),ceiling(max(beth_time)),by=0.7)
length(h)
h_cut <- data.frame(table(cut(beth_time,breaks = h)))
h_cut$relfreq <- h_cut$Freq / sum(h_cut$Freq)
plot(h_cut$relfreq,main='Histogram of Relative Frequency',type='l')
exp_beth <- pexp(h_cut$relfreq,rate=1/b_t)
plot(exp_beth,type='l',col='red',main='Exponenetial Distribution - Beth Israel Hospital')
chisq <- chisq.test(h_cut$Var1,exp_beth)
chisq

#Part 5:
i=1
tot_time <- c()
while(i<=5000){
  tot_time <- c(tot_time,((beth_time[i]+tufts_time[i]+mas_time[i]+bos_time[i]+bri_time[i])*60/mean(sample)))
  i = i + 1
}
tot_time_ans <- mean(tot_time)
tot_time_ans

#Problem 2 :- Simulation for normal distribution
generate_normal <- rnorm(5000,mean = 150, sd = 50)
generate_normal
plot(density(generate_normal))

#Part 1:
i = 1
beth_2<- c()
tufts_2 <- c()
mas_2 <- c()
bos_2 <- c()
bri_2 <- c()

while(i <= 5000){
  beth_2 <- c(beth_2,generate_normal[i]*0.3)
  tufts_2 <- c(tufts_2,generate_normal[i]*0.15)
  mas_2 <- c(mas_2,generate_normal[i]*0.20)
  bos_2 <- c(bos_2,generate_normal[i]*0.25)
  bri_2 <- c(bri_2,generate_normal[i]*0.10)
  i = i + 1
}

b <- round(mean(beth_2))
t <- round(mean(tufts_2))
m <- round(mean(mas_2))
bo <- round(mean(bos_2))
br <- round(mean(bri_2))
print(c(b,t,m,bo,br))

#Part 2:
Beth_time_2 <- c()
Tufts_time_2 <- c()
Mas_time_2 <- c()
Bos_time_2 <- c()
Bri_time_2 <- c()

i=1
while(i<=5000){
  Beth_time_2 <- c(Beth_time_2,((beth_2[i]*7)+2)/60)
  Tufts_time_2 <- c(Tufts_time_2,((tufts_2[i]*10)+4)/60)
  Mas_time_2 <- c(Mas_time_2,((mas_2[i]*15)+3)/60)
  Bos_time_2 <- c(Bos_time_2,((bos_2[i]*15)+5)/60)
  Bri_time_2 <- c(Bri_time_2,((bri_2[i]*20)+3)/60)
  i = i + 1
}


b_2 <- round(mean(Beth_time_2),digits = 4)
t_2 <- round(mean(Tufts_time_2),digits = 4)
m_2 <- round(mean(Mas_time_2),digits = 4)
bo_2 <- round(mean(Bos_time_2),digits = 4)
br_2 <- round(mean(Bri_time_2),digits = 4)
print(c(b_2,t_2,m_2,bo_2,br_2))

#Part 3:
generate_normal
beth_2
s <- cumsum(beth_2)
index <- c(1:5000)
cum_mean <- s/index
plot(index,cum_mean,type='l',main='Laws of Large Number - Beth Israel Hospital')
abline(h=b, col = 'red') 

#Part 4:
plot(hist(Beth_time_2))

n<- length(Beth_time_2)
a<- mean(Beth_time_2)
s<- sd(Beth_time_2)
error <- qnorm((1+0.95)/2)*s/sqrt(n)
left <- a-error
right <- a+error
confidence_interval <- right-left
confidence_interval

h <- seq(min(Beth_time_2),ceiling(max(Beth_time_2)),by=0.9)
length(h)
h_cut <- data.frame(table(cut(Beth_time_2,breaks = h)))
h_cut$relfreq <- h_cut$Freq / sum(h_cut$Freq)
plot(h_cut$relfreq,main='Histogram of Relative Frequency',type='l')
exp_beth <- pnorm(h_cut$relfreq,mean(h_cut$relfreq),sd(h_cut$relfreq))
plot(exp_beth,type='l',col='red',main='Normal Distribution - Beth Israel Hospital')
chisq <- chisq.test(h_cut$Var1,exp_beth)
chisq

#Part 5:
i=1
tot_time_2 <- c()
while(i<=5000){
  tot_time_2 <- c(tot_time_2,((Beth_time_2[i]+Tufts_time_2[i]+Mas_time_2[i]+Bos_time_2[i]+Bri_time_2[i])*60)/mean(generate_normal))
  i = i + 1
}
tot_time_2_ans <- mean(tot_time_2)
tot_time_2_ans
