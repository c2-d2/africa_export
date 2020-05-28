devtools::load_all("~/Documents/Github/covback")
library(tidyverse)
library(patchwork)

## Some toy fake incidence
incidence <- rpois(100,dpois(1:100,50) * 1000)

incu_par1 <- 1.621
incu_par2 <- 0.418

confirm_par1 <- 2.72
confirm_par2 <- 2.7

## Incubation period
p1 <- ggplot(data.frame(x=seq(0,21,by=0.1),y=dlnorm(seq(0,21,by=0.1),incu_par1, incu_par2))) +
  geom_line(aes(x=x,y=y)) + ylab("Probability of symptom onset") + xlab("Days since infection") +
  ggtitle("Incubation period")
median_incu <- exp(incu_par1)
mean_incu <- exp(incu_par1 + incu_par2^2 /2)
mode_incu <- exp(incu_par1 - incu_par2^2)

## Confirmation delay
p2 <- ggplot(data.frame(x=seq(0,21,by=0.1),y=dgamma(seq(0,21,by=0.1),confirm_par1, scale=confirm_par2))) +
  geom_line(aes(x=x,y=y)) + ylab("Probability of confirmation") + xlab("Days since symptom onset") +
  ggtitle("Confirmation delay")
mean_confirm <- confirm_par1*confirm_par2

## Time varying confirm delay
mean_confirms1 <- c(rep(3.18 * (1/0.59), 50), rep(3.72 * (1/0.42),50))
confirm_shapes <- c(rep(3.18, 50), rep(3.72, 50))
confirm_scales <- c(rep(1/0.59, 50), rep(1/0.42, 50))

xs <- seq(0,21,by=0.1)
p2b <- ggplot(data.frame(x=rep(xs,2),
                         y=c(dgamma(xs,3.18, scale=(1/0.59)), dgamma(xs,3.72, scale=(1/0.42))),
                         period=c(rep("late",length(xs)), rep("early",length(xs))))) +
  geom_line(aes(x=x,y=y,col=period)) + ylab("Probability of confirmation") + xlab("Days since symptom onset") +
  ggtitle("Confirmation delay") +
  theme(legend.position=c(0.8,0.8))


## With full distributions
prev1 <- convert_prevalence(incidence, mean_incu,mean_confirms1,incu_par1,incu_par2, confirm_shapes, confirm_scales)

## With mean shifts
prev2 <- convert_prevalence(incidence, mean_incu,mean_confirms1)

plot_dat <- data.frame(x=rep(seq(0, length(incidence)-1, by=1),2), 
                       ver=rep(c("distributions","mean_shifts"),each=length(incidence)),
                       y=c(prev1, prev2))

p3 <- ggplot(plot_dat) +
  geom_line(aes(x=x,y=y,col=ver)) +
  xlab("Day") +
  ylab("Prevalence") +
  ggtitle("Comparison of mean shifts and full distributions") +
  geom_vline(xintercept=50,linetype="dashed") +
  theme(legend.position=c(0.1,0.8))
(p1 + p2b) / p3
png("~/Documents/compare_prevalence_estimates.png",width=6,height=5,res=300,units="in")
(p1 + p2b) / p3
dev.off()



