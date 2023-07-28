library(tidyverse)
path = "C:/temp/antenna_model/"
setwd(path)

#####
ls <- c(rA = 1, rH = 3, rW = 3,pA = 3, pH = 1, l = 1)

rA <- 2
rH <- 1
rW <- 25

pA <- 2
pH <- 1
u <- pA - pH
l <- 1
e <- exp(1)

w <- sqrt(rW*(1/rA + 1/rH))
a <- u/(1-e^(2*w*l))

tow <- function(rA,rH,rW,pA,pH,l){sqrt(rW*(1/rA + 1/rH))}

param <- tibble(rA,rH,rW,pA,pH,l)
params <- param %>% add_row(!!! ls)

params <- params %>% mutate(w = tow(rA,rH,rW,pA,pH,l))

eqn = function(x,w){u/(1-e^(2*w*l))*(e^(w*x) - e^(w*(2*l-x)))}

#####


# n <- 2*(rA*rH)/(rH^2-rA^2)
# n <- w*(1+exp(w*l))/(rH-rA)*a
# m <- 1/2*(pA + pH + (rA-rH)/(rA+rH)*u)
# m1 <- 2*rA*a*(1-exp(2*w*l))

n <- 0
m <- (rA*pA + rH*pH)/(rA + rH)

p_low <- function(x)eqn(x,.1)
p_mid <- function(x)eqn(x,3)
p_hi <- function(x)eqn(x,5)

p_diff <- function(x)p_hi(x)

p_A <- function(x){rH/(rA+rH)*p_diff(x) + n*x + m}
p_H <- function(x){-rA/(rA+rH)*p_diff(x) + n*x + m}


##### Corrections #####




curve(p_A, from = 0, to = 1, col = 2)  # Draw Base R plot
curve(p_H, from = - 0, to = 1, col = 3, add = TRUE)
curve(p_diff, from = - 0, to = 1, col = 4, add = TRUE)

#plot distribution
data_dist <- data.frame(x = seq(0,1,.0001),            # Create data for ggplot2
                       values = c(p_A(seq(0,1,.0001)),
                                  p_H(seq(0,1,.0001)),
                                  p_diff(seq(0,1,.0001))),
                       fun = rep(c("pA", "pH", "w"), each = 10001))

ggplot(data_dist,                                   # Draw ggplot2 plot
       aes(x, values, col = fun)) +
  geom_line(size = 1) +
  theme_classic() +
  scale_color_manual(name="Pressure",
                     labels=c("Difference","Antenna Vessel","Hemocoel"),
                     breaks = c("w", "pA", "pH"),
                     values=c("blue","red","green")) +
  labs(title = "Pressure Distribution Across \n Permeable Antenna Model", y = "Normalized Pressure", x = "Normalized Distance")

ggsave(filename = "p_dist.png", path = "C:/temp/antenna_model/")

# plot comparison
data_dist <- data.frame(x = seq(0,1,.0001),            # Create data for ggplot2
                        values = c(p_low(seq(0,1,.0001)), # or eqn(seq(0,1,.0001),1)
                                   p_mid(seq(0,1,.0001)),
                                   p_hi(seq(0,1,.0001))),
                        fun = rep(c("w = .1", "w = 3", "w = 5"), each = 10001))

ggplot(data_dist,                                   # Draw ggplot2 plot
       aes(x, values, col = fun)) +
  geom_line() +
  theme_classic() +
  labs(title = "Pressure Difference Along Antenna Model", y = "Pressure Difference", col = "values of omega")

ggsave(filename = "p_comp.png")


data_dist <- data.frame(x = seq(0,1,.0001),            # Create data for ggplot2
                        values = c(p_low(seq(0,1,.0001)), # or eqn(seq(0,1,.0001),1)
                                   p_mid(seq(0,1,.0001)),
                                   p_hi(seq(0,1,.0001))),
                        fun = rep(c("w = .1", "w = 3", "w = 5"), each = 10001))

