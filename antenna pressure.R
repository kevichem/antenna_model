library(tidyverse)
##### Prelims #####
  rA <- 2
  rH <- 1
  rW <- 25
  w <- sqrt(rW*(1/rA + 1/rH))
  pA <- 2
  pH <- 1
  u <- pA - pH
  l <- 1
  e <- exp(1)
  
  a <- u/(1-e^(2*w*l))


  eqn = function(x,w){u/(1-e^(2*w*l))*(e^(w*x) - e^(w*(2*l-x)))}
  
  n <- 2*(rA*rH)/(rH^2-rA^2)
  m <- 1/2*(pA + pH + (rA-rH)/(rA+rH)*u)

  fun.1 <- function(x)eqn(x,.1)
  fun.2 <- function(x)eqn(x,3)
  fun.3 <- function(x)eqn(x,5)
  PA <- function(x){rH/(rA+rH)*fun.3(x) + n*x + m}
  PH <- function(x){-rA/(rA+rH)*fun.3(x) + n*x + m}

    #####
  
curve(eqn(x,5), from=0, to=l, n=200, xlab="x", ylab="P_A - P_H", col="blue",lwd=2, 
      main="Plot of  Y")


ggplot(data = data.frame(x = 0), mapping = aes(x = x)) + 
  geom_point(mapping = aes(x = displ, y = hwy, color = class))

p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))


##### Attempts #####
p +
  layer(position = 'identity',
        geom = "path", 
        stat = "function",
        params = list(fun = log),
        mapping = aes(color = "fun.1"))
+
  layer(geom = "path", 
        stat = "function",
        params = list(fun = fun.2),
        mapping = aes(color = "fun.2")
  ) +
  layer(geom = "path", 
        stat = "function",
        params = list(fun = fun.3),
        mapping = aes(color = "fun.3")
  ) +
  scale_x_continuous(limits = c(-5, 5)) +
  scale_color_manual(name = "Functions",
                     values = c("blue", "red", "green"), # Color specification
                     labels = c("X^2 + X", "-X + 10", "3X + 2"))


p <- ggplot(data = data.frame(x = 0), mapping = aes(x = x))
fun.1 <- function(x) x^2 + x
p + stat_function(fun = fun.1) + xlim(-5,5)



curve(fun.3(x), from=0, to=l, n=200, xlab="x", ylab="P_A - P_H", col= "orange",lwd=2, 
      main="Plot of  Y = sin(X)+cos(X) ", add = F)

ggplot(df,aes(x)) +
  stat_function(fun=function(x) fun.1(x)) +
  stat_function(fun=function(x) 2*log(x))

##### Graph pA-pH #####
fun1 <- function(x)fun.1(x)
fun2 <- function(x)fun.2(x)
fun3 <- function(x)fun.3(x)
  data_fun <- data.frame(x = seq(0,1,.001),            # Create data for ggplot2
                         values = c(fun1(seq(0,1,.001)),
                                    fun2(seq(0,1,.001)),
                                    fun3(seq(0,1,.001))),
                         fun = rep(c("U = .1", "U = 3", "U = 5"), each = 1001))
  
  ggplot(data_fun,                                   # Draw ggplot2 plot
         aes(x, values, col = fun)) +
    geom_line() + 
    theme_classic()
  )


##### Graph pA and pH #####
  data_fun <- data.frame(x = seq(0,1,.001),            # Create data for ggplot2
                         values = c(PA(seq(0,1,.001)),
                                    PH(seq(0,1,.001)),
                                    fun.3(seq(0,1,.001))),
                         Parameter = rep(c("PA", "PH", "U = 5"), each = 1001))
  
  ggplot(data_fun,                                   # Draw ggplot2 plot
         aes(x, values)) +
    geom_line() + 
    theme_classic()
