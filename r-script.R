# Create a sample power forecast data with 50 ensembles for weekly total power generation of a wind power plant with 100 MWs of installed power and 40% capacity factor 
power = data.frame(power = rnorm(50,mean = 7*24*100*0.4, sd = 1000))
hist(power$power, breaks=12, main = "Histogram for Synthetic Wind Power Generation Forecast Ensembles",col="royalblue")  

dens = density(power$power)
str(dens)

filled = c()

# Regular Grids?
{for (i in 1:(length(dens$x)-1)) {
  
  filled[i] = diff(dens$x[i:(i+1)])
  
}
  
  a = c()
  for (i in 1:(length(filled)-1)) {
    a[i] = diff(filled[i:(i+1)]) <= 0.000001
  }
}
print(all(a))
rm(filled, a)

library(ggplot2)

# Density plot of the ggplot
(m = ggplot(power, aes(x = power)) + 
    geom_density() +
    theme_bw() +
    labs(x = "Power", y = "Density", title = "Density Plot"))

# Take the data of the plot.
p = ggplot_build(m)

# First, ggplot's densities with red thick line then r's base densities with black thinner line.
plot(p$data[[1]]$x,p$data[[1]]$y, type = "l", col="#FF000050",lwd = 5,
     main = "Comparison of the densities between ggplot and r-base", 
     xlab = "Power",
     ylab = "Densities")
lines(dens$x, dens$y,col="#000050",lwd =2 )

# Is the area under the curve is equal to 1?
sum(dens$y)*diff(dens$x[1:2])

# Extract the data from the output of the ggplot_build()
prob_frcst = p$data[[1]]

# A function to create probabilities for each bin of the density plot.
scaling = function(x) {
  
  (x)/(sum(x))
  
}

prob_frcst$cdf = cumsum(scaling(prob_frcst$density))

# A function that makes a number rounded towards up or down by a user specified number.
round_it = function(x, roundto, tow = "up") {
  if(tow == "up") {  #Yukar? Yuvarla
    x + (roundto - x %% roundto)
  } else {
    if(tow == "down") {  #Asagi Yuvarla
      x - (x %% roundto)
    }
  }
}

# ggplot function for making easy to visualize the CDFs
plot_cdf =  function(data, prob_1, prob_2, wpp_name) {
  
  minimum = round_it(min(data$x),250,"down")
  maximum = round_it(max(data$x),250,"up")
  
  ggplot(data, aes(x = x, y = cdf)) +
    geom_line() +
    scale_x_continuous(name = "Power", 
                       limits = c(minimum,maximum), 
                       breaks = seq(minimum,maximum,250)) +
    scale_y_continuous(name = "CDF probabilities (%)",
                       limits = c(0,1),
                       breaks = seq(0,1,0.1),
                       labels = scales::percent) +
    ggtitle(paste(wpp_name,"WPP, Weekly Wind Power Generation Forecasts, Theoretical CDF")) +
    theme_bw()+
    theme(text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 90)) +
    geom_hline(yintercept = prob_1, color = "tomato", linetype = "dotted", lwd = 1) +
    geom_hline(yintercept = prob_2, color = "tomato", linetype = "dotted", lwd = 1) +
    coord_flip()
  
}

plot_cdf(prob_frcst, prob_1 = 0.3, prob_2 = 0.7,wpp_name = "Any")

# A method for extracting the closest CDF to 30% and 70%.
(percent30_i = which.min(abs(prob_frcst$cdf-0.3)))
(percent70_i = which.min(abs(prob_frcst$cdf-0.7)))

percent_index_finder = function(data, prob) {
  
  which.min(abs(data-prob))
  
}

prob_extraction = data.frame(Probabilities = seq(0.1,0.9,0.1), Value = 0)

for (i in (1:nrow(prob_extraction))) {
  
  prob_extraction[i,2] = prob_frcst$x[percent_index_finder(prob_frcst$cdf,prob_extraction[i,1])]
  
}

prob_extraction

# Comparison plot for the eCDF and theoretical CDFs
ggplot(power, aes(power)) + 
  stat_ecdf(geom = "line", aes(color = "Empirical CDF")) +
  geom_line(data = prob_frcst, aes(x = x, y = cdf, color = "Theroretical CDF"), alpha = 0.9) +
  scale_x_continuous(name = "Power")+
  scale_y_continuous(name = "CDF (%)", labels = scales::percent, breaks = seq(0,1,0.1))+
  ggtitle("Comparison of Theoretical and Empirical CDFs") +
  scale_color_manual(name = "CDF Types", values = c("Empirical CDF" = "black","Theroretical CDF"="tomato")) +
  theme_bw()
