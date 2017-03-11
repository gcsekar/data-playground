# Load Facebook data
library(ggplot2)

# Graph Function

drawplot= function(data, param1, param2, colorvector, facetby ){
  
  g <- ggplot(data, aes_string(param1, param2, color=colorvector))
  g <- g + geom_point() + facet_grid(~ Type )
  g <- g + xlim(range(data[param1]))
  g <- g + geom_smooth(method="lm", formula = y ~ x)
  g
}


drawboxplot= function(data, param1, param2, colorvector, facetby ){
  
  g <- ggplot(data, aes_string(param1, param2, color=colorvector))
  g <- g + geom_boxplot() + facet_grid(~ Type )
  g <- g + ylim(range(1:2500))
  g
  
}