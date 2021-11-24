cheese=data.frame(char=
                    c("White", "Yellow", "Gray", "Curd_size", "Size_uniformity",
                      "Shape_uniformity", "Liquid_solid_ratio", "Sour", "Sweet",
                      "Cheesy", "Rancid", "Cardboard", "Storage", "Breakdown_rate",
                      "Firm", "Sticky", "Slippery", "Heavy", "Particle_size",
                      "Runny", "Rubbery"),
                  p=c(.004, .002, .13, .29, .73, .08, .02, .4, .24, .01, .0001,
                      .0001, .001, .001, .0001, .41, .07, .15, .42, .002, .006))
cheese = cheese[order(cheese$p),] 
cheese
cheese$B = p.adjust(cheese$p, "bonferroni") 
cheese$H = p.adjust(cheese$p, "holm") 
cheese$BH = p.adjust(cheese$p, "BH")
x = cheese$p
y = cbind(cheese$B, cheese$H, cheese$BH) 
cheese

matplot(x, y, xlab = "original p−value", ylab="adjusted p−value", col=1:3) 
legend('bottomright' , legend = c("Bonferroni", "Holm", "BH"), col = 1:3, pch = 10) 
abline (0 , 1 , col = 1)
