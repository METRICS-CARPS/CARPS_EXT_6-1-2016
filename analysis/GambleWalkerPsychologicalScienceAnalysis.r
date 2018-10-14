
# Bicycle helmet risk taking study
# Ian Walker

library(beanplot)
library(car)

# read data
d <- read.csv( "GambleWalkerPsychologicalScienceData.csv", sep = "\t" )

#####
# Main analyses
#####

screen.draw <- TRUE # set to TRUE to draw to screen rather than file
if (screen.draw) { 
  try(dev.off())
  dev.new( width = 10, height = 5 )
} else {
  png( "figure2.png", width = 3000, height = 1500, res = 300 )
}
par( mfrow = c(1, 3) )

# Balloon data
writeLines("\n=============================================================")
writeLines("== Balloon analysis                                        ==")
writeLines("=============================================================\n")
beanplot( d$BART ~ d$Condition, horizontal = TRUE, col = c("darkseagreen1", "olivedrab", "red", "black"), side = "second", xlab = "Score", ylab = "", main = "a. Risk taking", names = c("Helmet", "Cap"), las = 1)
print( var.test( BART ~ Condition, data = d ) ) #check comparable variance
tt.balloon <- t.test( BART ~ Condition, data = d, var.equal = TRUE ) # do the t-test
print( tt.balloon )
print( aggregate( d$BART, by = list( d$Condition ), FUN = sd ) )

# SSS data
writeLines("\n=============================================================")
writeLines("== SSS analysis                                            ==")
writeLines("=============================================================\n")
beanplot( d$SSS_total ~ d$Condition, horizontal = TRUE, col = c("darkseagreen1", "olivedrab", "red", "black"), side = "second", xlab = "Score", ylab = "", main = "b. Sensation seeking", names = c("Helmet", "Cap"), las = 1)
print( var.test( SSS_total ~ Condition, data = d ) ) #check comparable variance
tt.sss <- t.test( SSS_total ~ Condition, data = d, var.equal = FALSE ) # do the t-test
print( tt.sss )
print( aggregate( d$SSS_total, by = list(d$Condition), FUN = sd ) )

# prepare the data - STAI
relevant.columns <- c("Condition", "STAI_S_Y_PRE", "STAI_S_Y_DURING", "STAI_S_Y_POST")
aovdata <- d[relevant.columns]
scores <- as.matrix(aovdata[ ,2:4])
# run the analysis - Split-plot
writeLines("\n=============================================================")
writeLines("== STAI analysis                                           ==")
writeLines("=============================================================\n")
scores.long <- reshape( aovdata, direction = "long", varying = list(colnames(aovdata[,2:4])), timevar = "beforeafter", v.names = "score", times = 1:3 )
beanplot( scores.long$score ~ scores.long$beforeafter + scores.long$Condition, horizontal = TRUE, col = c("darkseagreen1", "olivedrab", "red", "black"), side = "second", xlab = "Score", ylab = "", main = "c. Anxiety", names = c("Helmet.T1", "Helmet.T2", "Helmet.T3", "Hat.T1", "Hat.T2", "Hat.T3"), las = 1, log = "" )
options( contrasts = c("contr.sum", "contr.poly") ) 
timepoints <- c("pre", "during", "post")
timeframe <- data.frame(timepoints)
model <- lm( sqrt(scores) ~ 1 + Condition, data = aovdata )
aovmodel.STAI <- Anova( model, idata = timeframe, idesign = ~timepoints, type = 3)
print(summary(aovmodel.STAI))


# Let's try an overall model to predict risk-taking
# Note that various reduced models (with interactions etc removed) were also tested for the analysis - these
# can easily be recreated by running lm() commands based on the one below and compared to this model
# with the anova() command
writeLines("\n=============================================================")
writeLines("== Regression model                                        ==")
writeLines("=============================================================\n")
sss <- scale(d$SSS_total)
age <- scale(d$Age)
d$Helmet_Use_Likelihood[d$Cycling_Frequency == 1] <- NA # because can't use data from non-riders
model <- lm(d$BART ~ d$Condition * scale(d$Helmet_Use_Likelihood) * scale(d$Cycling_Frequency * age * sss))
print(summary(model))

if (!screen.draw) { 
  dev.off()
}
par(mfrow = c(1,1))

