### Experimental and field data set on different serological assays for FeLV
# load the pROC library
library(pROC)

# Loading the data
# --> put dataset in the same folder as the script.
ex <- read.csv("ROC_exp.csv", header=T, sep = ";")
fd <- read.csv("ROC_field.csv", header=T, sep = ";")

### Experimental data
# Plotting p15E
plot.roc(ex$Provirus, ex$p15E, 
         main = "Experimental data",
         col="#1c61b6",
         percent=TRUE)

# Plotting the FL74 and EPK211 and adding them to the plot
lines.roc(ex$Provirus, ex$FL74, 
          percent=TRUE, col="#008600")
lines.roc(ex$Provirus, ex$EPK211, 
          percent=TRUE, col="darkviolet")

# Calculating the AUC (area under the curve)
auc.p15E <- round(auc(roc(ex$Provirus, ex$p15E)),3)
auc.FL74 <- round(auc(roc(ex$Provirus, ex$FL74)),3)
auc.EPK211 <- round(auc(roc(ex$Provirus, ex$EPK211)),3)

# Adding a legend (with rounded AUC for each serological assay)
legend("bottomright", 
       title = "AUC",
       legend=c(paste("p15E: ",auc.p15E), 
                paste("FL74: ", auc.FL74),
                paste("EPK211: ", auc.FL74)),
       col=c("#1c61b6", "#008600", "darkviolet"), lwd=2)

### Field data
# (same as above but with the fd dataset)

# Plotting p15E
plot.roc(fd$Provirus, fd$p15E, 
         main = "Field data",
         col="#1c61b6",
         percent=TRUE)

# Plotting the FL74 and EPK211
lines.roc(fd$Provirus, fd$FL74, 
          percent=TRUE, col="#008600")
lines.roc(fd$Provirus, fd$EPK211, 
          percent=TRUE, col="darkviolet")

# Calculating the AUC
auc.p15E <- round(auc(roc(fd$Provirus, fd$p15E)),3)
auc.FL74 <- round(auc(roc(fd$Provirus, fd$FL74)),3)
auc.EPK211 <- round(auc(roc(fd$Provirus, fd$EPK211)),3)

# Adding a legend
legend("bottomright", 
       title = "AUC",
       legend=c(paste("p15E: ",auc.p15E), 
                paste("FL74: ", auc.FL74),
                paste("EPK211: ", auc.FL74)),
       col=c("#1c61b6", "#008600", "darkviolet"), lwd=2)


### Non-paired ROC test 
# Is there a significant difference between the ROC curve of the p15E
# from the experimental data vs. the field data?
roc.test(roc(ex$Provirus, ex$p15E), # roc object for experimental data
         roc(fd$Provirus, fd$p15E), # roc object for field data
         paired=FALSE)
