# https://cran.r-project.org/web/packages/Gmisc/vignettes/Descriptives.html;
# For labeling we use the label() function from the Hmisc package
# install.packages("Hmisc")
# install.packages("Gmisc")

library(Hmisc)
library(Gmisc)

#Import data
winequality <- read.csv("../data/winequality-all.csv")

#Drop quality variable
var.out <- !names(winequality) %in% c("quality")
winequality <- winequality[,var.out]

#Style all of the table output using htmlTable's theme handler
htmlTable::setHtmlTableTheme(css.rgroup = "")

winequality$qualityclass <- factor(winequality$qualityclass, levels=c("Low","Normal","High"),
                                   labels=c("Low (N=246)","Normal (N=6,053)","High (N=198)")) 
winequality$type <- factor(winequality$type, levels=c("red","white"),
                           labels=c("Red Wine","White Wine"))

label(winequality$qualityclass)         <- "Quality"
label(winequality$type)                 <- "Type"
label(winequality$fixed.acidity)        <- "Fixed Acidity"
units(winequality$fixed.acidity)        <- "(g tartaric acid/dm^3), median (IQR)"
label(winequality$volatile.acidity)     <- "Volatile Acidity"
units(winequality$volatile.acidity)     <- "(g acetic acid/dm^3), median (IQR)"
label(winequality$citric.acid)          <- "Citric Acid"
units(winequality$citric.acid)          <- "(g/dm^3), median (IQR)"
label(winequality$residual.sugar)       <- "Residual Sugar"
units(winequality$residual.sugar)       <- "(g/dm^3), median (IQR)"
label(winequality$chlorides)            <- "Chlorides"
units(winequality$chlorides)            <- "(g sodium chloride/dm^3), median (IQR)"
label(winequality$free.sulfur.dioxide)  <- "Free Sulfur Dioxide"
units(winequality$free.sulfur.dioxide)  <- "(mg/dm^3), median (IQR)"
label(winequality$total.sulfur.dioxide) <- "Total Sulfur Dioxide"
units(winequality$total.sulfur.dioxide) <- "(mg/dm^3), median (IQR)"
label(winequality$density)              <- "Density"
units(winequality$density)              <- "(g/cm^3), median (IQR)"
label(winequality$pH)                   <- "pH"
units(winequality$pH)                   <- "median (IQR)"
label(winequality$sulphates)            <- "Sulphates"
units(winequality$sulphates)            <- "(g potassium sulphate/dm^3), median (IQR)"
label(winequality$alcohol)              <- "Alcohol"
units(winequality$alcohol)              <- "(volume %), median (IQR)"

getTable1Stats <- function(x, digits = 0, ...){
  getDescriptionStatsBy(x = x, 
                        by = winequality$qualityclass,
                        digits = digits,
                        continuous_fn = describeMedian,
                        header_count = TRUE,
                        ...)
}

t1 <- list()
t1[["Type"]] <- getTable1Stats(winequality$type)
t1[["Fixed Acidity"]] <- getTable1Stats(winequality$fixed.acidity)
t1[["Volatile Acidity"]] <- getTable1Stats(winequality$volatile.acidity)
t1[["CitricAcidity"]] <- getTable1Stats(winequality$citric.acid)
t1[["Residual Sugar"]] <- getTable1Stats(winequality$residual.sugar)
t1[["Chlorides"]] <- getTable1Stats(winequality$chlorides)
t1[["Free Sulfur Dioxide"]] <- getTable1Stats(winequality$free.sulfur.dioxide)
t1[["Total Sulfur Dioxide"]] <- getTable1Stats(winequality$total.sulfur.dioxide)
t1[["Density"]] <- getTable1Stats(winequality$density)
t1[["pH"]] <- getTable1Stats(winequality$pH)
t1[["Sulphates"]] <- getTable1Stats(winequality$sulphates)
t1[["Alcohol"]] <- getTable1Stats(winequality$alcohol)

mergeDesc(t1,
          htmlTable_args = list(caption  = "Basic Descriptive Statistics from the Wine Quality Dataset"))

t1
