library(devtools)

#install_github("sebastian-engelke/graphicalExtremes")

document() # generate documentations

build_manual() #generate manual

setwd("..")
install("ExtremesBinaryClassifier") #install package
library(ExtremesBinaryClassifier)
