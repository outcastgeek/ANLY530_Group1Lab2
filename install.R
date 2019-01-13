
#r = getOption("repos")
#r["CRAN"] = "http://cran.us.r-project.org"
#options(repos = r)

# R Dependencies
install.packages("knitr")
install.packages("kableExtra")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ModelMetrics")
install.packages("generics")
install.packages("gower")
install.packages("caret", dependencies = c("Depends", "Suggests"))
install.packages("naivebayes")
