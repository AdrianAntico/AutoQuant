# No Remotes ----
# Attachments ----
to_install <- c("catboost", "caTools", "data.table", "doParallel", "foreach", "forecast", "ggplot2", "h2o", "itertools", "lubridate", "monreg", "pROC", "RColorBrewer", "recommenderlab", "ROCR", "scatterplot3d", "stringr", "tm", "tsoutliers", "wordcloud", "xgboost", "zoo")
for (i in to_install) {
  message(paste("looking for ", i))
  if(i == "catboost") {
    devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')
  } else if(i == "h2o") {
    if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
    if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
    pkgs <- c("RCurl","jsonlite")
    for (pkg in pkgs) {
      if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
    }
    install.packages("h2o", type="source", repos="https://h2o-release.s3.amazonaws.com/h2o/rel-yates/3/R")
  } else if (!requireNamespace(i)) {
    message(paste("     installing", i))
    install.packages(i)
  }
}
