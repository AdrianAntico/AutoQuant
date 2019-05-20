# No Remotes ----
# Attachments ----
to_install <- c("catboost", "caTools", "data.table", "doParallel", "foreach", "forecast", "ggplot2", "h2o", "itertools", "lubridate", "monreg", "pROC", "RColorBrewer", "recommenderlab", "ROCR", "scatterplot3d", "stringr", "tm", "tsoutliers", "wordcloud", "xgboost", "zoo")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      install.packages(i)
    }
  }
