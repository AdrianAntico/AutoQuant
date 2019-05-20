# No Remotes ----
# Attachments ----
to_install <- c("h2o","caTools", "data.table", "doParallel", "foreach", "forecast", "ggplot2", "itertools", "lubridate", "monreg", "pROC", "RColorBrewer", "recommenderlab", "ROCR", "scatterplot3d", "stringr", "tm", "tsoutliers", "wordcloud", "zoo","catboost")
  for (i in to_install) {
    message(paste("looking for ", i))
    if (!requireNamespace(i)) {
      message(paste("     installing", i))
      if(i == "catboost") {
        devtools::install_github('catboost/catboost',
                                 subdir = 'catboost/R-package')
      } else if (i == "h2o") {
        if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
        Sys.sleep(3)
        if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }
        Sys.sleep(3)

        # Next, we download packages that H2O depends on.
        pkgs <- c("RCurl","jsonlite")
        Sys.sleep(3)
        for (pkg in pkgs) {
          if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg)}
        }

        # Now we download, install and initialize the H2O package for R.
        install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-yates/3/R")
      } else {
        install.packages(i)
      }
    }
  }


