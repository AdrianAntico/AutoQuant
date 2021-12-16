FROM rocker/shiny:4.0.5

# Get linux up to date
RUN apt-get update && apt-get install -y \
    --no-install-recommends \
    git-core \
    libssl-dev \
    libcurl4-gnutls-dev \
    curl \
    libsodium-dev \
    libxml2-dev \
    libicu-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

ENV _R_SHLIB_STRIP_=true
RUN echo 'Its free real estate'
RUN install2.r --error --skipinstalled \
    shiny \
    forecast \
    jsonlite \
    ggplot2 \
    htmltools \
    plotly \
    devtools \
    arules \
    bit64 \
    combinat \
    data.table \
    doParallel \
    e1071 \
    fBasics \
    foreach \
    forecast \
    fpp \
    ggplot2 \
    gridExtra \
    itertools \
    lubridate \
    MLmetrics \
    nortest \
    RColorBrewer \
    recommenderlab \
    pROC \
    Rfast \
    scatterplot3d \
    stringr \
    timeDate \
    tsoutliers \
    xgboost \
    lightgbm \
    jsonlite \
    RCurl \
    shinydashboard \
    shinyjs \
    shinyWidgets \
    htmltools

#RUN sudo /usr/bin/ R -e "install.packages('https://github.com/AdrianAntico/RemixAutoML/raw/master/RemixAutoML_0.6.0.tar.gz', repos = NULL, type = 'source', INSTALL_opts = c('--no-multiarch', '--no-test-load'))"
RUN R -e "devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')"
RUN R -e "devtools::install_github('AdrianAntico/prettydoc', upgrade = FALSE, dependencies = FALSE, force = TRUE)"
RUN R -e "devtools::install_github('AdrianAntico/RemixAutoML', upgrade = FALSE, dependencies = FALSE, force = TRUE)"

# data.table::fread(file='./ThreeGroup-FC-Walmart-XREG3.csv')
#EXPOSE 80
CMD /bin/bash \
&& R -e "options('shiny.port'=3838,shiny.host='0.0.0.0'); library(RemixAutoML); RemixAutoML::AppsPlotting(data=NULL,XVariable='XREG2',YVariable='XREG1',DateName='Date',GroupVariables=NULL, FilterVariable='XREG1', Browser=TRUE, Docker=TRUE, Debug=TRUE)"
#&& R -e "options('shiny.port'=80,shiny.host='0.0.0.0'); RemixAutoML::AppsPlotting(data=NULL,XVariable='XREG2',YVariable='XREG1',DateName='Date',GroupVariables=NULL,FilterVariable='XREG1',Debug = TRUE)"
#EXPOSE 3838
#WORKDIR /var/lib/docker/tmp/buildkit-mount696119871/RemixAutoML/inst/shiny-apps/Insights
#COPY . /srv/shiny-server/

#RUN top

#USER shiny

#CMD /bin/bash \
#&& R -e "options('shiny.port'=80,shiny.host='0.0.0.0')"

#EXPOSE 3838

#CMD ["/usr/bin/shiny-server"]