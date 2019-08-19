FROM ubuntu:16.04
# following command is needed for some reason https://github.com/phusion/baseimage-docker/issues/58
RUN echo 'debconf debconf/frontend select Noninteractive' | debconf-set-selections
RUN apt-get -y update \
&& apt-get install -y --no-install-recommends apt-utils \
&& apt-get -y upgrade
# Various dependencies to get stuff installed
RUN apt-get -y install wget curl gcc make libaio1 libaio-dev gdebi-core gfortran g++ libssl-dev libcurl4-openssl-dev \
liblapack-dev liblapack3 libopenblas-base libopenblas-dev libfreetype6-dev libmagick++-dev \
libjpeg-dev cargo libgl1-mesa-dev libglu1-mesa-dev apt-transport-https
# Because APS is watching everything we do... they intercept all outgoing HTTPS calls, substitute their own certificate,
# so they can MITM monitor traffic.  (un)fortunately, ubuntu base image doesn't really like that.  So we have to 
# manually add that it is okay for APS to do that.  To do so, we make a request to a "bad" site, save the certificates,
# and tell ubuntu to trust those certificates.  Oh boi!
# The first command actually does this, the other commands add the certificates as trusted
RUN cd /usr/share/ca-certificates && mkdir pnw && cd pnw \
&& openssl s_client -showcerts -verify 5 -connect github.com:443 < /dev/null | awk '/BEGIN/,/END/{ if(/BEGIN/){a++}; out="cert"a".pem"; print >out}' \
&& chmod 775 /usr/share/ca-certificates/pnw && chmod 664 -R /usr/share/ca-certificates/pnw/* \
&& for i in $(ls /usr/share/ca-certificates/pnw); do echo "pnw/$i"; done >> /etc/ca-certificates.conf \
&& update-ca-certificates
# SETUP R
# download packages directly into archive - https://community.c9.io/t/how-to-install-a-deb-package/5007
# have to do the below command to add r dependent packages
RUN echo "deb https://cloud.r-project.org/bin/linux/ubuntu xenial-cran35/" >> /etc/apt/sources.list \
&& apt-get -y update \
&& wget https://cran.cnr.berkeley.edu/bin/linux/ubuntu/xenial-cran35/r-base-core_3.6.0-1xenial_amd64.deb \
&& wget https://cran.cnr.berkeley.edu/bin/linux/ubuntu/xenial-cran35/r-recommended_3.6.0-1xenial_all.deb \
&& wget https://cran.cnr.berkeley.edu/bin/linux/ubuntu/xenial-cran35/r-base_3.6.0-1xenial_all.deb \
&& gdebi --non-interactive r-base-core_3.6.0-1xenial_amd64.deb \
&& gdebi --non-interactive r-recommended_3.6.0-1xenial_all.deb \
&& gdebi --non-interactive r-base_3.6.0-1xenial_all.deb \
&& rm -f r-base-core_3.6.0-1xenial_amd64.deb \ 
&& rm -f r-recommended_3.6.0-1xenial_all.deb \
&& rm -f r-base_3.6.0-1xenial_all.deb \
&& echo "r <- getOption('repos'); r['CRAN'] <- 'http://cran.cnr.berkeley.edu'; options(repos = r);" > ~/.Rprofile

# INSTALL R PACKAGES
RUN /usr/bin/Rscript -e "install.packages('knitr', dependencies = TRUE);warnings()"
RUN /usr/bin/Rscript -e "install.packages(c('data.table','foreach','doParallel','itertools', 'recommenderlab', 'lubridate', 'zoo', 'caTools', 'pROC', 'scatterplot3d', 'monreg', 'tm', 'wordcloud'), dependencies = TRUE);"
RUN /usr/bin/Rscript -e "install.packages(c('h2o','forecast', 'tsoutliers', 'xgboost'), dependencies = TRUE);"
# RUN /usr/bin/Rscript  -e "install.packages('h2o', type='source', repos=('http://h2o-release.s3.amazonaws.com/h2o/latest_stable_R'))"
RUN Rscript -e "devtools::install_github('catboost/catboost', subdir = 'catboost/R-package')"

# Setup Oracle
# RUN apt-get -y install alien
# COPY includes/oracle /tmp/oracle
# RUN alien -i /tmp/oracle/oracle*.rpm \
# && mkdir /usr/lib/oracle/11.2/client64/network/admin -p \
# && cp /tmp/oracle/tnsnames.ora /usr/lib/oracle/11.2/client64/network/admin/
# ENV ORACLE_HOME=/usr/lib/oracle/11.2/client64
# ENV OCI_LIB=/usr/lib/oracle/11.2/client64/lib
# ENV OCI_INC=/usr/include/oracle/11.2/client64
# ENV PATH=$PATH:$ORACLE_HOME/bin
# ENV LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$ORACLE_HOME/lib
# ENV TNS_ADMIN=$ORACLE_HOME/network/admin
# RUN /usr/bin/Rscript -e "install.packages(c('ROracle'), dependencies = TRUE);"
# RUN rm -r /tmp/oracle

# INSTALL LOCAL PACKAGES - https://kbroman.org/pkg_primer/pages/build.html
# COPY includes/r-packages /tmp/r-packages
RUN /usr/bin/Rscript -e "install.packages(c('arules','Matrix','RColorBrewer','magick','nortest'));"

# RUN R CMD build /tmp/r-packages/RemixAutoML && R CMD INSTALL RemixAutoML_0.5.0.tar.gz
RUN Rscript -e "devtools::install_github('AdrianAntico/RemixAutoML', upgrade = FALSE, dependencies = FALSE, force = TRUE);"
RUN rm -r /tmp/r-packages/
  # Maybe this will help?
RUN apt-get -y update && apt-get -y install default-jdk
# SETUP PYTHON
RUN apt-get -y install python-pip
RUN pip install cx_Oracle tensorflow keras==2.0 pandas dill scikit-learn==0.19 scipy==1.2
#RUN apt-get -y install software-properties-common
#RUN add-apt-repository ppa:deadsnakes/ppa && apt-get -y update && apt-get -y install python3.7 && echo 'python=python3.7' >> ~/.bashrc
# DEFAULT COMMAND
CMD /bin/bash