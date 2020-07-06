FROM jenslaufer/docker-shiny-apps-base:latest

RUN apt-get update && apt-get install -y libproj-dev libgdal-dev

COPY R/apps/ /srv/shiny-server/apps

ARG MONGODB_URI=mongodb://apps-db

RUN echo "MONGODB_URI=${MONGODB_URI}" >> /home/shiny/.Renviron

RUN R -e "update.packages(lib.loc='/usr/local/lib/R/site-library', ask=FALSE, checkBuilt=TRUE, repos = 'https://cloud.r-project.org')"
RUN R -e "install.packages('proj4',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('mclust',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggalt',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('bbplot',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('scales',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinyWidgets',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinythemes',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('shinycssloaders',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('logging',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "install.packages('ggrepel',dependencies=TRUE, repos='http://cran.rstudio.com/')"
RUN R -e "devtools::install_github('vqv/ggbiplot')"