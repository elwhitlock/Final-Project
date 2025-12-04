# used Dr. Post's as temp

# start from the rstudio/plumber image
FROM rstudio/plumber

# install the linux libraries needed for plumber
RUN apt-get update -qq && apt-get install -y  libssl-dev  libcurl4-gnutls-dev libxml2-dev libpng-dev pandoc 
  
  
# install plumber, GGally
RUN R -e "install.packages(c('tidyverse','tidymodels','ranger', 'ggplot2'))"

# set wd trouble shooting errors
WORKDIR /app

# copy API.R dataset and model from the current directory into the container
# not everything is in project repo
COPY API.R API.R
COPY diabetes_binary_health_indicators_BRFSS2015.csv diabetes_binary_health_indicators_BRFSS2015.csv
COPY rf_final_model.rds rf_final_model.rds

# open port to traffic
EXPOSE 8385

# when the container starts, start the myAPI.R script
ENTRYPOINT ["R", "-e", \
    "pr <- plumber::plumb('API.R'); pr$run(host='0.0.0.0', port=8385)"]
