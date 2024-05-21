FROM jingyingwong/mhw_base:latest

#COPY renv.lock.prod renv.lock
#RUN R -e 'renv::restore()'

#RUN R -e 'devtools::install_github("dbca-wa/rivRmon")'
# COPY www/ /www/
# RUN ls --recursive /www/

RUN mkdir /home/mhw_app 
 
COPY app_cockburnsound.R /home/mhw_app/app_cockburnsound.R
COPY functions.R /home/mhw_app/functions.R
RUN ls -la
COPY .env /home/mhw_app/.env
COPY www/ /home/mhw_app/www/

WORKDIR /home/mhw_app

EXPOSE 3838
# CMD Rscript -e "options('shiny.port'=3838,shiny.host='0.0.0.0');library(shiny);/home/mhw_app/app_cockburnsound.R"
CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');shiny::runApp('app_cockburnsound.R')"

# FROM mhw_base

# # Create directory for the Shiny app
# RUN mkdir /home/mhw_app

# # Copy necessary files into the app directory
# COPY app_cockburnsound.R /home/mhw_app/
# COPY functions.R /home/mhw_app/
# COPY .env /home/mhw_app/

# # Copy the contents of the www directory into the app directory
# COPY www/ /home/mhw_app/www/

# # Set working directory to the app directory
# WORKDIR /home/mhw_app

# # Expose the Shiny app port
# EXPOSE 3838

# # Command to run the Shiny app
# CMD Rscript -e "options('shiny.port' = 3838, shiny.host = '0.0.0.0'); library(shiny); source('app_cockburnsound.R')"
