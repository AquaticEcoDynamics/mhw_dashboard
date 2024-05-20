FROM jingyingwong/mhw_dashboard:latest

EXPOSE 3838

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');shiny::runApp('app_cockburnsound.R')"