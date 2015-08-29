#!/bin/bash

# replace booking_database_run_1 with appropriate container name when running the database
sudo docker-compose run -d database
sudo docker run -p 3000:3000 -itv ~/locked.danang/booking/site:/opt/server --link booking_database_run_1:database -e "HOST=0.0.0.0" -e "PGHOST=database" -e "PGPORT=5432" -e "PGUSER=postgres" -e "PGDATABASE=postgres" -e "FB_APPID=fill_in" -e "FB_APPSECRET=fill_in" lolotp/yesod /bin/bash
