#!/bin/bash


# replace booking_database_run_2 with appropriate container name when running the database
sudo docker-compose run -d database
sudo docker run -p 3000:3000 -itv ~/locked.danang/booking/site:/opt/server --link booking_database_run_1:database -e "HOST=0.0.0.0" -e "PGHOST=database" -e "PGPORT=5432" -e "PGUSER=postgres" -e "PGDATABASE=postgres" lolotp/yesod /bin/bash
