# Local

scp -r -i /home/joebrew/.ssh/odkkey.pem /home/joebrew/Documents/saint/credentials ubuntu@bohemia.team:/home/ubuntu/Documents/saint
scp -r -i /home/joebrew/.ssh/odkkey.pem /home/joebrew/Documents/saint/data.csv ubuntu@bohemia.team:/home/ubuntu/Documents/saint/data.csv

# Remote
sudo cp -r /home/ubuntu/Documents/saint/credentials/ /srv/shiny-server/saint/credentials
sudo cp /home/ubuntu/Documents/saint/data.csv /srv/shiny-server/saint/data.csv

sudo su - -c "R -e \"remove.packages('saint')\""
sudo su - -c "R -e \"devtools::install_github('databrew/saint')\""
sudo systemctl restart shiny-server