# Local

scp -r -i /home/joebrew/.ssh/odkkey.pem /home/joebrew/Documents/saint/credentials ubuntu@bohemia.team:/home/ubuntu/Documents/saint

# Remote
sudo cp -r /home/ubuntu/Documents/saint/credentials/ /srv/shiny-server/saint/credentials

sudo su - -c "R -e \"remove.packages('saint')\""
sudo su - -c "R -e \"devtools::install_github('databrew/saint')\""
sudo systemctl restart shiny-server