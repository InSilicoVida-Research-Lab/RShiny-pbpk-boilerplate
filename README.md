# RShiny-pbpk-boilerplate

### Deploy application
Run following commands in terminal to deploy the application to server:
```
APP_NAME="myapp"
SHINY_DIR="/srv/shiny-server/$APP_NAME"

sudo mkdir -p $SHINY_DIR

sudo cp -R . $SHINY_DIR/
sudo chown -R shiny:shiny $SHINY_DIR

## To allow installation of packages as a shiny_user
sudo chown -R shiny:shiny /usr/local/lib/R/site-library
sudo chmod -R 775 /usr/local/lib/R/site-library

sudo systemctl restart shiny-server
```
