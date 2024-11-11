# RShiny-pbpk-boilerplate

### Deploy application
Run following commands in terminal to deploy the application to server:
```
APP_NAME="myapp"
SHINY_DIR="/srv/shiny-server/$APP_NAME"

sudo mkdir -p $SHINY_DIR

sudo cp -R . $SHINY_DIR/
sudo chown -R shiny:shiny $SHINY_DIR

sudo systemctl restart shiny-server
```