sudo apt-get install libprotobuf-dev
sudo apt-get install libmagick++-dev
remotes::install_github("kauebraga/leaflet")


cd /srv/shiny-server && rm -r atlas && git clone git@github.com:itdp/atlas.git

mkdir atlas/data && cd atlas/data
wget https://github.com/ITDP/atlas/releases/download/alpha/data_alpha.zip
unzip data_alpha.zip && rm data_alpha.zip && systemctl restart shiny-server


# dev version]

cd /srv/shiny-server/atlas-dev && rm -r atlas && git clone -b alpha-ui-change git@github.com:itdp/atlas.git

mkdir atlas/data && cd atlas/data
wget https://github.com/ITDP/atlas/releases/download/alpha/data_alpha.zip
unzip data_alpha.zip && rm data_alpha.zip && systemctl restart shiny-server
