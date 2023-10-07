sudo apt-get install libprotobuf-dev
sudo apt-get install libmagick++-dev
remotes::install_github("kauebraga/leaflet")



# upload data -------------------------------------------------------------
library(piggyback)
pb_release_delete(repo = "ITDP/atlas", "v0.0.1")
pb_release_delete(repo = "ITDP/atlas", "alpha")
pb_new_release(repo = "ITDP/atlas", "alpha")

pb_new_release(repo = "ITDP/atlas", "beta_test")

pb_upload("data/data_alpha.zip", 
          repo = "ITDP/atlas", 
          tag = "alpha")



# on the server -------------------------------------------------------------------------------

cd /srv/shiny-server && rm -r atlas && git clone git@github.com:itdp/atlas.git

mkdir atlas/data && cd atlas/data
wget https://github.com/ITDP/atlas/releases/download/beta/data_beta.zip
wget https://github.com/ITDP/atlas/releases/download/beta/data_beta.z01
wget https://github.com/ITDP/atlas/releases/download/beta/data_beta.z02
wget https://github.com/ITDP/atlas/releases/download/beta/data_beta.z03
wget https://github.com/ITDP/atlas/releases/download/beta/data_beta.z04
wget https://github.com/ITDP/atlas/releases/download/beta/data_beta.z05

zip -F data_beta.zip --out data_beta_full.zip

unzip data_beta_full.zip && rm data_beta_full.zip && systemctl restart shiny-server


# dev version ------------

cd /srv/shiny-server/atlas-dev && rm -r atlas && git clone -b alpha-ui-change git@github.com:itdp/atlas.git

mkdir atlas/data && cd atlas/data
wget https://github.com/ITDP/atlas/releases/download/alpha/data_alpha.zip
unzip data_alpha.zip && rm data_alpha.zip && systemctl restart shiny-server
