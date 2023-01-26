# The script will download the Atlas data and setup the directories
options(timeout=300)

# create data-raw folder
dir.create("data-raw")


# download data
download.file("https://github.com/ITDP/atlas/releases/download/mvp1/sample_3.zip",
              "data-raw/sample_3.zip")

# unpack files
unzip("data-raw/sample_3.zip",
      exdir = "data-raw")

options(shiny.fullstacktrace = TRUE)



# upload data -------------------------------------------------------------
library(piggyback)
pb_new_release(repo = "ITDP/atlas", "v0.0.1")
