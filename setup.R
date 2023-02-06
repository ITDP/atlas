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
"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"
pb_release_delete(repo = "ITDP/atlas", "v0.0.1")
pb_new_release(repo = "ITDP/atlas", "v0.0.1")

pb_upload("data/data_alpha.zip", 
          repo = "ITDP/atlas", 
          tag = "v0.0.1")
