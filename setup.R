# CENSUS DATA API -------

# These lines of code need to be run once after the project is set up
# and should not be run again after that.
# 
library(miscgis)
library(secure)
library(acs)

# miscgis::mk_proj_dir()
# key <- "" #add the key here
# secure::encrypt("censusapi", key = key)
# acs::api.key.install(key = decrypt("censusapi"))