# Trying out Quinn's noaaGEFSpoint package to get forecast ensembles
# https://github.com/rqthomas/noaaGEFSpoint/
# devtools::install_github("rqthomas/noaaGEFSpoint")
 
outdir <- "../data_raw/meteorology/GEFS"
if(!dir.exists(outdir)) dir.create(outdir)

# -----------------------
# Try with Morton Arb
# -----------------------
site.name="MortonArb"
lat.in=41.812739
lon.in=-88.072749

noaaGEFSpoint::noaa_gefs_download_downscale(site_list = site.name, lat_list = lat.in, lon_list = lon.in, output_directory=outdir, forecast_time = "all", forecast_date = "all", downscale = F, run_parallel = FALSE, num_cores = 1, overwrite = FALSE)
# -----------------------

# -----------------------
# Try Quinn's example
# -----------------------
site_file <- system.file("extdata", "noaa_download_site_list.csv", package = "noaaGEFSpoint")

neon_sites <- read_csv(site_file)


noaaGEFSpoint::noaa_gefs_download_downscale(site_list = neon_sites$site_id, lat_list = neon_sites$latitude, lon_list = neon_sites$longitude, output_directory=outdir, forecast_time = "all", forecast_date = "all", downscale = TRUE, run_parallel = FALSE, num_cores = 1, overwrite = FALSE)
# -----------------------



# -----------------------
# Try syntax from eco4cast challenge: https://github.com/eco4cast/neon4cast-noaa-download/blob/master/launch_download_downscale.R
# -----------------------
noaaGEFSpoint::noaa_gefs_download_downscale(site_list = paste0(site.name, 2),
                                            lat_list = lat.in,
                                            lon_list= lon.in,
                                            output_directory = outdir,
                                            forecast_time = "00",
                                            forecast_date = Sys.Date(),
                                            downscale = FALSE,
                                            run_parallel = FALSE,
                                            num_cores = 1,
                                            method = "point",
                                            overwrite = FALSE)
# -----------------------

noaaGEFSpoint::noaa_gefs_download_downscale(site_list = paste0(site.name, 2),
                                            lat_list = lat.in,
                                            lon_list= lon.in,
                                            output_directory = outdir,
                                            forecast_time = "00",
                                            forecast_date = Sys.Date(),
                                            downscale = FALSE,
                                            run_parallel = FALSE,
                                            num_cores = 1,
                                            method = "grid",
                                            overwrite = FALSE)
