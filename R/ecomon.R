#' Cast staged data for one species into an ecomon compatible data frame.
#' 
#' Note that not everything transfers 1:1, so we make some choices for some
#' of the ecomon values (*e.g.* NA for gear_volume_filtered)
#' 
#' @export
#' @param x data frame of AZMP data
#' @param species chr, one of "calanus_finmarchicus", "calanus_glacialis",
#'  or "calanus_hyperboreus"
#' @return data frame, suitable to bind to ecomon (corrected) staged data.
#'  See [ecomon::read_staged()] for more information on the output.
as_ecomon = function(x = read_calanus(),
                     species = c("calanus_finmarchicus",
                                 "calanus_glacialis",
                                 "calanus_hyperboreus")[1]){
  
  stages = sprintf("%s_%s", species, as.roman(1:6) |> tolower()) |>
    rlang::set_names(sprintf("c%s_m2", 1:6))
  
  
  y = x |>
    dplyr::select(dplyr::all_of(dplyr::starts_with(species))) |>
    dplyr::rename(dplyr::all_of(stages)) |>
    dplyr::mutate(unk_m2 = NA_real_)
  
  date = sprintf("%0.4i-%0.2i-%0.2i", x$year, x$month, x$day) |>
    as.Date(format = "%Y-%m-%d")
  
  N = nrow(x)
  
  dplyr::tibble(
    seq = sprintf("AZMP_%i", seq_len(N)),
    cruise_name = sprintf("%s_%s", x$region, x$transect),
    station = x$station, 
    latitude = x$latitude, 
    longitude = x$longitude, 
    date = date, 
    sta_depth = NA_real_, 
    tow_depth = x$depth, 
    gear_volume_filtered = NA_real_, 
    zoo_aliquot = NA_real_,
    total_m2 = rowSums(y, na.rm = TRUE)) |>
  dplyr::bind_cols(y)
}

#' Retrieve a template to match the ecomon staged data table
#' 
#' @export
#' @return zero row data frame
ecomon_template = function(){
  dplyr::tibble(seq = 23221, 
                cruise_name = "MM7701",
                station = 2, 
                latitude = 41.1833, 
                longitude = -70.6667, 
                date = Sys.Date(), 
                sta_depth = 33, 
                tow_depth = 25, 
                gear_volume_filtered = 222.702, 
                zoo_aliquot = 256,
                total_m2 = 28.738, 
                c6_m2 = 28.73795476, 
                c5_m2 = 0, 
                c4_m2 = 0, 
                c3_m2 = 0, 
                c2_m2 = 0, 
                c1_m2 = 0,
                unk_m2 = 0) |>
    dplyr::slice(-1)
}