#' Generate a listing of column types suitable for\code{\link[readr]{read_tsv}}
#' 
#' @return a named list of column types
cols_calanus <- function(){
  list(
    region = readr::col_character(),
    transect = readr::col_character(),
    station = readr::col_character(),
    year = readr::col_double(),
    month = readr::col_double(),
    day = readr::col_double(),
    time = readr::col_time(format = ""),
    timezone = readr::col_character(),
    longitude = readr::col_double(),
    latitude = readr::col_double(),
    depth = readr::col_double(),
    mesh_size = readr::col_double(),
    calanus_finmarchicus_i = readr::col_double(),
    calanus_finmarchicus_ii = readr::col_double(),
    calanus_finmarchicus_iii = readr::col_double(),
    calanus_finmarchicus_iv = readr::col_double(),
    calanus_finmarchicus_v = readr::col_double(),
    calanus_finmarchicus_vi = readr::col_double(),
    calanus_glacialis_i = readr::col_double(),
    calanus_glacialis_ii = readr::col_double(),
    calanus_glacialis_iii = readr::col_double(),
    calanus_glacialis_iv = readr::col_double(),
    calanus_glacialis_v = readr::col_double(),
    calanus_glacialis_vi = readr::col_double(),
    calanus_hyperboreus_i = readr::col_double(),
    calanus_hyperboreus_ii = readr::col_double(),
    calanus_hyperboreus_iii = readr::col_double(),
    calanus_hyperboreus_iv = readr::col_double(),
    calanus_hyperboreus_v = readr::col_double(),
    calanus_hyperboreus_vi = readr::col_double(),
    .default = readr::col_guess()
  )
}


#' Read calanus abundance tables
#' 
#' @export
#' @param filename character, the file to read
#' @param form character, one of 'tibble' or 'sf'
#' @return  tibble or sf POINT table
read_calanus <- function(filename = c(
                          get_data_path("CalanusAbundance_m2_CAN_data.txt"),
                          get_data_path("CalanusAbundance_m2_CAN_data_1999_2024.txt"))[2],
                         form = c("tibble", "sf")[1]){
  
  x <- readr::read_tsv(filename,
                       col_types = cols_calanus())
  
  if (tolower(form[1]) == "sf"){
    x <- sf::st_as_sf(x, coords = c("longitude", "latitude"), crs = 4326)
  }
  x
}

#' Retrieve the columns to select when reading a prof ctd
#' 
#' @return charcater vector
cols_ctd_prof <- function(){
  c("DATA_TYPE", "STN_NUMBER","SOURCE_ID","D_P_CODE","OBS_YEAR" ,"OBS_MONTH", "OBS_DAY",
    "OBS_TIME","Q_DATE_TIME", "LONGITUDE (+E)", "LATITUDE (+N)","Q_POS", "DEPTH_PRESS","DP_FLAG",       
    "PSAL", "Q_PSAL", "TEMP", "Q_TEMP")
}

#' Read a single \code{prof} ctd file
#'
#' @export
#' @param filename char, the file to read
#' @param data_type char, codes of data types to retain
#' @param source_id char, codes of data sources to retain
#' @param q_date_time num, codes of q_data_time to retain
#' @param q_pos num, codes of q_pos to retain
#' @param q_temp num, codes of q_temp to retain
#' @param dp_flag num, codes of dp_flag to retain
#' @param q_psal num, codes of q_psal to retain
#' @return tibble
read_ctd_prof <- function(filename, 
                          data_type = c("CD", "CU"),
                          source_id =  c("BIO", "NAFC"),
                          q_date_time = c(1,2,5),
                          q_pos = c(1,2,5),
                          q_temp = c(1,2,5),
                          dp_flag = c(1,2,5),
                          q_psal = c(1,2,5)){
  
  data.table::fread(filename[1], select = cols_ctd_prof()) |>
    dplyr::as_tibble() |>
    dplyr::rename(c("LONGITUDE" = "LONGITUDE (+E)", 
                    "LATITUDE" =  "LATITUDE (+N)") ) |>
    dplyr::filter(
      .data$DATA_TYPE %in% data_type,
      .data$SOURCE_ID %in% source_id,
      .data$Q_DATE_TIME %in% q_date_time,
      .data$Q_POS %in% q_pos,
      .data$Q_TEMP %in% q_temp,
      .data$DP_FLAG %in% dp_flag,
      .data$Q_PSAL %in% q_psal
    ) |>
    dplyr::mutate(DEPTH=ifelse(.data$D_P_CODE=="P", 
                               oce::swDepth(.data$DEPTH_PRESS, latitude = .data$LATITUDE, 
                                            eos = getOption("oceEOS", default = "gsw")), 
                               .data$DEPTH_PRESS),
                  TIME = format(as.POSIXct(sprintf("%0.4i", .data$OBS_TIME),format="%H%S"), "%H:%S")) |>
    dplyr::rename(c("YEAR" = "OBS_YEAR", 
                    "MONTH" = "OBS_MONTH", 
                    "DAY" = "OBS_DAY")) |> 
    dplyr::select(dplyr::all_of(c("YEAR", "MONTH", "DAY", "TIME", 
                                "LONGITUDE", "LATITUDE", 
                                "DEPTH", "PSAL", "TEMP")))
}

read_ctd_surf <- function(filename){
  data.table::fread(filename[1]) |>
    dplyr::as_tibble()
}


#' Read a single CTD - note that this is sensitive to prof and surf
#' 
#' @export
#' @param filename character, the file to read
#' @param form character, one of 'tibble' or 'sf'
#' @return tibble or sf POINT table
read_ctd <- function(filename = get_data_path("Maritimes","a_MEDS_profile_prof_2015_2020.csv"),
                     form = c("tibble", "sf")[1]){
 
  is_prof <- grep("_prof_", filename[1], fixed = TRUE)
  x <- if (is_prof){
    read_ctd_prof(filename[1])
  } else {
    read_ctd_surf(filename[1])
  }
  if (tolower(form[1]) == "sf"){
    x <- sf::st_as_sf(x, coords = c("LONGITUDE (+E)", "LATITUDE (+N)"), crs = 4326)
  }
  x

}