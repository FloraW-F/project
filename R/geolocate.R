
#add in checks to make sure output is correct
get_stem_location <- function(decimalLongitude, decimalLatitude, stemAzimuth, stemDistance) {
  #check inputs are correct
  checkmate::assert_numeric(decimalLongitude)
  checkmate::assert_numeric(decimalLatitude)
  checkmate::assert_numeric(stemAzimuth)
  checkmate::assert_numeric(stemDistance)
  #main body function
  out <- geosphere::destPoint(p = cbind(decimalLongitude, decimalLatitude), b = stemAzimuth, d = stemDistance) %>%
    tibble::as_tibble() 
  #check output is correct
  checkmate::assert_false(any(is.na(out))) #need to assign main function code an object name so can check this
  return(out)
}