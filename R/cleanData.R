#### compileData -----------------------------------------------------------
#' @title compileData
#' @author Baptiste Schmid, \email{baptiste.schmid@@vogelwarte.ch};
#' Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description The function compileData aim to filter database-extracts and save metadata used to compute MTR \code{computeMTR}. The function \code{compileData} is a list of filtered data and parameters.
#'
#' @param echoData dataframe with the echo data from the data list created by the function \code{extractDBData}.
#' @param protocolData dataframe with the protocol data from the data list created by the function \code{extractDBData} or a subset of it created by the function \code{filterProtocolData}. Echoes not detected during the listed protocols will be excluded.
#' @param protocolData dataframe resulting from the function \code{extractDbData}.
#' @param pulseTypeSelection character vector with the pulse types which should be included in the subset. Options: “S”, “M”, “L” (short-, medium-, long-pulse). Default is NULL: no filtering applied based on pulseType.
#' @param rotationSelection numeric vector to select the operation modes with and/or without antenna rotation. Options: 0, 1. (0 = no rotation, 1 = rotation). Default is NULL: no filtering applied based on rotation mode.
#' @param timeRangeTargetTZ Character vector of length 2, with start and end of 
#' time range, formatted as "%Y-%m-%d %H:%M". Echoes outside the time range will
#'  be excluded.
#' @param targetTimeZone "Etc/GMT0" String specifying the target time zone. 
#' Default is "Etc/GMT0".
#' @param classSelection character string vector with the classes that should be 
#' included.
#' @param classProbCutOff numeric cutoff value for class probabilities. Echoes 
#' with a lower class probability will be excluded.
#' @param altitudeRange_AGL numeric vector of length 2 with start and end of the 
#' altitude range. Echoes outside the altitude range will be excluded.
#' @param manualBlindTimes dataframe with the manual blind times created by the 
#' function \code{loadManualBlindTimes}.
#' @param echoValidator logical, if set to TRUE, echoes labelled by the echo 
#' validator as “non-bio scatterer” will be excluded. If set to FALSE, all 
#' echoes are included.
#' 
#' @return Returns filtered data table (echo, protocol, blindTimes, sunriseAndSunset, site, radar) and necessary paramters as input for \code{computeMTR}.
#' @export
#' @examples
#' \dontrun{
#'   
#' }
#' 
compileData = function( 
                      echoData           = NULL, 
                      protocolData       = NULL,
                      BlindTimesData     = NULL,
                      sunriseSunsetData  = NULL,
                      radarSiteData      = NULL,
                      pulseTypeSelection = NULL, 
                      rotationSelection  = NULL,
                      timeRangeTargetTZ  = NULL,
                      targetTimeZone     = "Etc/GMT0",
                      classSelection     = NULL, 
                      classProbCutOff    = NULL, 
                      altitudeRange_AGL  = NULL, 
                      manualBlindTimes   = NULL, 
                      echoValidator      = FALSE){
  
  # set the time window
  startTime = timeRangeTargetTZ[1]
  stopTime  = timeRangeTargetTZ[2]

  
  # Filter protocol data
  # =============================================================================
  protocolDataSubset = filterProtocolData(protocolData       = protocolData, 
                                          pulseTypeSelection = pulseTypeSelection, 
                                          rotationSelection  = rotationSelection)  
  TimesInd = (protocolDataSubset$startTime_targetTZ < stopTime) & 
    (protocolDataSubset$stopTime_targetTZ > startTime)
  protocolDataSubset =  protocolDataSubset[TimesInd, ]
  
  
  # Filter Site & Radar data
  # =============================================================================
  mycols_site <- c("radarID", "siteID", "siteCode", "siteName", "siteDesc",             
  "timeShift", 
  "projectStart_originTZ", "projectStart_targetTZ", "projectEnd_originTZ", "projectEnd_targetTZ",
  "longitude", "latitude", "altitude", "radarOrientation",
  "customer"
  )
  # Select according to Pulse Type
  mycols_radar<- c("type", "serialNo", "northOffset", "delta", "tiltAngle",  
                   "transmitPower","antennaGainInDBi", "waveGuideAttenuation"
                   ) 
                   
  if(pulseTypeSelection == 'S') 
  {
    mycols_radar <- c(
      mycols_radar, 
      c("short0V", "shortSatLower", "shortSteepness", "shortSatUpper", "pulseLengthShort")
    )
  }
  if(pulseTypeSelection == 'M') 
  {
    mycols_radar <- c(
      mycols_radar, 
      c("medium0V", "mediumSatLower", "mediumSteepness", "mediumSatUpper", "pulseLengthMedium")
    )
  }
  if(pulseTypeSelection == 'L') 
  {
    mycols_radar <- c(
      mycols_radar, 
      c("long0V", "longSatLower", "longSteepness", "longSatUpper","pulseLengthLong")
    )
  }
  # filter variables
  radarSiteData <- radarSiteData[, c(mycols_site, mycols_radar)]
  if( is.na(radarSiteData$timeShift) ) warning("The 'timeShift' parameter is missing. Edit the site table!")
 
  
  # Filter blindTimes data
  # =============================================================================
  # restrict the time range
  if(!any( names(BlindTimesData) == 'type') ) warning("The 'type' column is missing in the dataset 'BlindTimesData'. Use the output of the function 'mergeVisibilityAnd ManualBlinfTime'.")
  TimesInd = (BlindTimesData$start_targetTZ < stopTime) & 
    (BlindTimesData$stop_targetTZ > startTime)
  BlindTimesDataSubset =  BlindTimesData[TimesInd, ]
  
  # Filter twilight data 
  # =============================================================================
  # restrict the time range on sunStart and sunStop
  TimesInd = (sunriseSunsetData$sunStart < stopTime) & 
    (sunriseSunsetData$sunStop > startTime)
  sunriseSunsetDataSubset =  sunriseSunsetData[TimesInd, ]
  # ToDo: use the twilight function if no dataset is included, but the site table include the necessary info on location.
  
  
  # Filter echo data
  # =============================================================================
  echoDataSubset = filterEchoData(echoData          = echoData, 
                                  timeRangeTargetTZ = timeRangeTargetTZ, 
                                  targetTimeZone    = targetTimeZone,
                                  protocolData      = protocolDataSubset, 
                                  classSelection    = classSelection, 
                                  classProbCutOff   = classProbCutOff, 
                                  altitudeRange_AGL = altitudeRange_AGL, 
                                  manualBlindTimes  = manualBlindTimes, 
                                  echoValidator     = echoValidator) 
  
# Return the filtered protocol and echo data
# =============================================================================
  filteredData = list(echoData     = echoDataSubset,
                      protocolData = protocolDataSubset,
                      BlindTimesData = BlindTimesDataSubset,
                      sunriseSunsetData = sunriseSunsetDataSubset,
                      siteData = siteDataSubset
                      )
  return(filteredData)
}