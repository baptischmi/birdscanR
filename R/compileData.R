#### compileData -----------------------------------------------------------
#' @title compileData
#' @author Baptiste Schmid, \email{baptiste.schmid@@vogelwarte.ch};
#' Fabian Hertner, \email{fabian.hertner@@swiss-birdradar.com}; 
#' Birgen Haest, \email{birgen.haest@@vogelwarte.ch}
#' @description The function \code{compileData} aim to filter database-extracts and save metadata used to compute MTR \code{computeMTR}. The function \code{compileData} is a list of filtered data and parameters. It takes the output from \code{extractDbData} 
#' and trunk the needed dataset to the restricted settings, e.g. time frame, pulse type.
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
                      echoValidator      = FALSE){
  
  # set the time window
  startTime = timeRangeTargetTZ[1]
  stopTime  = timeRangeTargetTZ[2]
  
  # Filter parameters
  # =============================================================================
  ls_filters <- list(
    timeRangeTargetTZ    = timeRangeTargetTZ,
    pulseTypeSelection   = pulseTypeSelection,
    rotationSelection    = rotationSelection,
    # targetTimeZone       = targetTimeZone, # redundant: not a filter, and also incldued in the radarSiteData.
    classSelection       = classSelection,
    classProbCutOff      = classProbCutOff,
    altitudeRange_AGL    = altitudeRange_AGL,
    echoValidator        = echoValidator
  )
  
  #-----------------------------------------------------------------------------
  # meta data
  metaFilters <- data.frame(
    "colname" = c('timeRangeTargetTZ',
                  'pulseTypeSelection',
                  'rotationSelection',
                  'classSelection',
                  'classProbCutOff',
                  'altitudeRange_AGL',
                  'echoValidator'
    ),
    "type" = c('POSIXct',
               'char',
               'integer',
               'char',
               'num',
               'num',
               'logical'
    ),
    "description" = c('Time range (beginning to end period) of data collection in the time zone sued for analyses.',
                      'Pulse type is either "S" for Short-pulse, "M" for Medium pulse, or "L" for Long-pulse. See radar table for pulse duration',
                      'Rotation is either "0"when the antenna is static, or "1" if the antena is rotating on its vertical axis. Flight speed and direction available only if the anteanna is rotating',
                      'List of class - can be a subset of all avaialble classes',
                      'PostHoc filter on classification. if "0", all echoes are used, if e.g. 0.4, only echoes with a class probability >= 0.4 are kept.',
                      'Altitude range (agl) from the lowest to the highest.',
                      'Wether to use the PostHoc echo validator as additional filter. Per default set on "FALSE".'
    )
  )
  
  # Filter protocol data
  # =============================================================================
  protocolDataSubset = filterProtocolData(protocolData       = protocolData, 
                                          pulseTypeSelection = pulseTypeSelection, 
                                          rotationSelection  = rotationSelection)  
  TimesInd = (protocolDataSubset$startTime_targetTZ < stopTime) & 
    (protocolDataSubset$stopTime_targetTZ > startTime)
  protocolDataSubset =  protocolDataSubset[TimesInd, ]
  #-----------------------------------------------------------------------------
  # meta data for ProtocolData
  metaProtocol <- data.frame(
    "colname" = c("protocolID", "siteID",
                  "startTime_originTZ", "startTime_targetTZ", "stopTime_originTZ", "stopTime_targetTZ",
                  "pulseType", "rotate", "stc", "threshold",               
                  "softwareVersion" 
                  
    ),
    "type" = c('int', 'int', 
               'POSIXct','POSIXct','POSIXct','POSIXct',
               'char', 'int', 'num', 'num',
               'logi' # obviously missing information!?
               
    ),
    "description" = c('Incremental ID of measurement periods - linked to EchoData and BlindTimes.',
                      'Site ID - linked to the site & radar data.',
                      'Timestamp upon the start of the measurement period. TimeZone as given in DB',
                      'Timestamp upon the start of the measurement period. TimeZone defined by the user, since 2020 usually UTC',
                      'Timestamp upon the end of the measurement period. TimeZone as given in DB',
                      'Timestamp upon the end of the measurement period. TimeZone defined by the user, since 2020 usually UTC',
                      'Either "S" for Short-pulse, "M" for Medium pulse, "L" for Long-pulse. See radar table for pulse duration',
                      '"0"when the antenna is static, "1" if the antena is rotating on its vertical axis. Flight speed and direction available only if the anteanna is rotating',
                      'Sensitivity Time Control in meter. Bascially a distance to set the minial detected object size. Key feature to calcualte the MTR-factor of the echo.',
                      'Detection threshold in DBm. Key feature to calcualte the MTR-factor of the echo.',
                      'not Available. Software version updon detection. Can differ from the classifier versions'
    )
  )
  
  
  # Filter Site & Radar data
  # =============================================================================
  site$targetTimeZone = targetTimeZone
  
  mycols_site <- c("radarID", "siteID", "siteCode", "siteName", "siteDesc",             
    "timeZone_tagetTZ", # "timeZone_originTZ",
    "projectStart_originTZ", "projectStart_targetTZ", "projectEnd_originTZ", "projectEnd_targetTZ",
    "longitude", "latitude", "altitude",
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
  # if( is.na(radarSiteData$timeShift) ) warning("The 'timeShift' parameter is missing. Edit the site table!")
 
  #-----------------------------------------------------------------------------
  # meta data
  metaRadarSiteData <- data.frame(
    "colname" = c("radarID", "siteID", "siteCode", "siteName", "siteDesc",             
                  "timeZone_targetTZ", # "timeZone_originTZ", 
                  "projectStart_originTZ", "projectStart_targetTZ", "projectEnd_originTZ", "projectEnd_targetTZ",
                  "longitude", "latitude", "altitude",
                  "customer",
                  "type", "serialNo", "northOffset", "delta", "tiltAngle",  
                  "transmitPower","antennaGainInDBi", "waveGuideAttenuation",
                  "xxx0V", "xxxSatLower", "xxxSteepness", "xxxSatUpper", "pulseLengthXxx"
    ),
    "type" = c('int',
               'int',
               'char',
               'char',
               'char',
               'char',
               'POSIXct',
               'POSIXct',
               'POSIXct',
               'POSIXct',
               'num',
               'num',
               'int',
               'char',
               'char',
               'int',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num',
               'num'
               
    ),
    "description" = c('Serial number of radar unit - abrevaited.',
                      'Radar location: Site ID (integer) given by radar operator.',
                      'Radar location: Site code (three letters) given by radar operator.',
                      'Radar location: full name.',
                      'Radar location: optional further description',
                      'Time Zone used for analyses, usually UTC',
                      'Beginning of the data collection, using the time zone set on radar.',
                      'Beginning of the data collection, using the time zone set for the analyses - see variable "timeZone_targetTZ".',
                      'End of the data collection, using the time zone set on radar.',
                      'End of the data collection, using the time zone set for the analyses - see variable "timeZone_targetTZ".',
                      'Radar location: Longitude', # toDo: specify format
                      'Radar location: Latitude',
                      'Radar location: altitude above sea level',
                      'Radar operator',
                      'Model of radar unit, e.r. "BirdScan MR1" from Swiss Birdradar Solution.',
                      'Serial number of radar unit - full',
                      'Radar parameter: northOffset',
                      'Radar parameter: delta',
                      'Radar parameter: titltAngle - a contstant for BirdScan MR1.',
                      'Radar parameter: transmitted power [W] - can vary between years because of exchange of the magnetron.',
                      'Radar parameter: Antenna gain [dBi] is given by the antenna - a contstant for BirdScan MR1.',
                      'Radar parameter: Wave Guide attenuation []is given by the antenna - a contstant for BirdScan MR1.',
                      'Pulse type parameter: xxx0V - Calibration. ',
                      'Pulse type  parameter: xxxSatLower - Calibration.',
                      'Pulse type  parameter: xxxSteepness - Calibration.',
                      'Pulse type  parameter: xxxSatUpper - Calibration.',
                      'Pulse type  parameter: pulseLengthXxx - Calibration - duration of the pulse length. This value ultimately define the range resolution.'
                      
    )
  )
  
  
  # Filter blindTimes data
  # =============================================================================
  # restrict the time range
  if(!any( names(BlindTimesData) == 'type') ) warning("The 'type' column is missing in the dataset 'BlindTimesData'. Use the output of the function 'mergeVisibilityAnd ManualBlinfTime'.")
  TimesInd = (BlindTimesData$start_targetTZ < stopTime) & 
    (BlindTimesData$stop_targetTZ > startTime)
  BlindTimesDataSubset =  BlindTimesData[TimesInd, ]
  
  #-----------------------------------------------------------------------------
  # meta data
  metaBlindTimes <- data.frame(
    "colname" = c('type',
                  'start_targetTZ',
                  'stop_targetTZ',
                  'protocolID'    
    ),
    "type" = c('char',
               'POSIXct',
               'POSIXct',
               'char'
    ),
    "description" = c('Type of BlindTime. 
                          Blindtime is used to calcualte the effective duration of measurements during a teporal bin of the MTR table.
                          Common denominations are: 
                          "protocolChange" that include the blindtime subsequent to the start of a new measrurement period (protocolID),
                          "technical" denote periods with technical misfunction of the radar,
                          "rain" denote periods of precipitation.',
                      'Beginning of the blind period',
                      'End of the blind period',
                      'ID of measurement periods - linked to protocol table'
                      
    )
  )
  
  # Filter twilight data 
  # =============================================================================
  # restrict the time range on sunStart and sunStop
  TimesInd = (sunriseSunsetData$sunStart < stopTime) & 
    (sunriseSunsetData$sunStop > startTime)
  sunriseSunsetDataSubset =  sunriseSunsetData[TimesInd, ]
  # ToDo: use the twilight function if no dataset is included, but the site table include the necessary info on location.
  
  #-----------------------------------------------------------------------------
  # meta data
  metaSunriseSunset <- data.frame(
    "colname" = c("is_night", "date",
                  "sunStart", "sunStop",
                  "civilStart",  "civilStop",
                  "nauticalStart", "nauticalStop" 
                  
    ),
    "type" = c('int', 'POSIXct',
               'POSIXct','POSIXct',
               'POSIXct','POSIXct',
               'POSIXct','POSIXct'
               
    ),
    "description" = c('"0" if daytime, "1" if nighttime,',
                      "Date of event (in UTC)",
                      "Time of sunrise in UTC - see site table for location",
                      "Time of sunset in UTC - see site table for location",
                      "Time of dawn (civil-twilight, 6° below horizon) in UTC - see site table for location",
                      "Time of dusk (civil-twilight, 6° below horizon) in UTC - see site table for location",
                      "Time of dawn (nautical-twilight, 9° below horizon) in UTC - see site table for location",
                      "Time of dusk (nautical-twilight, 9° below horizon) in UTC - see site table for location"
                      
    )
  )
  
  
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
  
  #-----------------------------------------------------------------------------
  # meta data
  metaEcho <- data.frame(
    "colname" = c('name'
                  
    ),
    "type" = c('char'
               
    ),
    "description" = c('dummy'
    )
  )
  
  
  #-----------------------------------------------------------------------------
  # meta data
  ls_metaData <- list(
    echoData          = metaEcho,
    protocolData      = metaProtocol,
    blindTimesData    = metaBlindTimes,
    sunriseSunsetData = metaSunriseSunset,
    radarSiteData     = metaRadarSiteData,
    filterParameters  = metaFilters
  )
  

  
  # Return the filtered protocol and echo data
  # =============================================================================
  compiledData = list(
    echoData           = echoDataSubset,
    protocolData       = protocolDataSubset,
    blindTimesData     = BlindTimesDataSubset,
    sunriseSunsetData  = sunriseSunsetDataSubset,
    radarSiteData      = radarSiteData,
    filterParameters   = ls_filters,
    metaData           = ls_metaData
                      )
  
  return(compiledData)
}