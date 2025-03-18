#' Change all time units to days in the data filtered by filter_by_occu_FAERS().
#' This function converts age and time units in the data to days, and processes occupation and reaction data.
#' @name timetodayFAERS
#' @param workingdir Directory containing `F_COREDATA_1PS_PROF.RData`.
#' @param usexistRData Logical. Specifies whether to use `F_COREDATA_1PS_PROF.RData` in the working directory for calculations.
#'        Must be set to `TRUE` and `workingdir` must be provided if `time_to_day_FAERS` is used independently.
#' @param filteres Filtered results for changing time units. Used only when `extract1PSFAERS` is called internally.
#'        Set to `NULL` if `time_to_day_FAERS` is used separately.
#' @import dplyr
#' @import stringr
#'
#' @return A character vector containing the path of the processed file "F_COREDATA_1PS_PROF_STU.RData", which can be used for further analysis
#'
#' @examples
#' # Example_1 Perform FAERS data preprocessing in one step and
#' # generate `F_COREDATA_1PS_PROF_STU.RData` in a temporary folder.
#' # In practice, it is recommended to set `usetempdir = FALSE` and specify `workingdir`
#' # to prevent the processed results in the temporary folder from being automatically deleted.
#' extract_FAERS_data(
#'   workingdir = system.file("extdata", package = "extractFAERS"),
#'   usetempdir = TRUE,
#'   corenum = 2,
#'   startfile = 1,
#'   endfile = 4,
#'   onlydoextract = FALSE,
#'   occpextract = NULL
#' )
#'
#' # Example_2 Stepwise FAERS data preprocessing
#' # Setting `onlydoextract = TRUE` extracts only single-drug cases and organizes file paths.
#' # The processed file paths are saved in a temporary directory.
#' extractfaerspath <- extract_FAERS_data(
#'   workingdir = system.file("extdata", package = "extractFAERS"),
#'   usetempdir = TRUE,
#'   corenum = 2,
#'   startfile = 1,
#'   endfile = 4,
#'   onlydoextract = TRUE,
#'   occpextract = NULL
#' )
#' print(extractfaerspath)
#'
#' # Filter data based on reporter occupation
#' # By default, only reports from healthcare professionals
#' # (e.g., physicians, pharmacists) are retained.
#' faers1psprofdata <- filter_by_occp_FAERS(
#'   workingdir = extractfaerspath,
#'   occpextract = NULL,
#'   savetoRData = TRUE
#' )
#'
#' # Standardize time units to days
#' # This ensures consistency in the dataset and facilitates analysis of adverse reactions
#' # based on patient age.
#' time_to_day_FAERS(
#' workingdir = extractfaerspath,
#' usexistRData = TRUE,
#' filteres = NULL
#' )
#'
#' @details
#' This package includes example data files in `extdata`:
#' - `faers_ascii_2015q1_example.zip`: Example dataset 1.
#' - `faers_ascii_2015q2_example.zip`: Example dataset 2.
#' - `faers_ascii_2015q3_example.zip`: Example dataset 3.
#' - `faers_ascii_2015q4_example.zip`: Example dataset 4.
#' - Use `system.file("extdata",package = "extractFAERS")` to access the folder contain example zip files.
#'
#' @export

time_to_day_FAERS <- function(workingdir=NULL,usexistRData=FALSE,filteres=NULL) {
  # Load the dataset containing filtered data
  if(usexistRData){load(paste0(workingdir,"/F_COREDATA_1PS_PROF.RData"))}else{
  demos_prof=filteres$demos_prof
  allindi_prof=filteres$allindi_prof
  allreac_prof=filteres$allreac_prof
  demos_prof=filteres$demos_prof
  indexdata_prof=filteres$indexdata_prof
  indexdrug_prof=filteres$indexdrug_prof
  indexpt_prof=filteres$indexpt_prof
  }
  # Convert the year in the 'sysyear' field to a 4-digit year format
  demos_prof$sysyear <- substr(demos_prof$sysyear, 1, 2)
  demos_prof$sysyear <- paste0("20", demos_prof$sysyear)

  # Convert age to days based on the age code
  demos_prof$age_day[demos_prof$age_cod == "DEC"] <- demos_prof$age[demos_prof$age_cod == "DEC"] * 10 * 360
  demos_prof$age_day[demos_prof$age_cod == "YR"] <- demos_prof$age[demos_prof$age_cod == "YR"] * 360
  demos_prof$age_day[demos_prof$age_cod == "MON"] <- demos_prof$age[demos_prof$age_cod == "MON"] * 30
  demos_prof$age_day[demos_prof$age_cod == "WK"] <- demos_prof$age[demos_prof$age_cod == "WK"] * 7
  demos_prof$age_day[demos_prof$age_cod == "DY"] <- demos_prof$age[demos_prof$age_cod == "DY"]
  demos_prof$age_day[demos_prof$age_cod == "HR"] <- demos_prof$age[demos_prof$age_cod == "HR"] / 24

  # Count the number of occurrences for each reaction (pt) in the reaction data
  allreaccount <- allreac_prof$pt %>% table %>% as.data.frame
  allreac_prof$count <- allreaccount$Freq[match(allreac_prof$pt, allreaccount$.)]

  # Assign occupation country information from the demo data to the reaction data
  allreac_prof$occr_country <- demos_prof$occr_country[match(allreac_prof$primaryid, demos_prof$primaryid)]

  # Standardize occupation country values by replacing missing or incorrect values
  allreac_prof$occr_country[allreac_prof$occr_country == ""] <- "N.R."
  allreac_prof$occr_country[allreac_prof$occr_country == " "] <- "N.R."
  allreac_prof$occr_country[allreac_prof$occr_country == "TW"] <- "CN"

  # Convert 'pt' column to lowercase and capitalize the first letter of each word
  allreac_prof$pt <- allreac_prof$pt %>% tolower() %>% str_to_sentence()

  # Convert the 'indexpt_prof' data to lowercase and capitalize the first letter of each word
  indexpt_prof <- indexpt_prof %>% tolower() %>% str_to_sentence()

  # Save the processed data to a new file
  message("Processed file that can be used for further analysis is in:")
  save(allindi_prof, allreac_prof, demos_prof, indexdata_prof, indexdrug_prof, indexpt_prof, file = paste0(workingdir,"/F_COREDATA_1PS_PROF_STU.RData"))
    return(paste0(workingdir,"/F_COREDATA_1PS_PROF_STU.RData"))


  }
