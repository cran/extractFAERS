#' Filter extracted FAERS data by reporter occupation
#' @name filterbyoccpFAERS
#' @param workingdir Character vector. The directory containing decompressed FAERS ASCII folders.
#' @param temp_dir Internal parameter used only when `extract1PSFAERS` is called internally. Do not modify.
#' @param occpextract Character vector. Specifies the occupation types to extract.
#'        Defaults to `c("MD", "HP", "PH", "OT")`.
#' @param savetoRData Logical. Determines whether to save `F_COREDATA_1PS_PROF.RData` in the working directory.
#'        Must be set to `TRUE` if `filter_by_occu_FAERS` is used independently.
#' @import dplyr
#' @import parallel
#' @import stringr
#'
#' @return A list containing six data frames, containing formatted FAERS data after selecting single-drug cases and filtering reports based on reporter occupation. Can be used by time_to_day_FAERS() to standardize time units.
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

filter_by_occp_FAERS <- function(workingdir = NULL, temp_dir=NULL, occpextract = NULL,savetoRData=FALSE) {

  if(is.null(temp_dir)==FALSE){workingdir<-temp_dir}

  # Check if workingdir is provided; stop execution if missing
  if (is.null(workingdir)) { stop("workingdir not found") }


  # If occpextract is not specified, use the default occupation types:
  # MD (Medical Doctor), HP (Health Professional), PH (Pharmacist), OT (Other)
  if (is.null(occpextract)) { occpextract <- c("MD", "HP", "PH", "OT") }

  # Load DEMO1PS directory (contains patient demographic data)
  doc <- paste0(workingdir,"/DEMO1PS/")
  fs <- list.files(doc)  # List all files in the directory

  # Read each CSV file, extract the reporting year from the filename
  files <- lapply(fs, function(x) {
    y <- read.csv(paste(doc, x, sep = ""), sep = "$", row.names = NULL)
    y$sysyear <- substr(x, 5, 8)  # Extract year from filename (characters 5-8)
    y
  })

  # Combine all loaded data into one data frame
  files <- do.call(rbind, files)

  # Select relevant columns and remove duplicates to ensure uniqueness
  demos <- files[c(1, 2, 4, 9, 14:20, 26, 23:25)] %>% unique()

  # Check if specified occupation types exist in the dataset
  if (!all(occpextract %in% (demos$occp_cod %>% table()))) {
    message(paste("Using occp_cod:", paste(occpextract, collapse = ", "),
                  ". Available occp_cod are:", paste(names(demos$occp_cod %>% table()), collapse =", ")))
  }

  # Load REAC1PS directory (contains adverse reaction data)
  doc <- paste0(workingdir,"/REAC1PS/")
  fs <- list.files(doc)  # List all files in the directory

  # Read and combine all CSV files
  files <- lapply(fs, function(x) {
    read.csv(paste(doc, x, sep = ""), sep = "$", row.names = NULL)
  })
  files <- do.call(rbind, files)
  allreac <- files  # Store all adverse reaction data

  # Load INDI1PS directory (contains indication data)
  doc <- paste0(workingdir,"/INDI1PS/")
  fs <- list.files(doc)  # List all files in the directory

  # Read and combine all CSV files
  files <- lapply(fs, function(x) {
    read.csv(paste(doc, x, sep = ""), sep = "$", row.names = NULL)
  })
  files <- do.call(rbind, files)
  allindi <- files  # Store all indication data

  ########################### Load drug information ###########################


  # Load DRUG directory (contains drug information)
  doc <- paste0(workingdir,"/DRUG/")

  # Load INDEX1PS directory (contains drug index data)
  indexdoc <- paste0(workingdir,"/INDEX1PS/")
  indexfs <- list.files(indexdoc)  # List all files in the directory
  indexfs <- as.list(indexfs)  # Convert to list

  # Read and combine all CSV files
  files <- lapply(indexfs, function(x) {
    read.csv(paste(indexdoc, x, sep = ""), sep = "$", row.names = NULL)
  })
  files2 <- do.call(rbind, files)

  # Select relevant columns and remove duplicates
  indexdata <- files2[, c(1, 2, 5, 6)]
  indexdata <- unique(indexdata)

  # Standardize drug names: remove special characters and replace backslashes
  indexdata$prod_ai <- str_remove_all(indexdata$prod_ai, ", \\(\\+/-\\)-")  # Remove ", (+/-)-"
  indexdata$prod_ai <- str_replace_all(indexdata$prod_ai, "\\\\", "/")  # Replace "\" with "/"

  # Extract unique drug names
  indexdrug <- unique(indexdata$prod_ai) %>% as.data.frame()

  # Extract unique adverse event terms
  indexpt <- allreac$pt %>% unique()

  # Save all extracted data into an RData file
  # save(indexdata, indexdrug, indexpt, allreac, allindi, demos, file="F_COREDATA_1PS.RData")

  ########################### Filter data based on occupation ###########################

  # Filter patient demographic data by specified occupation types
  demos_prof <- demos[demos$occp_cod %in% occpextract,]

  # Filter adverse reaction data for these patients
  allreac_prof <- allreac[allreac$primaryid %in% demos_prof$primaryid,]

  # Filter drug data for these patients
  indexdata_prof <- indexdata[indexdata$primaryid %in% demos_prof$primaryid,]

  # Filter indication data for these patients
  allindi_prof <- allindi[allindi$primaryid %in% demos_prof$primaryid,]

  # Extract unique drug names for these patients
  indexdrug_prof <- unique(indexdata_prof$prod_ai)

  # Extract unique adverse event terms for these patients
  indexpt_prof <- allreac_prof$pt %>% unique()

  # Match primaryid with year information and standardize the format (convert to 4-digit year, e.g., "2017")
  allreac_prof$sysyear <- demos_prof$sysyear[match(allreac_prof$primaryid, demos_prof$primaryid)]
  allreac_prof$sysyear <- paste0("20", substr(allreac_prof$sysyear, 1, 2))

  # Convert indication and adverse event terms to lowercase for consistency
  allindi_prof$indi_pt <- tolower(allindi_prof$indi_pt)
  allreac_prof$pt <- tolower(allreac_prof$pt)

  # Save filtered data into an RData file for further analysis
  outputdir<-paste0(workingdir,"/","F_COREDATA_1PS_PROF.RData")
  if(savetoRData){save(indexdata_prof, indexdrug_prof, indexpt_prof, allreac_prof, allindi_prof, demos_prof,
       file = outputdir)}
  return(list(allindi_prof=allindi_prof,allreac_prof=allreac_prof,demos_prof=demos_prof,indexdata_prof=indexdata_prof, indexdrug_prof=indexdrug_prof, indexpt_prof=indexpt_prof))
}


