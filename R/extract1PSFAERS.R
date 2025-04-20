#' Extract reports with only one drug used from FAERS data
#'
#' This function processes the FDA Adverse Event Reporting System (FAERS) data
#' to extract reports where only a single drug was administered.
#' Only data after 2014Q3 can be directly extracted with this package
#'
#' @name extract1PSFAERS
#' @param workingdir Character vector. The directory containing the decompressed FAERS ASCII folders.
#' @param usetempdir Logical. If TRUE, processed files are stored in a temporary directory; otherwise, they are saved in `workingdir`.
#' @param corenum Numeric. The number of CPU cores to use for parallel processing. Using more cores reduces processing time.
#' @param startfile Numeric. The index of the first file to process in the DRUG and related folders.
#' @param endfile Numeric. The index of the last file to process in the DRUG and related folders.
#' @param occpextract Character vector. Specifies the occupation categories for data extraction. Defaults to `c("MD", "HP", "PH", "OT")`.
#' @param onlydoextract Logical. If TRUE, only extracts data without performing additional combination or filtering steps.
#'
#'
#' @import dplyr
#' @import parallel
#' @import stringr
#' @import utils
#'
#' @return A character vector containing the file paths of the processed folders
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



extract_FAERS_data <- function(workingdir = NULL,usetempdir=FALSE, corenum = NULL, startfile = NULL, endfile = NULL, onlydoextract=FALSE ,occpextract = NULL) {

  # Validate input parameters
  if (is.null(workingdir)) {
    stop("Error: 'workingdir' is not provided. Please specify the directory containing FAERS ASCII folders.")
  }


  if (!is.numeric(corenum) | is.null(corenum)) {
    stop("Error: 'corenum' must be a numeric value specifying the number of CPU cores.")
  }

  #------ Decompress FAERS data files ------
  zip_files <- list.files(workingdir, pattern = "faers_ascii.*\\.zip$", full.names = TRUE)

  if (length(zip_files) == 0) {
    message("No valid .zip files found in the directory.\n")
  } else {
    message("Found the following .zip files:\n")
    message(zip_files)
  }

  # Extract all found ZIP files

  if (usetempdir==TRUE){
    temp_dir<-tempdir()
    for (zip_file in zip_files) {
      message("Extracting:", basename(zip_file), "\n")
      unzip(zip_file, exdir = paste0(temp_dir,"/unzip/"), overwrite = TRUE)
    }
    txt_files <- list.files(paste0(temp_dir,"/unzip/"), pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
    dir.create(paste0(temp_dir,"/ascii/"), showWarnings = FALSE)
    file.copy(txt_files, file.path(paste0(temp_dir,"/ascii/"), basename(txt_files)), overwrite = TRUE)
    message("Extraction complete. All files are now in:", temp_dir, "\n")
  }else{
    temp_dir<-tempdir()
    for (zip_file in zip_files) {
      message("Extracting:", basename(zip_file), "\n")
      unzip(zip_file, exdir = paste0(temp_dir,"/unzip/"), overwrite = TRUE)
    }
    txt_files <- list.files(paste0(temp_dir,"/unzip/"), pattern = "\\.txt$", full.names = TRUE, recursive = TRUE)
    dir.create(paste0(workingdir,"/ascii/"), showWarnings = FALSE)
    file.copy(txt_files, file.path(paste0(workingdir,"/ascii/"), basename(txt_files)), overwrite = TRUE)
    message("Extraction complete. All files are now in:", workingdir, "\n")
    }






  #------ Organizing ASCII files into categorized folders ------
  ascii_folder <- paste0(workingdir, "/ascii/")
  demo_folder <- paste0(workingdir, "/DEMO/")
  reac_folder <- paste0(workingdir, "/REAC/")
  drug_folder <- paste0(workingdir, "/DRUG/")
  indi_folder <- paste0(workingdir, "/INDI/")

  if (usetempdir==TRUE){
    ascii_folder <- paste0(temp_dir, "/ascii/")
    demo_folder <- paste0(temp_dir, "/DEMO/")
    reac_folder <- paste0(temp_dir, "/REAC/")
    drug_folder <- paste0(temp_dir, "/DRUG/")
    indi_folder <- paste0(temp_dir, "/INDI/")
  }

  # Create necessary directories if they do not exist
  dir.create(demo_folder, showWarnings = FALSE)
  dir.create(reac_folder, showWarnings = FALSE)
  dir.create(drug_folder, showWarnings = FALSE)
  dir.create(indi_folder, showWarnings = FALSE)

  files <- list.files(ascii_folder, full.names = TRUE)

  # Move files into their respective folders based on their names
  for (file in files) {
    if (grepl("DEMO", basename(file))) {
      file.rename(file, file.path(demo_folder, basename(file)))
    } else if (grepl("REAC", basename(file))) {
      file.rename(file, file.path(reac_folder, basename(file)))
    } else if (grepl("DRUG", basename(file))) {
      file.rename(file, file.path(drug_folder, basename(file)))
    } else if (grepl("INDI", basename(file))) {
      file.rename(file, file.path(indi_folder, basename(file)))
    }
  }

  if (usetempdir==FALSE){
  unlink(paste0(workingdir,"/ascii"), recursive = TRUE)
  }

    message("Files have been organized.\n")

  #------ Create output directories for processed data ------

    if(usetempdir==FALSE){
      index1ps_folder<-paste0(workingdir,"/INDEX1PS/")
      demo1ps_folder<-paste0(workingdir,"/DEMO1PS/")
      indi1ps_folder<-paste0(workingdir,"/INDI1PS/")
      reac1ps_folder<-paste0(workingdir,"/REAC1PS/")
    }else{
      index1ps_folder<-paste0(temp_dir,"/INDEX1PS/")
      demo1ps_folder<-paste0(temp_dir,"/DEMO1PS/")
      indi1ps_folder<-paste0(temp_dir,"/INDI1PS/")
      reac1ps_folder<-paste0(temp_dir,"/REAC1PS/")
    }
    dir.create(index1ps_folder, showWarnings = FALSE)
    dir.create(demo1ps_folder, showWarnings = FALSE)
    dir.create(indi1ps_folder, showWarnings = FALSE)
    dir.create(reac1ps_folder, showWarnings = FALSE)
    print(index1ps_folder)
    print(demo1ps_folder)
    print(indi1ps_folder)
    print(reac1ps_folder)
  #------ Extract reports with a single drug from DRUG files ------
  doc <- drug_folder
  fs <- list.files(doc)

  if (is.null(startfile)) startfile = 1
  if (is.null(endfile)) endfile = length(fs)

  extract1PS_Drug <- function(i) {
    indexdoc <- index1ps_folder
    fs <- list.files(doc)

    message(paste("Processing DRUG file", i, "... This may take minitues to hours.\n"))

    DEMOs <- read.csv(paste(doc, fs[i], sep = ""), sep = "$", row.names = NULL)

    # Filter cases where only one drug is reported
    DEMOs2 <- split.data.frame(DEMOs, DEMOs$primaryid)
    DEMOs3 <- lapply(DEMOs2, function(x) {
      if (nrow(x) == 1) return(x)
    })
    DEMOs3 <- do.call(rbind, DEMOs3)
    DEMOs3 <- DEMOs3[DEMOs3$prod_ai != "",]

    write.table(DEMOs3, paste(indexdoc, fs[i], sep = ""), sep = "$", row.names = FALSE, quote = FALSE)

    message(paste("Finished processing DRUG file", i, "\n"))
  }

  message("Extracting DRUG data, This may take minitues to hours\n")
  cl <- makeCluster(corenum)
  parLapply(cl, startfile:endfile, extract1PS_Drug)
  stopCluster(cl)
# i=1
  #------ Extract corresponding DEMO, INDI, and REAC data based on index ------
  extract1PS_DEMO<- function(i) {
    fs <- list.files(demo_folder)
    fsindex <- list.files(index1ps_folder)
    message(paste("Processing", demo_folder, "file", i, "... This may take minitues to hours.\n"))
    DEMOs <- read.csv(paste(demo_folder, fs[i], sep = ""), sep = "$", row.names = NULL)
    INDEX <- read.csv(paste(index1ps_folder, fsindex[i], sep = ""), sep = "$", row.names = NULL)
    # Filter records based on primaryid in the index file
    DEMOs <- DEMOs[DEMOs$primaryid %in% INDEX$primaryid,]
    write.table(DEMOs, paste(demo1ps_folder, fs[i], sep = ""), sep = "$", row.names = FALSE, quote = FALSE)
    message(paste("Finished processing", demo_folder, "file", i, "\n"))
  }

    cl <- makeCluster(corenum)
    message(paste("Extracting", demo_folder, "data, This may take minitues to hours\n"))
    parLapply(cl, startfile:endfile,extract1PS_DEMO)
    stopCluster(cl)
  # Process DEMO, INDI, and REAC data

    extract1PS_INDI<- function(i) {
      fs <- list.files(indi_folder)
      fsindex <- list.files(index1ps_folder)
      message(paste("Processing", indi_folder, "file", i, "... This may take minitues to hours.\n"))
      DEMOs <- read.csv(paste(indi_folder, fs[i], sep = ""), sep = "$", row.names = NULL)
      INDEX <- read.csv(paste(index1ps_folder, fsindex[i], sep = ""), sep = "$", row.names = NULL)
      # Filter records based on primaryid in the index file
      DEMOs <- DEMOs[DEMOs$primaryid %in% INDEX$primaryid,]
      write.table(DEMOs, paste(indi1ps_folder, fs[i], sep = ""), sep = "$", row.names = FALSE, quote = FALSE)
      message(paste("Finished processing", indi_folder, "file", i, "\n"))
    }

    cl <- makeCluster(corenum)
    message(paste("Extracting", indi_folder, "data, This may take minitues to hours\n"))
    parLapply(cl, startfile:endfile,extract1PS_INDI)
    stopCluster(cl)

    extract1PS_REAC<- function(i) {
      fs <- list.files(reac_folder)
      fsindex <- list.files(index1ps_folder)
      message(paste("Processing", reac_folder, "file", i, "... This may take minitues to hours.\n"))
      DEMOs <- read.csv(paste(reac_folder, fs[i], sep = ""), sep = "$", row.names = NULL)
      INDEX <- read.csv(paste(index1ps_folder, fsindex[i], sep = ""), sep = "$", row.names = NULL)
      # Filter records based on primaryid in the index file
      DEMOs <- DEMOs[DEMOs$primaryid %in% INDEX$primaryid,]
      write.table(DEMOs, paste(reac1ps_folder, fs[i], sep = ""), sep = "$", row.names = FALSE, quote = FALSE)
      message(paste("Finished processing", reac_folder, "file", i, "\n"))
    }

    cl <- makeCluster(corenum)
    message(paste("Extracting", reac_folder, "data, This may take minitues to hours\n"))
    parLapply(cl, startfile:endfile, extract1PS_REAC)
    stopCluster(cl)



  # Apply additional data processing functions if needed
  if(onlydoextract==FALSE){
  message("Filter data by reporter occupation")
    if(usetempdir==TRUE){
  filteres<-filter_by_occp_FAERS(workingdir = workingdir,temp_dir=temp_dir, occpextract = occpextract,savetoRData = TRUE)
    }else{filteres<-filter_by_occp_FAERS(workingdir = workingdir,temp_dir=NULL, occpextract = occpextract,savetoRData = TRUE)}
  message("\nDone")
  message("\nChange all time unit to day")
  if(usetempdir==FALSE){
  time_to_day_FAERS(workingdir=workingdir,filteres=filteres)}else{
    time_to_day_FAERS(workingdir=temp_dir,filteres=filteres)
  }
  message("\nDone")
  message("\nProcessing complete. Processed files are in the directory returned.
The file 'F_COREDATA_1PS_PROF_STU.RData' in workingdir contains single-drug reports from the FAERS database.
This dataset can be used for further analysis or integrated with the Visdrugs Shiny application.
For more details on Visdrugs, visit: https://github.com/mrpotatod/Visdrugs_v.0.3.0")
  }else{
    message("\nProcessing complete. Processed files are in the directory returned.
To filter data based on occupation, run 'filter_by_occp_FAERS()' with the working directory set to the location of the 'XXXX1PS' folders.
This will generate 'F_COREDATA_1PS_PROF.RData'.
To standardize time units to days, use 'time_to_day_FAERS()' in the same directory to obtain 'F_COREDATA_1PS_PROF_STU.RData'.\n")
    }
  return(dirname(index1ps_folder))
}
