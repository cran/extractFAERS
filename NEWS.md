# extractFAERS_0.1.4 - Release Notes

## Changes in version 0.1.4 (2025-06-18)

-Modify the code to retain only the latest report for each duplicate case in F_COREDATA_1PS_PROF_STU.RData. A new file zzz.R has been added to define global variables to suppress related warnings during package checks.


# extractFAERS_0.1.3 - Release Notes

## Changes in version 0.1.3 (2025-05-15)

-Modify the code so that the final F_COREDATA_1PS_PROF_STU.RData file contains an additional column age_day in the allreac_prof dataframe, which represents the patient's age in days.


# extractFAERS_0.1.2 - Release Notes

## Changes in version 0.1.2 (2025-03-27)

-Updated the DESCRIPTION file to improve the package description

-Improved the description of the extractFAERS function for better clarity

-Fixed minor documentation formatting issues

-Fixed an issue where files could not be extracted when the top-level folder name in the compressed file was not ‘ascii’