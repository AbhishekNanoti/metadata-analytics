##########################     ORGS COUNTRIES   ##############################

# Subsetting the Orgs countries to perform class and sci_capacity operations
subset_merge_df <- merge_of_total_reference_check_separatedH[, c("country_1", "country_2", "country_3", "country_4", "country_5", "country_6", "country_7", "country_8", "country_9", "country_10", "country_11", "country_12")]

# Replacing the country names with proper full forms for correct joining with sci_tech and class
for (i in 1:12) {
  col_name <- paste0("country_", i)
  subset_merge_df[, col_name] <- gsub("uk", "United Kingdom", subset_merge_df[, col_name])
}

for (i in 1:12) {
  col_name <- paste0("country_", i)
  subset_merge_df[, col_name] <- gsub("UK", "United Kingdom", subset_merge_df[, col_name])
}
for (i in 1:12) {
  col_name <- paste0("country_", i)
  subset_merge_df[, col_name] <- gsub("U.K", "United Kingdom", subset_merge_df[, col_name])
}
for (i in 1:12) {
  col_name <- paste0("country_", i)
  subset_merge_df[, col_name] <- gsub("USA", "United States", subset_merge_df[, col_name])
}
for (i in 1:12) {
  col_name <- paste0("country_", i)
  subset_merge_df[, col_name] <- gsub("Russia", "Russian Federation", subset_merge_df[, col_name])
}

# Remove the null column
subset_merge_df <- subset_merge_df %>% 
  select(-country_12)

library(stringi)

# Perform left join based on country names
merged_df <- subset_merge_df %>%
  left_join(scitech, by = c("country_1" = "country"))

# Repeat the above steps for the remaining country columns (country_2 to country_12)
for (i in 2:11) {
  col_name <- paste0("country_", i)
  
  merged_df <- merged_df %>%
    left_join(scitech, by = setNames("country", col_name))
}

# Combine the country columns into a single column for Orgs countries
merged_df$country_combined <- apply(merged_df[, paste0("country_", 1:11)], 1, function(x) paste(na.omit(x), collapse = ", "))

# Combine the iso3c and sci_capacity columns into a single column for Orgs countries
merged_df <- merged_df %>%
  unite(iso3c_combined, starts_with("iso3c."), sep = ", ", na.rm = TRUE) %>%
  unite(sci_capacity_combined, starts_with("sci_capacity."), sep = ", ", na.rm = TRUE)

# Appending the Class information to the countries from Orgs
for (i in 1:11) {
  col_name <- paste0("country_", i)
  
  merged_df <- merged_df %>%
    left_join(class, by = setNames("Economy", col_name))
}
merged_df <- merged_df %>%
  unite(region_combined, starts_with("Region."), sep = ", ", na.rm = TRUE) %>%
  unite(income_group_combined, starts_with("Income.group."), sep = ", ", na.rm = TRUE) %>%
  unite(Lending_category_combined, starts_with("Lending.category."), sep = ", ", na.rm = TRUE) %>%
  unite(EMUorHIPC_combined, starts_with("Other..EMU.or.HIPC."), sep = ", ", na.rm = TRUE)

merged_df <- merged_df %>% 
  select(-c(14:15))

merged_df <- merged_df %>% 
  select(-Code.x)

merged_df <- merged_df %>% 
  select(-c(19:ncol(merged_df)))



###################       DIRECT SUBMISSION COUNTRIES    ######################

# Subsetting the Direct Submission countries to perform class and sci_capacity operations
new_subset_merge_df <- merge_of_total_reference_check_separatedH[, c("countryD_1", "countryD_2", "countryD_3", "countryD_4", "countryD_5")]

# Changing the Column names
colnames(new_subset_merge_df) <- c("dataCountry1", "dataCountry2", "dataCountry3", "dataCountry4", "dataCountry5")

# Replacing the country names with proper full forms for correct joining with sci_tech and class
for (i in 1:5) {
  col_name <- paste0("dataCountry", i)
  new_subset_merge_df[, col_name] <- gsub("USA", "United States", new_subset_merge_df[, col_name])
}

for (i in 1:5) {
  col_name <- paste0("dataCountry", i)
  new_subset_merge_df[, col_name] <- gsub("uk", "United Kingdom", new_subset_merge_df[, col_name])
}


for (i in 1:5) {
  col_name <- paste0("dataCountry", i)
  new_subset_merge_df[, col_name] <- gsub("UK", "United Kingdom", new_subset_merge_df[, col_name])
}

for (i in 1:5) {
  col_name <- paste0("dataCountry", i)
  new_subset_merge_df[, col_name] <- gsub("U.K", "United Kingdom", new_subset_merge_df[, col_name])
}

for (i in 1:5) {
  col_name <- paste0("dataCountry", i)
  new_subset_merge_df[, col_name] <- gsub("Russia", "Russian Federation", new_subset_merge_df[, col_name])
}

# Keeping a consistent form of country names in all the columns
new_subset_merge_df$dataCountry1 <- str_to_title(new_subset_merge_df$dataCountry1)
new_subset_merge_df$dataCountry2 <- str_to_title(new_subset_merge_df$dataCountry2)
new_subset_merge_df$dataCountry3 <- str_to_title(new_subset_merge_df$dataCountry3)
new_subset_merge_df$dataCountry4 <- str_to_title(new_subset_merge_df$dataCountry4)
new_subset_merge_df$dataCountry5 <- str_to_title(new_subset_merge_df$dataCountry5)

# Perform left join based on country names
new_merged_df <- new_subset_merge_df %>%
  left_join(scitech, by = c("dataCountry1" = "country"))

# Repeat the above steps for the remaining country columns (dataCountry2 to dataCountry5)
for (i in 2:5) {
  col_name <- paste0("dataCountry", i)
  
  new_merged_df <- new_merged_df %>%
    left_join(scitech, by = setNames("country", col_name))
}

# Combine the country columns into a single column for Direct submission countries
new_merged_df$country_combined <- apply(new_merged_df[, paste0("dataCountry", 1:5)], 1, function(x) paste(na.omit(x), collapse = ", "))

# Combine the iso3c and sci_capacity columns into a single column for Direct submission countries
new_merged_df <- new_merged_df %>%
  unite(iso3c_combined, starts_with("iso3c."), sep = ", ", na.rm = TRUE) %>%
  unite(sci_capacity_combined, starts_with("sci_capacity."), sep = ", ", na.rm = TRUE)

# Appending the Class information to the countries from Direct Submission Countries
for (i in 1:5) {
  col_name <- paste0("dataCountry", i)
  
  new_merged_df <- new_merged_df %>%
    left_join(class, by = setNames("Economy", col_name))
}
new_merged_df <- new_merged_df %>%
  unite(region_combined, starts_with("Region."), sep = ", ", na.rm = TRUE) %>%
  unite(income_group_combined, starts_with("Income.group."), sep = ", ", na.rm = TRUE) %>%
  unite(Lending_category_combined, starts_with("Lending.category."), sep = ", ", na.rm = TRUE) %>%
  unite(EMUorHIPC_combined, starts_with("Other..EMU.or.HIPC."), sep = ", ", na.rm = TRUE)

new_merged_df <- new_merged_df %>% 
  select(-c(9))
  
new_merged_df <- new_merged_df %>% 
  select(-Code.x)

new_merged_df <- new_merged_df %>% 
  select(-c(13:19))



colnames(new_merged_df)[6:12] <- c("dataSubmissioniso3cCombined", "dataSubmissionSCICapacityCombined", "dataSubmissionCountryCombined", "dataSubmissionRegionCombined", "dataSubmissionIncomeGroupCombined", "dataSubmissionLendingCategoryCombined", "dataSubmissionEMUofHIPCCombined")
colnames(merged_df)[12:18] <- c("Orgsiso3cCombined", "OrgsSCICapacityCombined", "OrgsCountryCombined", "OrgsRegionCombined", "OrgsIncomeGroupCombined", "OrgsLendingCategoryCombined", "OrgsEMUofHIPCCombined")


merge_of_total_reference_check_separatedH <- merge_of_total_reference_check_separatedH %>% 
  select(-c(20:ncol(merge_of_total_reference_check_separatedH)))

# THE FINAL CSV
final_collaboration_df <- cbind(merge_of_total_reference_check_separatedH, merged_df, new_merged_df)

#Writing into a file
final_collaboration.file <-
  paste0("/groups/sebratt/ananoti/final_collaboration.csv")
write.csv(final_collaboration_df, final_collaboration.file,
          row.names = TRUE)


subset_final_collaboration <- final_collaboration_df[, c("refid","index","OrgsCountryCombined","dataSubmissionCountryCombined")]

subset_refIndexMerge_new <- refIndexMerge_new[, c("refid","index")]

# Assuming you already have the data frame subset_final_collaboration

subset_refIndexMerge_new <- subset_refIndexMerge_new %>%
  group_by(refid, index) %>%
  summarise(count = n())

subset_final_collaboration_x <- subset_final_collaboration %>%
  group_by(refid, index) %>%
  count() %>%
  ungroup()

subset_final_collaboration_xx <- subset_final_collaboration %>%
  group_by(refid, index) %>%
  summarise(OrgsCountryCombined = paste(OrgsCountryCombined, collapse = ", "),
            dataSubmissionCountryCombined = paste(dataSubmissionCountryCombined, collapse = ", "))

subset_final_collaboration_SCI <- final_collaboration_df %>%
  group_by(refid, index) %>%
  summarise(OrgsSCICapacityCombined = paste(OrgsSCICapacityCombined, collapse = ", "),
            dataSubmissionSCICapacityCombined = paste(dataSubmissionSCICapacityCombined, collapse = ", "))

# Remove leading and trailing commas
subset_final_collaboration_SCI$OrgsSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$OrgsSCICapacityCombined, "^,|,$|^ | $", "")
subset_final_collaboration_SCI$OrgsSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$OrgsSCICapacityCombined, ",$", "")
subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined, "^,|,$|^ | $", "")

# Remove consecutive commas and empty values
subset_final_collaboration_SCI$OrgsSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$OrgsSCICapacityCombined, ",{2,}", ",")
subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined, ",{2,}", ",")

# Remove empty values within the string
subset_final_collaboration_SCI$OrgsSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$OrgsSCICapacityCombined, "(^,|,)\\s*,", ",")
subset_final_collaboration_SCI$OrgsSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$OrgsSCICapacityCombined, ",(\\s*,|$)", ",")
subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined, "(^,|,)\\s*,", ",")
subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined, ",(\\s*,|$)", ",")

# Remove spaces before and after commas
subset_final_collaboration_SCI$OrgsSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$OrgsSCICapacityCombined, "\\s*,\\s*", ",")
subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined <- str_replace_all(subset_final_collaboration_SCI$dataSubmissionSCICapacityCombined, "\\s*,\\s*", ",")

subset_final_collaboration_xx_merge <- merge(refIndexMerge_new, subset_final_collaboration_xx, by = c("refid","index"))

# Writing the file
subset_final_collaboration_for_Mrudang.file <-
  paste0("/groups/sebratt/ananoti/subset_final_collaboration_for_Mrudang.csv")
write.csv(subset_final_collaboration_xx_merge, subset_final_collaboration_for_Mrudang.file,
          row.names = TRUE)

################################################################################

# Creating a duplicate
SCI_distribution <- subset_final_collaboration_SCI

SCI_distributionv2 <- SCI_distribution

#########################  Orgs scientific capacity  ##########################

# Creating base variables for Publications
SCI_distribution$OrgsSAC <- ifelse(grepl("SAC", SCI_distribution$OrgsSCICapacityCombined), 1, 0)
SCI_distribution$OrgsSPC <- ifelse(grepl("SPC", SCI_distribution$OrgsSCICapacityCombined), 1, 0)
SCI_distribution$OrgsSDC <- ifelse(grepl("SDC", SCI_distribution$OrgsSCICapacityCombined), 1, 0)
SCI_distribution$OrgsSLC <- ifelse(grepl("SLC", SCI_distribution$OrgsSCICapacityCombined), 1, 0)

# Finding homogeneous teams of only one category
SCI_distribution$OrgsSAC_H <- ifelse(grepl("\\bSAC\\b", SCI_distribution$OrgsSCICapacityCombined) &
                                       !grepl("SPC|SDC|SLC", SCI_distribution$OrgsSCICapacityCombined), 1, 0)

SCI_distribution$OrgsSPC_H <- ifelse(grepl("\\bSPC\\b", SCI_distribution$OrgsSCICapacityCombined) &
                                       !grepl("SAC|SDC|SLC", SCI_distribution$OrgsSCICapacityCombined), 1, 0)

SCI_distribution$OrgsSDC_H <- ifelse(grepl("\\bSDC\\b", SCI_distribution$OrgsSCICapacityCombined) &
                                       !grepl("SPC|SAC|SLC", SCI_distribution$OrgsSCICapacityCombined), 1, 0)

SCI_distribution$OrgsSLC_H <- ifelse(grepl("\\bSLC\\b", SCI_distribution$OrgsSCICapacityCombined) &
                                       !grepl("SPC|SDC|SAC", SCI_distribution$OrgsSCICapacityCombined), 1, 0)

# Finding pairs
SCI_distribution$OrgsSAC_SPC <- ifelse(SCI_distribution$OrgsSAC == 1 & SCI_distribution$OrgsSPC == 1 & SCI_distribution$OrgsSDC == 0 & SCI_distribution$OrgsSLC == 0, 1, 0)
SCI_distribution$OrgsSAC_SDC <- ifelse(SCI_distribution$OrgsSAC == 1 & SCI_distribution$OrgsSPC == 0 & SCI_distribution$OrgsSDC == 1 & SCI_distribution$OrgsSLC == 0, 1, 0)
SCI_distribution$OrgsSAC_SLC <- ifelse(SCI_distribution$OrgsSAC == 1 & SCI_distribution$OrgsSPC == 0 & SCI_distribution$OrgsSDC == 0 & SCI_distribution$OrgsSLC == 1, 1, 0)
SCI_distribution$OrgsSPC_SDC <- ifelse(SCI_distribution$OrgsSAC == 0 & SCI_distribution$OrgsSPC == 1 & SCI_distribution$OrgsSDC == 1 & SCI_distribution$OrgsSLC == 0, 1, 0)
SCI_distribution$OrgsSPC_SLC <- ifelse(SCI_distribution$OrgsSAC == 0 & SCI_distribution$OrgsSPC == 1 & SCI_distribution$OrgsSDC == 0 & SCI_distribution$OrgsSLC == 1, 1, 0)
SCI_distribution$OrgsSDC_SLC <- ifelse(SCI_distribution$OrgsSAC == 0 & SCI_distribution$OrgsSPC == 0 & SCI_distribution$OrgsSDC == 1 & SCI_distribution$OrgsSLC == 1, 1, 0)

# Finding triplets
SCI_distribution$OrgsSAC_SPC_SDC <- ifelse(SCI_distribution$OrgsSAC == 1 & SCI_distribution$OrgsSPC == 1 & SCI_distribution$OrgsSDC == 1 & SCI_distribution$OrgsSLC == 0, 1, 0)
SCI_distribution$OrgsSAC_SPC_SLC <- ifelse(SCI_distribution$OrgsSAC == 1 & SCI_distribution$OrgsSPC == 1 & SCI_distribution$OrgsSDC == 0 & SCI_distribution$OrgsSLC == 1, 1, 0)
SCI_distribution$OrgsSAC_SDC_SLC <- ifelse(SCI_distribution$OrgsSAC == 1 & SCI_distribution$OrgsSPC == 0 & SCI_distribution$OrgsSDC == 1 & SCI_distribution$OrgsSLC == 1, 1, 0)
SCI_distribution$OrgsSPC_SDC_SLC <- ifelse(SCI_distribution$OrgsSAC == 0 & SCI_distribution$OrgsSPC == 1 & SCI_distribution$OrgsSDC == 1 & SCI_distribution$OrgsSLC == 1, 1, 0)

# Finding Quadraplets
SCI_distribution$OrgsSAC_SPC_SDC_SLC <- ifelse(SCI_distribution$OrgsSAC == 1 & SCI_distribution$OrgsSPC == 1 & SCI_distribution$OrgsSDC == 1 & SCI_distribution$OrgsSLC == 1, 1, 0)


######################   Datasets scientific capacity   ######################

# Creating base variables for Datasets
SCI_distribution$dataSubmissionSAC <- ifelse(grepl("SAC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)
SCI_distribution$dataSubmissionSPC <- ifelse(grepl("SPC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)
SCI_distribution$dataSubmissionSDC <- ifelse(grepl("SDC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)
SCI_distribution$dataSubmissionSLC <- ifelse(grepl("SLC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

# Finding homogeneous teams of only one category
SCI_distribution$dataSubmissionSAC_H <- ifelse(grepl("\\bSAC\\b", SCI_distribution$dataSubmissionSCICapacityCombined) &
                                                 !grepl("SPC|SDC|SLC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

SCI_distribution$dataSubmissionSPC_H <- ifelse(grepl("\\bSPC\\b", SCI_distribution$dataSubmissionSCICapacityCombined) &
                                                 !grepl("SAC|SDC|SLC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

SCI_distribution$dataSubmissionSDC_H <- ifelse(grepl("\\bSDC\\b", SCI_distribution$dataSubmissionSCICapacityCombined) &
                                                 !grepl("SPC|SAC|SLC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

SCI_distribution$dataSubmissionSLC_H <- ifelse(grepl("\\bSLC\\b", SCI_distribution$dataSubmissionSCICapacityCombined) &
                                                 !grepl("SPC|SDC|SAC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

# Finding pairs
SCI_distribution$dataSubmissionSAC_SPC <- ifelse(SCI_distribution$dataSubmissionSAC == 1 & SCI_distribution$dataSubmissionSPC == 1 & SCI_distribution$dataSubmissionSDC == 0 & SCI_distribution$dataSubmissionSLC == 0, 1, 0)
SCI_distribution$dataSubmissionSAC_SDC <- ifelse(SCI_distribution$dataSubmissionSAC == 1 & SCI_distribution$dataSubmissionSPC == 0 & SCI_distribution$dataSubmissionSDC == 1 & SCI_distribution$dataSubmissionSLC == 0, 1, 0)
SCI_distribution$dataSubmissionSAC_SLC <- ifelse(SCI_distribution$dataSubmissionSAC == 1 & SCI_distribution$dataSubmissionSPC == 0 & SCI_distribution$dataSubmissionSDC == 0 & SCI_distribution$dataSubmissionSLC == 1, 1, 0)
SCI_distribution$dataSubmissionSPC_SDC <- ifelse(SCI_distribution$dataSubmissionSAC == 0 & SCI_distribution$dataSubmissionSPC == 1 & SCI_distribution$dataSubmissionSDC == 1 & SCI_distribution$dataSubmissionSLC == 0, 1, 0)
SCI_distribution$dataSubmissionSPC_SLC <- ifelse(SCI_distribution$dataSubmissionSAC == 0 & SCI_distribution$dataSubmissionSPC == 1 & SCI_distribution$dataSubmissionSDC == 0 & SCI_distribution$dataSubmissionSLC == 1, 1, 0)
SCI_distribution$dataSubmissionSDC_SLC <- ifelse(SCI_distribution$dataSubmissionSAC == 0 & SCI_distribution$dataSubmissionSPC == 0 & SCI_distribution$dataSubmissionSDC == 1 & SCI_distribution$dataSubmissionSLC == 1, 1, 0)

# Finding triplets
SCI_distribution$dataSubmissionSAC_SPC_SDC <- ifelse(SCI_distribution$dataSubmissionSAC == 1 & SCI_distribution$dataSubmissionSPC == 1 & SCI_distribution$dataSubmissionSDC == 1 & SCI_distribution$dataSubmissionSLC == 0, 1, 0)
SCI_distribution$dataSubmissionSAC_SPC_SLC <- ifelse(SCI_distribution$dataSubmissionSAC == 1 & SCI_distribution$dataSubmissionSPC == 1 & SCI_distribution$dataSubmissionSDC == 0 & SCI_distribution$dataSubmissionSLC == 1, 1, 0)
SCI_distribution$dataSubmissionSAC_SDC_SLC <- ifelse(SCI_distribution$dataSubmissionSAC == 1 & SCI_distribution$dataSubmissionSPC == 0 & SCI_distribution$dataSubmissionSDC == 1 & SCI_distribution$dataSubmissionSLC == 1, 1, 0)
SCI_distribution$dataSubmissionSPC_SDC_SLC <- ifelse(SCI_distribution$dataSubmissionSAC == 0 & SCI_distribution$dataSubmissionSPC == 1 & SCI_distribution$dataSubmissionSDC == 1 & SCI_distribution$dataSubmissionSLC == 1, 1, 0)

# Finding Quadraplets
SCI_distribution$dataSubmissionSAC_SPC_SDC_SLC <- ifelse(SCI_distribution$dataSubmissionSAC == 1 & SCI_distribution$dataSubmissionSPC == 1 & SCI_distribution$dataSubmissionSDC == 1 & SCI_distribution$dataSubmissionSLC == 1, 1, 0)

#############   Dataset-Publication Combo scientific capacity   #############

# Creating base variables for combination of Datasets Publications
SCI_distribution$ComboSAC <- ifelse(grepl("SAC", SCI_distribution$OrgsSCICapacityCombined) & 
                                      grepl("SAC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)
SCI_distribution$ComboSPC <- ifelse(grepl("SPC", SCI_distribution$OrgsSCICapacityCombined) & 
                                      grepl("SPC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)
SCI_distribution$ComboSDC <- ifelse(grepl("SDC", SCI_distribution$OrgsSCICapacityCombined) & 
                                      grepl("SDC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)
SCI_distribution$ComboSLC <- ifelse(grepl("SLC", SCI_distribution$OrgsSCICapacityCombined) & 
                                      grepl("SLC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

# Finding homogeneous teams of only one category
SCI_distribution$ComboSAC_H <- ifelse(grepl("\\bSAC\\b", SCI_distribution$OrgsSCICapacityCombined) &
                                        !grepl("SPC|SDC|SLC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

SCI_distribution$ComboSPC_H <- ifelse(grepl("\\bSPC\\b", SCI_distribution$OrgsSCICapacityCombined) &
                                        !grepl("SAC|SDC|SLC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

SCI_distribution$ComboSDC_H <- ifelse(grepl("\\bSDC\\b", SCI_distribution$OrgsSCICapacityCombined) &
                                        !grepl("SPC|SAC|SLC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

SCI_distribution$ComboSLC_H <- ifelse(grepl("\\bSLC\\b", SCI_distribution$OrgsSCICapacityCombined) &
                                        !grepl("SPC|SDC|SAC", SCI_distribution$dataSubmissionSCICapacityCombined), 1, 0)

# Finding pairs
SCI_distribution$ComboSAC_SPC <- ifelse(SCI_distribution$ComboSAC == 1 & SCI_distribution$ComboSPC == 1 & SCI_distribution$ComboSDC == 0 & SCI_distribution$ComboSLC == 0, 1, 0)
SCI_distribution$ComboSAC_SDC <- ifelse(SCI_distribution$ComboSAC == 1 & SCI_distribution$ComboSPC == 0 & SCI_distribution$ComboSDC == 1 & SCI_distribution$ComboSLC == 0, 1, 0)
SCI_distribution$ComboSAC_SLC <- ifelse(SCI_distribution$ComboSAC == 1 & SCI_distribution$ComboSPC == 0 & SCI_distribution$ComboSDC == 0 & SCI_distribution$ComboSLC == 1, 1, 0)
SCI_distribution$ComboSPC_SDC <- ifelse(SCI_distribution$ComboSAC == 0 & SCI_distribution$ComboSPC == 1 & SCI_distribution$ComboSDC == 1 & SCI_distribution$ComboSLC == 0, 1, 0)
SCI_distribution$ComboSPC_SLC <- ifelse(SCI_distribution$ComboSAC == 0 & SCI_distribution$ComboSPC == 1 & SCI_distribution$ComboSDC == 0 & SCI_distribution$ComboSLC == 1, 1, 0)
SCI_distribution$ComboSDC_SLC <- ifelse(SCI_distribution$ComboSAC == 0 & SCI_distribution$ComboSPC == 0 & SCI_distribution$ComboSDC == 1 & SCI_distribution$ComboSLC == 1, 1, 0)

# Finding triplets
SCI_distribution$ComboSAC_SPC_SDC <- ifelse(SCI_distribution$ComboSAC == 1 & SCI_distribution$ComboSPC == 1 & SCI_distribution$ComboSDC == 1 & SCI_distribution$ComboSLC == 0, 1, 0)
SCI_distribution$ComboSAC_SPC_SLC <- ifelse(SCI_distribution$ComboSAC == 1 & SCI_distribution$ComboSPC == 1 & SCI_distribution$ComboSDC == 0 & SCI_distribution$ComboSLC == 1, 1, 0)
SCI_distribution$ComboSAC_SDC_SLC <- ifelse(SCI_distribution$ComboSAC == 1 & SCI_distribution$ComboSPC == 0 & SCI_distribution$ComboSDC == 1 & SCI_distribution$ComboSLC == 1, 1, 0)
SCI_distribution$ComboSPC_SDC_SLC <- ifelse(SCI_distribution$ComboSAC == 0 & SCI_distribution$ComboSPC == 1 & SCI_distribution$ComboSDC == 1 & SCI_distribution$ComboSLC == 1, 1, 0)

# Finding Quadraplets
SCI_distribution$ComboSAC_SPC_SDC_SLC <- ifelse(SCI_distribution$ComboSAC == 1 & SCI_distribution$ComboSPC == 1 & SCI_distribution$ComboSDC == 1 & SCI_distribution$ComboSLC == 1, 1, 0)

# Writing the file
SCI_distribution.file <-
  paste0("/groups/sebratt/ananoti/SCI_distribution.csv")
write.csv(SCI_distribution, SCI_distribution.file,
          row.names = TRUE)

################## TO ADD YEAR in SCI_distribution  ##########################

new_SCI_distribution <- left_join(SCI_distribution, final_collaboration_df %>% select(refid, index, year_etc.x, year_etc.y), by = c("refid", "index"))

new_subset_final_collaboration <- final_collaboration_df[, c("refid","index","year_etc.x","year_etc.y")]

new_subset_final_collaboration_xx <- new_subset_final_collaboration %>%
  group_by(refid, index) %>%
  summarise(yearX = paste(year_etc.x, collapse = ", "),
            yearY = paste(year_etc.y, collapse = ", "))

new_subset_final_collaboration_xx$yearX <- sapply(strsplit(new_subset_final_collaboration_xx$yearX, ","), function(x) {
  if (length(unique(x)) == 1) {
    return(unique(x))
  } else {
    return(paste(unique(x), collapse = ", "))
  }
})

new_subset_final_collaboration_xx$yearY <- sapply(strsplit(new_subset_final_collaboration_xx$yearY, ","), function(x) {
  if (length(unique(x)) == 1) {
    return(unique(x))
  } else {
    return(paste(unique(x), collapse = ", "))
  }
})


new_subset_final_collaboration_xx$yearX <- sapply(strsplit(new_subset_final_collaboration_xx$yearX, ","), function(x) {
  if (length(unique(x)) == 1) {
    return(unique(x))
  } else {
    return(head(unique(x), 1))
  }
})

new_subset_final_collaboration_xx$yearY <- sapply(strsplit(new_subset_final_collaboration_xx$yearY, ","), function(x) {
  if (length(unique(x)) == 1) {
    return(unique(x))
  } else {
    return(head(unique(x), 1))
  }
})

SCI_distribution <- cbind(SCI_distribution, new_subset_final_collaboration_xx[c("yearX", "yearY")])

