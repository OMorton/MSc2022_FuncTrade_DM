
## global options settings
options(scipen = 999) 
#.libPaths("C:/Packages")

library(tidyverse)
library(mFD)

#### Read in data #### 

Reptiles_traits_names <- data.table::fread("Data/Dummy/SESfric/Reptiles_traits_names.csv")

Exp_country <- data.table::fread("Data/Dummy/SESfric/Exp_country_no_year_total.csv") %>% 
  ## get rid of spaces in sp names
  mutate(Taxon = gsub(" ", "_", Taxon))

## NOTE trait data missing species names? So I just added them back in alphabetically
## from the export data for this (don't do that normally)
row.names(Reptiles_traits_names) <-  unique(sort(Exp_country$Taxon))



#### Calculate functional space ####

#Calculating the functional space 
fspace <- mFD::tr.cont.fspace(sp_tr = Reptiles_traits_names, pca = TRUE, 
                              nb_dim = 4, scaling = "no_scale", compute_corr = "pearson")

## So as expected mAD goes down with axes
fspace$quality_metrics

## PC1 - 3 explain 95% of the variation
fspace$eigenvalues_percentage_var

## PC4 has very weak associations with any trait.
## we also have 4 traits so not sure it makes sense to have 4 axes
qual_fspace <- quality.fspaces(fspace$sp_dist_init)
plt <- mFD::traits.faxes.cor(
  sp_tr  = Reptiles_traits_names, 
  sp_faxes_coord = qual_fspace$details_fspaces$sp_pc_coord[ , c("PC1", "PC2", "PC3", "PC4")], 
  plot = TRUE)
plt

#extracting the species coordinates of the species in the matrix
species_coordinates <- qual_fspace$details_fspaces$sp_pc_coord %>% as.matrix()

#### Get species matrix ####
Occ_matrix <- Exp_country %>% 
  pivot_wider(names_from = Taxon, values_from = Total_volume, values_fill = 0) %>%
  tibble::column_to_rownames(var = "Exporter_Country") %>%
  ## keep countries with more species than axes
  filter(rowSums(.!=0) > 3) %>% as.matrix()


#### Calculating SES ####

## Calculate the "true" uncorrected/observed FD metrics.
## Just wrapped in system time to time the function so you can guess how
## long 100, 500 or 1000 will take.
system.time(observed_output <- alpha_fd_indices_no_year_imp_country <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = qual_fspace$details_fspaces$sp_pc_coord[ , c("PC1", "PC2", "PC3")],
  asb_sp_w         = Occ_matrix,
  ind_vect         = c("fric", "fdiv"),
  scaling          = FALSE,
  check_input      = TRUE,
  details_returned = TRUE))

observed_fd <- observed_output$functional_diversity_indices %>%
  rownames_to_column(var = "Exporter")

## Set how many simulated communities you want to run 
## in small studies 999 is a kind of unofficial sample for bigger things we've 
## dropped down to 200. More is better up to a point so if you can get 500 done in 
## an hour then go with that.
iter <- 10

## Make an empty object to store the data in
## it important that everytime you rerun the loop (e.g. after you've changed the 
## data to importer data) you rerun this to empty the old object ready to store the 
## new simulations 
Storage <- data.frame(matrix(NA, nrow = 0, ncol = 0))

## For loop to get simulated expected communities.
for (i in 1:iter) {
  ## Prints an update to the console with each iteration
  cat(i, " out of ", iter, " simulated communities ", " (",i/iter*100,"%)\n", sep = "")
  
  ## Randomizes the species order of the occurence matrix so you get random
  ## communities from each country
  random_matrix <- picante::randomizeMatrix(Occ_matrix, null.model = "richness")
  
  ## Calc fd
  expected_fd <- alpha.fd.multidim(
    sp_faxes_coord   = qual_fspace$details_fspaces$sp_pc_coord[ , c("PC1", "PC2", "PC3")],
    asb_sp_w         = random_matrix,
    ind_vect         = c("fric", "fdiv"),
    scaling          = FALSE, verbose = FALSE,
    check_input      = TRUE,
    details_returned = TRUE)
  
  ## tidying - add country column (exporter/importer/year etc) and add a collum
  ## that stores the simulation number
  metrics_iter <- expected_fd$functional_diversity_indices %>% 
    rownames_to_column(var = "Country") %>% mutate(sim = i)
  
  ## save to storage
  Storage <- rbind(Storage, metrics_iter)
}

## Summarise the simulated comms
expected_sum <- Storage %>% group_by(Country) %>% 
  summarise(ave_expected = mean(fric), sd_expected = sd(fric))

## calc the ses metric
sesfric_sum <- left_join(observed_fd, expected_sum, by = c("Exporter" = "Country")) %>%
  mutate(SES.fric = (fric - ave_expected)/sd_expected)
