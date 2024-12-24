library(tidyverse)
library(tibble)
library(matlib)
library(leontief)
library(corpcor)


# Read data files
leontief_weights <- generate_leontief(exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",,
                                      update = F)

emissions <- format_emissions(exdir = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23",
                              update = F) %>%
  unite(resource_id, ref_area,industry, sep = "_") |>
  group_by(resource_id,time_period) %>%
  summarise(direct_emissions = sum(obs_value,na.rm = T)) %>%
  mutate(time_period = as.character(time_period)) %>%
  ungroup()


add_embodied_emissions <- function(leontief_weights,
                                   emissions)
{

  # Merge data and calculate emissions per output
  df <- emissions |>
    full_join(leontief_weights, by = c("resource_id", "time_period")) |>
    mutate(emissions_per_output = case_when(total_output == 0 ~ 0,
                                            T ~ direct_emissions / total_output)) |>
    filter(!(if_any(everything(), ~ is.na(.x) & direct_emissions == 0))) %>%
    filter(!grepl('HH',resource_id))


  #generate a data frame that allows to consider embodied emissions with respect
  # different kinds of demand


  y_country <- df |>
    select(
      contains("P3_S13"),  # Y
      contains("P3_S14"),  # Y
      contains("P3_S15"),  # Y
      contains("P51G"),    # Y
      contains("P5M"),     # Y
      time_period, resource_id
    ) %>%
    pivot_longer(
      cols= grep('P5|P3',colnames(x=.)),
      names_to= "sto"
    )

  for(time_period in unique(df$time_period))
  {

    dt = df %>%
      filter(time_period == !!time_period) %>%
      select(!time_period) %>%
      filter(!grepl('HH',resource_id))

    B = matrix(dt$emissions_per_output)

    L <- dt |>
      select(-c(contains('P3'),contains('P5'),contains('emissions'),final_demand,total_output)) %>%
      filter(resource_id %in% colnames(.)) %>%
      column_to_rownames('resource_id') %>%
      as.matrix()

    ## modify the Leontief inverse to consider demand in other industries

    Int <- matrix(0, nrow = dim(L)[1], ncol = dim(L)[1])

    # initialize L adjusted
    L_adjust <- L

    #now the actual adjustment starts:

    for (i in grep('26|61|62|63',colnames(x=L)))
    {

      #equation 8 / equation 11
      Int <- Int + (1 / L_adjust[i, i] *  L_adjust[, i]  %*%   t(L_adjust[i, ])   )

      # equation 9
      L_adjust  <- L -  Int

    }

    rm(L_adjust)

    # Calculate embodied emissions
    # iterate of different types of countries
    country_list = df %>%
      select(resource_id) %>%
      unique() %>%
      separate(resource_id,c('country','industry'),sep = "_",extra = 'merge') %>%
      select(country) %>%
      unlist() %>%
      unique()

    #iterate over different types of demand

    count = 0

    for (demand in unique(y_country$sto)){

        # pick the right demand type y
        y = y_country %>%
          filter(sto == demand & time_period == !!time_period) %>%
          separate(resource_id,c('country','industry'),sep = "_",extra = 'merge') %>%
          arrange(country,industry) %>%
          {diag(x = .$value)}

        # Calculate embodied emissions
        embodied_emissions <- t(B) %*% Int %*% y

        # prepare emboded emisssions for merge

        add = data.frame(resource_id  = dt$resource_id,
                         time_period = as.character(time_period)) %>%
          mutate(!!paste0("embodied_emissions_", demand) := embodied_emissions[1,])
        # Add embodied emissions to the dataframe
        df = df %>% left_join(y = add,
                              by = c('time_period','resource_id'))

        if(verbose)
        {
        count = count + 1

        if(count %% 5 == 0) print(paste0(count,"    /    ",length(unique(y_country$sto))))
        }
    }

    Int = Int %>%
      as.data.frame() %>%
      `row.names<-`(colnames(.)) %>%
      `colnames<-`(paste0('Int_wght_',colnames(.))) %>%
      rownames_to_column('resource_id') %>%
      mutate(time_period = as.character(time_period))

    df = df %>% left_join(y = Int,
                          by = c('time_period','resource_id'))


    rm(Int)

    # add Scope 2 & 3 emissions (calculate Scope 3 emissions by the productions
    # perspective (see Charpentier & Blain 2024))

    embodied_emissions = sweep(sweep(sweep(L,2,diag(L),`/`),1,as.numeric(B),`*`),2,dt$total_output,`*`)

    rm(L)

    indirect_impacts = embodied_emissions - diag(x = as.numeric(B) * dt$total_output)

    production_footprint <- tibble(
      industry_ref_area = rownames(embodied_emissions),
      production_footprint= colSums(embodied_emissions,na.rm = T),
      correction_factor = diag(L)) |>
      mutate(scope2 = colSums(indirect_impacts[grepl('D35',rownames(indirect_impacts)),],na.rm = T),
             scope3 = colSums(indirect_impacts,na.rm = T) - scope2,
             time_period = time_period)

    df <- df %>%
      full_join(production_footprint, by = c("industry_ref_area"))

  }

  return(df)
}


add_embodied_emissions(leontief_weights,emissions)
