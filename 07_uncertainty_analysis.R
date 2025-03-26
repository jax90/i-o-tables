 x = c('here','ggthemes')

lapply(x,library,character.only = T)

lapply(lapply(c('utils.R'),here),source)


mc_sensitivity_analysis = function(values_agg,
                                   emissions,
                                   time_period = end_year,
                                   exdir = main_path,
                                   n,
                                   basis = end_year,
                                   sim = c('emissions','interactions'),
                                   verbose = T)
{


  if(verbose) print("Prepare MRIOT and emissions' stressor...")

  input_output =
    values_agg %>%
    filter(time_period == time_period) |>
    select(!time_period) %>%
    filter(rowLabels %in% colnames(.)) %>% #rm W2 components (essentially value added and TLS)
    column_to_rownames('rowLabels')

  E = emissions %>%
    filter(time_period == !!time_period) %>%
   # separate(resource_id,c("country","industry"),sep = "_",extra = 'merge',remove = F) %>%
    filter(industry != "HH") %>%
    arrange(country,industry)



    #create a folder to store the associated analysis

    subfolder = paste0(main_path,"/results/Wilting")

    if(!dir.exists(subfolder)) dir.create(subfolder,showWarnings = F,recursive = T)

    if(verbose) print("Initialisation of variables")

    X = matrix(rowSums(input_output,na.rm = T),ncol = 1)

    Z =
      input_output %>%
      select(all_of(rownames(.))) %>%
      as.matrix()

    diag(Z)[diag(Z) == colSums(Z,na.rm = T)] = 0

    A = sweep(Z,2,as.numeric(X),`/`) ; A[is.nan(A) | is.infinite(A)] = 0

    Ap = A

    selected_industry_num = which(grepl("26|61|62|63",colnames(A)))

    selected_industry = colnames(A)[selected_industry_num]

    draws_E = lapply(E$direct_emissions,function(x) sample(x = seq(.9*x,1.1*x,x/1000),size = n,replace = T)) #uniform perturbation

    for(x in sim) #Perform the considered uncertainty analysis
    {
      if(x == 'emissions') Lp = solve(diag(nrow = nrow(Ap)) - Ap) #no need to re-run inversion if we only draw emissions

      sto_results = c()

      for(i in 1:n)
      {

        if(verbose) print("Uniform draws...")

        if(x == 'interactions')
        {
          for(j in 1:length(Ap))
          {
            Ap[j] = sample(x = seq(.9*A[j],1.1*A[j],A[j]/1000),size = 1) #uniform perturbation
          }
        }

        if(x == 'emissions')
        {
          Ep = matrix(lapply(draws_E, `[[`, i),ncol = 1)
        }else{Ep = matrix(E$direct_emissions,ncol=1)}

        cp = matrix(unlist(Ep) / unlist(X),ncol = 1) %>% `row.names<-`(E$resource_id) ; cp[is.nan(unlist(cp))] = 0

        if(verbose & x == "interactions") print("Inversing Leontief matrix...")

        if(x == "interactions") Lp = solve(diag(nrow = nrow(Ap)) - Ap) #no need to re-run inversion if we only draw emissions

        if(verbose) print("Computing the production footprint...")

        sub_xp = X[selected_industry_num,1,drop = F] %>%
          `rownames<-`(selected_industry)

        ###METHOD 2

        sub_invLp = solve(Lp[selected_industry_num,selected_industry_num])

        prescaling_sub_prod = sub_invLp %*% sub_xp

        full_prodp = Lp[,selected_industry_num] %*% prescaling_sub_prod

        ####COMPUTE THE CORRESPONDING FOOTPRINT####

        fpt = t(cp) %*% full_prodp

        ####FORMAT AND RETURN RESULTS####


        formatted_results = full_prodp %>%
          as.data.frame() %>%
          `rownames<-`(colnames(A)) %>%
          `colnames<-`("Total Requirements") %>%
          mutate(`Initial Production` = case_when(rownames(.) %in% selected_industry ~ sub_xp[match(rownames(.),rownames(sub_xp))],
                                                  T ~ 0),
                 `Additional intermediate requirements` = `Total Requirements` - `Initial Production`,
                 `Impact intensity per unit of output` = cp,
                 `Distributed footprint` = `Impact intensity per unit of output` * `Total Requirements`) %>%
          select(`Initial Production`,`Additional intermediate requirements`,`Total Requirements`,`Impact intensity per unit of output`,`Distributed footprint`) %>%
          rownames_to_column("industry")

        TOTAL_results = data.frame(industry = "TOTAL",
                                   t(colSums(formatted_results[,-1],na.rm = T)),
                                   check.names = F) %>%
          select(!c(`Impact intensity per unit of output`))

        write.table(TOTAL_results,paste0(subfolder,"/mc_results",x,".csv"),sep = ";",append = T,
                    col.names=!file.exists(paste0(subfolder,"/mc_results",x,".csv")))

        if(verbose) print(paste0(i,"     /     ",n))



    }

  }



}

mc_sensitivity_analysis(values_agg,
                        emissions,
                        time_period = 2021,
                        sim = 'emissions',
                        n = 1000)


mc_sensitivity_analysis(values_agg,
                        emissions,
                        time_period = 2021,
                        sim = 'interactions',
                        n = 1000)
#
# mc_sensitivity_analysis(values_agg,
#                         emissions,
#                         time_period = 2021,
#                         rule = 'price',
#                         sim = 'interactions')

###APPENDIX : FIGURES ON UNCERTAINTY

#original_fpt = 2140567

original_fpt <- read_parquet(paste0(main_path,"/results/emissions_over_time")) |>
  filter(time_period== end_year) |>
  pull(embodied_total)


dt = read.csv(paste0(main_path,"/results/Wilting/mc_resultsinteractions.csv"),sep = ";",header = T,row.names = NULL,check.names = F) %>%
  filter(!is.na(`Distributed footprint`)) %>%
  mutate(Simulation = 'MRIOT') %>%
  rbind(
    read.csv(paste0(main_path,"/results/Wilting/mc_resultsemissions.csv"),sep = ";",header = T,row.names = NULL,check.names = F) %>%
      filter(!is.na(`Distributed footprint`)) %>%
      mutate(Simulation = 'Emissions')
  ) %>%
  mutate(paper = 'Wilting (2012)') %>%
  group_by(Simulation,paper) %>%
  mutate(mean_sim = mean(`Distributed footprint`),
         p5 = quantile(`Distributed footprint`,probs = .05),
         p95 = quantile(`Distributed footprint`,probs = .95),
         q5 = quantile(`Distributed footprint`,probs = .5),
         mean = mean(`Distributed footprint`,na.rm = T),
         n = n(),
         margin_error = 1.96*sd(`Distributed footprint`)/sqrt(n()),
         original_fpt = original_fpt) %>%
  ungroup()





options(scipen = 999)

scale = 1000

paper = 'Wilting (2012)'

ggplot(dt %>% mutate(`Distributed footprint` = `Distributed footprint` / scale) %>% filter(paper == !!paper),
       aes(x = `Distributed footprint`,colour = "black")) + geom_density(linewidth = .75,color = 'black') +
  theme_bw(base_family = 'serif', base_size = 11) + #tufte source code
  theme(
    legend.background = element_blank(),
    legend.key = element_blank(),
    panel.background = element_blank(),
    #panel.border = element_blank(),
    strip.background = element_blank(),
    plot.background = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank()
  ) +
  geom_vline(aes(xintercept=p5/scale,
             colour="perc. 5 and 95"), linetype="dashed", linewidth=1) +
  geom_vline(aes(xintercept=p95/scale,
             colour="perc. 5 and 95"), linetype="dashed", linewidth=1) +
    geom_vline(aes(xintercept=q5/scale,
               colour="median"), linetype="2121", linewidth=1) +
  geom_vline(aes(xintercept=mean_sim/scale,
             colour="mean"), linetype ="1212", linewidth=1) +
  geom_vline(aes(xintercept=original_fpt/scale,
             colour="actual"), linewidth=1) +
  # geom_rect(aes(xmin = (mean - margin_error)/scale, xmax = (mean + margin_error)/scale, ymin = -Inf, ymax = Inf),alpha = .005,show.legend = F,color = 'lightgrey')+

  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        legend.title=element_blank(),
        legend.position="bottom",
        legend.box = "horizontal",
        strip.text = element_text(size=20),
        axis.text.x = element_text(size=20),
        axis.title.x = element_text(size=20),
        legend.text = element_text(size = 18)) +
  scale_colour_manual(name = 'colour',
                      values =c('perc. 5 and 95'='red','mean'='green','median' = 'cyan','actual' = 'black'))  +
  #xlim(c(2000000 / scale, 2250000 / scale))+
  xlim(c(2100000 / scale, 2200000 / scale))+
  facet_wrap(~Simulation,scales = 'free_x') +
  labs(x = 'total embodied emissions (in mt CO2e)')


