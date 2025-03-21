x = c('here','ggthemes','foreach','doFuture','purrr','parallel','progressr','doRNG','doFuture')

lapply(x,library,character.only = T)

source(here('utils.R'))
source(here('01_aggregate_FIGARO_files.R'))
source(here('02_deflation_procedure.R'))

mc_sensitivity_analysis = function(values_agg,
                                   emissions,
                                   time_period = end_year,
                                   exdir = main_path,
                                   n,
                                   basis = end_year,
                                   sim = c('emissions','interactions'),
                                   n_cores = detectCores() - 3,
                                   verbose = T)
{

  if(verbose) print("Prepare MRIOT and emissions' stressor...")

  input_output =
    values_agg %>%
    filter(time_period == !!time_period) |>
    select(!time_period) %>%
    filter(rowLabels %in% colnames(.)) %>% #rm W2 components (essentially value added and TLS)
    column_to_rownames('rowLabels')

  E = emissions %>%
    separate(resource_id,c("country","industry"),sep = "_",extra = 'merge',remove = F) %>%
    filter(time_period == !!time_period) %>%
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

  Ep = matrix(E$direct_emissions,ncol=1)

  cores = min(n_cores,detectCores()-1) #not to overload your computer

  if(verbose) print(paste0("Parallelization procedure with ",cores," cores"))


  for(s in sim)
  {

  if(verbose) print(paste0("Running ",s))

  local({

  if(verbose) p = progressor(along = 1:n)


  plan(multisession, workers = cores)

  handlers(global = TRUE)

  add <- foreach(
    i = 1:n,
    .combine = 'rbind',
    .inorder = FALSE,
    .export = c('X', 'E', 'A', 's','n','verbose', 'perform_uncertainty', 'selected_industry_num', 'selected_industry', 'subfolder'),
    .packages = c('dplyr', 'progressr')) %dorng% {

      if(s == 'emissions')
      {
        draws_E = lapply(E$direct_emissions,function(x) sample(x = seq(.9*x,1.1*x,x/1000),size = n,replace = T)) #uniform perturbation

        E = lapply(draws_E, `[[`, i)
      }


      TOTAL_results <- perform_uncertainty(X = X, E = E, A = A, sim = s, verbose = F)

      write.table(TOTAL_results,paste0(subfolder,"/mc_results",s,".csv"),sep = ";",append = T,
                  col.names=!file.exists(paste0(subfolder,"/mc_results",s,".csv")))


      if(verbose) p(sprintf("i=%g", i))
    }
  })

      return(TOTAL_results)
    }


}


perform_uncertainty = function(X,E,A,sim,verbose = T)
{

  x = sim

  if(x == 'emissions')
  {
    Ap = A

    Lp = solve(diag(nrow = nrow(Ap)) - Ap) #no need to re-run inversion if we only draw emissions

    Ep = matrix(E,ncol = 1)
  }

  if(verbose) print("Uniform draws...")

  if(x == 'interactions')
  {
    Ap = A * sample(seq(from = .9,to = 1.1,by = .001),size = 2944^2,replace = T)

    Ep = matrix(E$direct_emissions,ncol=1)
  }

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

  return(TOTAL_results)


}

main_path = "C:/Users/Joris/OneDrive - La Société Nouvelle/Partage/FIGARO ed23"
user = "Joris"
values_agg = format_iot(folder = if(user =="jax"){paste0(main_path, "/data/values")}else{main_path},
                        exdir = if(user =="jax"){paste0(main_path, "/data/values")}else{main_path},
                        update = F,
                        edition = '23')


emissions <- format_emissions(folder = if(user =="jax"){paste0(main_path, "/data/emissions")}else{main_path},
                              exdir =if(user =="jax"){paste0(main_path, "/data/emissions")}else{main_path},
                              update = F,
                              edition = '23') %>%
  unite(resource_id, ref_area,industry, sep = "_") |>
  group_by(resource_id,time_period) %>%
  summarise(direct_emissions = sum(obs_value,na.rm = T)) %>%
  mutate(time_period = as.character(time_period)) %>%
  ungroup() %>%
  group_by(time_period) %>%
  mutate(absolute_emissions = sum(direct_emissions ,na.rm = T)) %>%
  ungroup()


mc_sensitivity_analysis(values_agg,
                        emissions,
                        time_period = 2021,
                        sim = 'emissions',
                        n = 10)


mc_sensitivity_analysis(values_agg,
                        emissions,
                        time_period = 2021,
                        sim = 'interactions',
                        n = 1000)

###APPENDIX : FIGURES ON UNCERTAINTY

original_fpt = 2140567

dt = readRDS(here("uncertainty_wilting.rds")) %>%
  group_by(Simulation,paper) %>%
  mutate(num = 1:n()) %>%
  filter(num <= 1000) %>%
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
        axis.text.x = element_text(size=16),
        axis.title.x = element_text(size=20),
        legend.text = element_text(size = 18)) +
  scale_colour_manual(name = 'colour',
                      values =c('perc. 5 and 95'='red','mean'='green','median' = 'cyan','actual' = 'black'))  +
  #xlim(c(2000000 / scale, 2250000 / scale))+
  xlim(c(2050000 / scale, 2250000 / scale))+
  facet_wrap(~Simulation,scales = 'free_x') +
  labs(x = 'total embodied emissions (in mt CO2e)')
