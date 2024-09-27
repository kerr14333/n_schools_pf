.output {
max-height: 500px;
overflow-y: scroll;
}

.cell-output-stdout {
  overflow-y: scroll;
  max-height: 500px;
}


## -----------------------------------------------------------------------------
#| echo: true
#| message: false
#| warning: false

library(cmdstanr)
library(jsonlite)
library(tidyverse)
library(glue)


## -----------------------------------------------------------------------------
#| echo: true
#| message: false

#read in data
stan_data <- read_json( "eight_schools.json", simplifyVector=T)
print(stan_data)


## -----------------------------------------------------------------------------
#| class-output: stan
#| echo: false
cat(readLines("eight_schools_noncentered.stan"), sep = "\n")


## -----------------------------------------------------------------------------
#| echo: true
#| warning: false

#### Run eight schools non-centered version
mod_eight    <- cmdstanr::cmdstan_model( "eight_schools_noncentered.stan",
                                        cpp_options=list(stan_threads=TRUE)) 

fit_eight <- mod_eight$pathfinder( data = stan_data,
                       refresh = 8,
                       draws=2000,
                       num_threads = 8,
                       num_paths = 20,
                       psis_resample = T 
                       )




## -----------------------------------------------------------------------------
#| echo: true
#| warning: false

eight_df <- fit_eight$draws(format="df")

#get number of distinct rows from output
d_rows <- eight_df %>% select(-.chain, -.iteration, -.draw) %>% distinct() %>% nrow()

#get total number of rows
rows <- nrow(eight_df)



## -----------------------------------------------------------------------------
#| class-output: stan
#| echo: false
cat(readLines("eight_schools_noncentered_gvf.stan"), sep = "\n")


## -----------------------------------------------------------------------------
#| echo: true
#| warning: false

#### Run eight schools non-centered version
suppressWarnings(mod1     <- cmdstanr::cmdstan_model( "eight_schools_noncentered_gvf.stan",
                                                    cpp_options=list(stan_threads=TRUE) ) )

#Fit our new model to 8-schools with HMC 
fit_samp = mod1$sample( data = stan_data, 
                         chains=3, 
                         threads_per_chain=1,
                         refresh=300,
                         adapt_delta = .99 )



## -----------------------------------------------------------------------------
mydraws <- fit_samp$draws(format="df")

library(invgamma)

mu_bar <- mydraws$mu   %>% mean()
tausq_bar <- mydraws$tau_sq %>% mean()
phi_bar <- mydraws$phi %>% mean()


cat("****Parameters Estimated From Model****","\n",
    "mu_bar:",mu_bar,"\n",
    "tausq_bar:", tausq_bar,"\n",
    "phi_bar:", phi_bar)


## -----------------------------------------------------------------------------
#| eval: false
#| echo: true

N <- x
sigma <- rinvgamma(N, 2, scale = phi_bar) 
theta <- rnorm(N, mu_bar, tausq_bar )
y     <- rnorm(theta,phi_bar)



## -----------------------------------------------------------------------------
#| eval: false
#| echo: true


#run pathfinder
fit_pf = mod_eight$pathfinder( data =  temp[[k]][['stan_data_gen']],
                   draws=3000,
                   num_threads = 12,
                   num_paths = 20,
                   psis_resample = T)  
        


## -----------------------------------------------------------------------------
#| echo: true
#| warning: false

library(parallel)


#status file just helps us keep track of progress
file.remove("status.txt")
#N_tot is a sequence of number of schools 
#we will generate
N_tot <- seq(from=8, to=150, by=2)

#for each number of 
n_times <- 10


#function I will use in an lapply
myloop <- function(x){
    
    #store the n_times simulations
    temp <- list()
    
    #begin simulating n_times
    for(k in 1:n_times){
        
        #this is simply here for me to keep track of the simulation
        write_lines( x = paste0("Number of Schools: ",x," Simulation Number: ", k),
                    file= "status.txt",
                    append=T )
        
        #create a holder for this 
        #simulations results
        temp[[k]] <- list()       
        
        #Generate New Data
        N <- x
        sigma <- rinvgamma(N, 2, scale = phi_bar) 
        theta <- rnorm(N, mu_bar, tausq_bar )
        y     <- rnorm(theta,phi_bar)

        #store the data we will use
        temp[[k]][['stan_data_gen']] <- list(J=N,y=y,sigma=sigma)
        
        #run pathfinder
        fit_pf = mod_eight$pathfinder( data =  temp[[k]][['stan_data_gen']],
                           draws=3000,
                           refresh=0,
                           num_threads = 4,
                           num_paths = 20,
                           psis_resample = T)  
        
        #store the model
        temp[[k]][['model']] <- fit_pf
        temp[[k]][['draws']] <- fit_pf$draws(format="df")
    }
    
    return( temp )
}

#run simulation
#results <- lapply(N_tot,myloop)  #I use this on Windows
results <- mclapply(N_tot,myloop,mc.preschedule=F,mc.cores=40)



## -----------------------------------------------------------------------------
#| echo: true
#| warning: false

 #number of schools in each simulation
 nschools  <- results %>% map( ~map_int( .x, ~pluck(.x,"stan_data_gen","J") )) %>% list_c()

 #return number of unique
 getdistinctdraws <- . %>% 
                      select(-.chain, -.iteration, -.draw) %>% 
                      distinct() %>% 
                      nrow()
 
 #get number of distinct samples in each simulation
 uniquedraws <- results %>% 
             map( ~map_int( .x, ~( pluck(.x,"draws") %>% getdistinctdraws) )) %>% 
             list_c()

 df_summary <- tibble(nschools = as.factor(nschools), 
                      n_unique_draws = uniquedraws)


## -----------------------------------------------------------------------------
#| echo: true
#| out-width: 90%

library(scales)

df_summary %>% ggplot(aes(x=nschools,y=n_unique_draws)) + 
               geom_boxplot() + 
               scale_x_discrete(breaks=c(seq(from=8,to=150,by=6),150)) +
               labs(title="Number of Unique Rows Out of 3000 Draws \nby Number of Schools in Simulation",
                    y = "Number of Unique Draws", 
                    x="Number of Schools in Simulation") + 
               theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1,size=10))



## -----------------------------------------------------------------------------
#| include: false

#This is just here to automate the dump of the code to an R script
knitr::purl('n_schools_pf.qmd')

