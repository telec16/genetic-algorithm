#########################################################################
# k-patition solver (c) by Loup Plantevin and Clementine Delaunay       #
#                                                                       #
# k-patition solver is licensed under a                                 #
# Creative Commons Attribution-ShareAlike 3.0 Unported License.         #
#                                                                       #
# You should have received a copy of the license along with this        #
# work.  If not, see <http://creativecommons.org/licenses/by-sa/3.0/>.  #
#########################################################################


source("debug.R")
source("parameters.R")

source("Algo_Genetique.R")

# Design of experiments parameters
levels = c(-1, 1)
get_index = function(level){
  return((level + 1) / 2 + 1)
}
do_experiment = function(parameters){
  return(T)
}

# Parameters
## Genetics parameters
n_generation = 500
n_popu_l = c(10,50)
prob_muta_l = c(0.001, .05)
prob_muta_inc = 0.01
type_fusion = 0
p_fusion_l = c(.05, .5)
n_cross_l = c(2, 4)
## Hybridation parameters
h_period_l = c(1, 10)
p_type1_l = c(0.2, 1)
p_decrease_ratio_l = c(.8, 0.999)
n_expe_l = c(50, 200)


# Debug
DEBUG_LEVEL = DBG$WARNING

workenv = new.env()
i = 0
N = 2**6

df = data.frame(n_popu = n_popu_l,
                prob_muta = prob_muta_l,
                p_fusion = p_fusion_l,
                p_type1 = p_type1_l,
                p_decrease_ratio = p_decrease_ratio_l,
                n_expe = n_expe_l,
                iteration=NA, score=NA)

for(n_popu in levels){
  for(prob_muta in levels){
    for(p_fusion in levels){
      for(p_type1 in levels){
        for(p_decrease_ratio in levels){
          for(n_expe in levels){

            i = i+1
            cat(sprintf("Doing %d/%d...\n", i, N))

            p_norm = list(
              ## Genetics parameters
              n_popu = n_popu,
              prob_muta = prob_muta,
              p_fusion = p_fusion,
              ## Hybridation parameters
              p_type1 = p_type1,
              p_decrease_ratio = p_decrease_ratio,
              n_expe = n_expe
            )

            p = list(
              ## Genetics parameters
              n_generation = 100,
              n_popu = n_popu_l[get_index(n_popu)],
              prob_muta = prob_muta_l[get_index(prob_muta)],
              prob_muta_inc = 0.01,
              p_fusion = p_fusion_l[get_index(p_fusion)],
              ## Hybridation parameters
              p_type1 = p_type1_l[get_index(p_type1)],
              p_decrease_ratio = p_decrease_ratio_l[get_index(p_decrease_ratio)],
              n_expe = n_expe_l[get_index(n_expe)]
            )

            if(do_experiment(p_norm)){
              AG(dataset, workenv, p)
            }
            else{
              workenv$iteration = NA
              workenv$score = NA
            }

            cat("Done. Saving...\n")

            # Reaaally inefficient. Bad practice. But it should be okay (and it's way worse up there).
            df = rbind(df, data.frame(p_norm, iteration=workenv$iteration, score=workenv$score))

          }
        }
      }
    }
  }
}

# Save the results
write.csv2(df,format(Sys.time(), "Results of %d-%m-%y_%H%M%S.csv"), row.names = TRUE)


