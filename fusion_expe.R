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
n_generation = 100
n_popu = 10
prob_muta = 0.01
prob_muta_inc = 0.01
n_cross_l = c(1,2,4,5)
p_fusion_l = c(.1,.3,.5)
type_fusion_l = c(0,1) #0: every-site crossing; 1: multi-site crossing
## Hybridation parameters
p_type1 = 0.8
p_decrease_ratio = 0.9
n_expe = 100

# Debug
DEBUG_LEVEL = DBG$WARNING

workenv = new.env()
i = 0
n_rep = 10
N = 7 * n_rep

df = NULL

# df = data.frame(n_popu = n_popu_l,
#                 prob_muta_incr = prob_muta_inc,
#                 prob_muta = prob_muta,
#                 p_fusion = p_fusion_l,
#                 p_type1 = p_type1,
#                 n_cross = n_cross_l,
#                 type_fusion = type_fusion_l,
#                 p_decrease_ratio = p_decrease_ratio,
#                 n_expe = n_expe, 
#                 iteration=NA, score=NA)
for (rep in (1:n_rep)){
  type_fusion_i = 0;
  for(p_fusion_i in p_fusion_l){
    i = i+1
    
    p = list(
      ## Genetics parameters
      n_generation = n_generation,
      n_popu = n_popu,
      prob_muta = prob_muta,
      prob_muta_inc = prob_muta_inc,
      n_cross = NA,
      p_fusion = p_fusion_i,
      type_fusion = type_fusion_i, #0: every-site crossing; 1: multi-site crossing
      ## Hybridation parameters
      p_type1 = p_type1,
      p_decrease_ratio = p_decrease_ratio,
      n_expe = n_expe
    )
    
    cat(sprintf("Doing %d/%d...\n", i, N))
    
    AG(dataset, workenv, p)
    
    cat("Done. Saving...\n")
    
    # Reaaally inefficient. Bad practice. But it should be okay (and it's way worse up there).
    df = rbind(df, data.frame(p, iteration=workenv$iteration, score=workenv$score))
  }
  type_fusion_i = 1
  for(n_cross_i in n_cross_l){
    i = i+1
    
    p = list(
      ## Genetics parameters
      n_generation = n_generation,
      n_popu = n_popu,
      prob_muta = prob_muta,
      prob_muta_inc = prob_muta_inc,
      n_cross = n_cross_i,
      p_fusion = NA,
      type_fusion = type_fusion_i, #0: every-site crossing; 1: multi-site crossing
      ## Hybridation parameters
      p_type1 = p_type1,
      p_decrease_ratio = p_decrease_ratio,
      n_expe = n_expe
    )
    
    cat(sprintf("Doing %d/%d...\n", i, N))
    
    AG(dataset, workenv, p)
    
    cat("Done. Saving...\n")
    
    # Reaaally inefficient. Bad practice. But it should be okay (and it's way worse up there).
    df = rbind(df, data.frame(p, iteration=workenv$iteration, score=workenv$score))
  }
}


# Save the results
write.csv2(df,format(Sys.time(), "Results of %d-%m-%y_%H%M%S.csv"), row.names = TRUE)
