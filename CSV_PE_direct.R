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
PE = read.csv2("PEgenhyb.csv")


# Debug
DEBUG_LEVEL = DBG$WARNING

workenv = new.env()
N = nrow(PE)

for (i in 1:N) {
  cat(sprintf("Doing %d/%d...\n", i, N))

  p = as.list(parameters)
  ## Genetics parameters
  p$n_popu = round(PE[i,]$n_popu)
  p$prob_muta = PE[i,]$prob_muta
  # p$prob_muta_inc = PE[i,]$prob_muta_inc
  p$p_fusion = PE[i,]$fusion_param
  p$n_cross = round(PE[i,]$fusion_param)
  ## Hybridation parameters
  p$h_period = round(PE[i,]$h_period)
  p$p_type1 = PE[i,]$p_type1
  # p$p_decrease_ratio = PE[i,]$p_decrease_ratio
  #p$n_expe = round(PE[i,]$n_expe)

  AG(dataset, workenv, p)

  PE[i,]$score = workenv$score
  PE[i,]$iteration = workenv$iteration
  PE[i,]$time = workenv$time
}

# Save the results
cat("Done. Saving...\n")
write.csv2(PE,format(Sys.time(), "Results of %d-%m-%y_%H%M%S.csv"))


