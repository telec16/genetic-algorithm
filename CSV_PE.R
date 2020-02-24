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
PE = read.csv2("PE1.csv")


# Parameters
## Genetics parameters
n_generation = 500
n_popu_f = setNames(lm(y ~ x, data = list(x = c(-1,1), y = c(10,50)))$coefficients, c("b","a"))
prob_muta_f = setNames(lm(y ~ x, data = list(x = c(-1,1), y = c(.01, .05)))$coefficients, c("b","a"))
prob_muta_inc = 0.01
type_fusion = 1
p_fusion_f = setNames(lm(y ~ x, data = list(x = c(-1,1), y = c(.05, .5)))$coefficients, c("b","a"))
n_cross_f = setNames(lm(y ~ x, data = list(x = c(-1,1), y = c(3, 7)))$coefficients, c("b","a"))
## Hybridation parameters
h_period_f = setNames(lm(y ~ x, data = list(x = c(-1,1), y = c(1, 10)))$coefficients, c("b","a"))
p_type1_f = setNames(lm(y ~ x, data = list(x = c(-1,1), y = c(0.2, 1)))$coefficients, c("b","a"))
p_decrease_ratio_f = setNames(lm(y ~ x, data = list(x = c(-1,1), y = c(.8, .999)))$coefficients, c("b","a"))
n_expe_f = setNames(lm(y ~ x, data = list(x = c(-1,1), y = c(50, 200)))$coefficients, c("b","a"))


# Debug
DEBUG_LEVEL = DBG$WARNING

workenv = new.env()
N = nrow(PE)

for (i in 1:N) {
  cat(sprintf("Doing %d/%d...\n", i, N))

  p = list(
    ## Genetics parameters
    n_generation = n_generation,
    n_popu = round(n_popu_f["a"] * PE[i,]$n_popu + n_popu_f["b"]),
    prob_muta = prob_muta_f["a"] * PE[i,]$prob_muta + prob_muta_f["b"],
    prob_muta_inc = prob_muta_inc,
    type_fusion = type_fusion,
    p_fusion = p_fusion_f["a"] * PE[i,]$fusion_param + p_fusion_f["b"],
    n_cross = round(n_cross_f["a"] * PE[i,]$fusion_param + n_cross_f["b"]),
    ## Hybridation parameters
    h_period = round(h_period_f["a"] * PE[i,]$h_period + h_period_f["b"]),
    p_type1 = p_type1_f["a"] * PE[i,]$p_type1 + p_type1_f["b"],
    p_decrease_ratio = p_decrease_ratio_f["a"] * PE[i,]$p_decrease_ratio + p_decrease_ratio_f["b"],
    n_expe = round(n_expe_f["a"] * PE[i,]$n_expe + n_expe_f["b"])
  )

  AG(dataset, workenv, p)

  PE[i,]$score = workenv$score
  PE[i,]$iteration = workenv$iteration
}

# Save the results
cat("Done. Saving...\n")
write.csv2(PE,format(Sys.time(), "Results of %d-%m-%y_%H%M%S.csv"))


