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

# Dataset
data = c(7,   32,   38,   58,   79,  108,  125,  199,  209 , 213,  227,  244,  269,  271,  272,  285,  288,  299,  324,  354,  364,  372,  379,  434,  455,  508,  523,  574,  613,  694,  756,  772,  772,  908,  909,  937, 1065, 1088, 1216, 1228, 1315, 1343, 1400, 1732, 1770, 2181, 2442, 2723, 2819, 3807)
p_part = 4

dataset = list(
  data = sort(data, decreasing = T),
  n_data = length(data),
  p_part = p_part,
  n_bit = ceiling(log2(p_part)),
  mu = sum(data)/p_part
)


# Parameters
parameters = list(
  ## Global parameters
  n_generation = 1000,
  n_popu = 38,
  ## Genetics parameters
  prob_muta = 0.00566,
  prob_muta_inc = 0.0072,
  n_cross = 2,
  p_fusion = .2529,
  type_fusion = 0, #0: every-site crossing; 1: multi-site crossing
  ## Hybridation parameters
  h_period = 1,
  p_type1 = 1.105,
  p_decrease_ratio = 0.8,
  n_expe = 50,
  ## Algorithme topology
  use_genetic = T,
  use_hybridation = T
)


# Not necessary, but its always better to protect the parameters
dataset <- as.environment(dataset)
parameters <- as.environment(parameters)
lockEnvironment(dataset, bindings = TRUE)
lockEnvironment(parameters, bindings = TRUE)

# Remove temporay variables
rm(data)
rm(p_part)


# Debug
DEBUG_LEVEL = DBG$LOG

