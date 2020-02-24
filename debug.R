#########################################################################
# k-patition solver (c) by Loup Plantevin and Clementine Delaunay       #
#                                                                       #
# k-patition solver is licensed under a                                 #
# Creative Commons Attribution-ShareAlike 3.0 Unported License.         #
#                                                                       #
# You should have received a copy of the license along with this        #
# work.  If not, see <http://creativecommons.org/licenses/by-sa/3.0/>.  #
#########################################################################


if(!exists("DBG")){
  # https://stackoverflow.com/questions/33838392/enum-like-arguments-in-r
  Enum <- function(...) {
    
    values <- sapply(match.call(expand.dots = TRUE)[-1L], deparse)
    
    stopifnot(identical(unique(values), values))
    
    res <- setNames(seq_along(values), values)
    res <- as.environment(as.list(res))
    lockEnvironment(res, bindings = TRUE)
    res
  }
  
  debug.ok = function(lvl){
    return(lvl <= DEBUG_LEVEL)
  }
  
  print.d = function(n, v, lvl){
    if(debug.ok(lvl)){
      print(n)
      print(v)
    }
  }

  DBG = Enum(ERROR, WARNING, LOG, VERBOSE, CLAIRE)
}

