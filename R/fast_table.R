fast_table <- function(x, size = 5L) {
       .Call("_lidR_fast_table", x, size)
     }
