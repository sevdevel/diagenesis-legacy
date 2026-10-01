`create.species` <-
function (geometry = c("1D", "2D"), name, phase = c("solute", 
    "solid", "bulk"), type = c("chemical", "composite", "artificial"), 
    z = 0) 
{
    if (geometry == "1D") 
        attributes <- spec.1D.att
    if (geometry == "2D") 
        attributes <- spec.2D.att
    spec.obj <- as.list(rep(0, length = length(attributes)))
    names(spec.obj) <- attributes
    spec.obj$name <- name
    spec.obj$geometry <- geometry[1]
    spec.obj$phase <- phase[1]
    spec.obj$type <- type[1]
    spec.obj$z <- z
    class(spec.obj) <- "species"
    return(spec.obj)
}
