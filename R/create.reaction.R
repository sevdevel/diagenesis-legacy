`create.reaction` <-
function (geometry = c("1D", "2D"), name, type = c("kinetic", 
    "equilibrium")) 
{
    if (geometry == "1D") 
        attributes <- reac.1D.att
    if (geometry == "2D") 
        attributes <- reac.2D.att
    reac.obj <- as.list(rep(0, length = length(attributes)))
    names(reac.obj) <- attributes
    reac.obj$name <- name
    reac.obj$geometry <- geometry[1]
    reac.obj$type <- type[1]
    class(reac.obj) <- "reaction"
    return(reac.obj)
}
