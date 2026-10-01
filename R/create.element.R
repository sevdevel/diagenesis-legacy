`create.element` <-
function (geometry = c("1D", "2D"), name) 
{
    if (geometry == "1D") 
        attributes <- elt.1D.att
    if (geometry == "2D") 
        attributes <- elt.2D.att
    elt.obj <- as.list(rep(0, length = length(attributes)))
    names(elt.obj) <- attributes
    elt.obj$name <- name
    elt.obj$geometry <- geometry[1]
    class(elt.obj) <- "element"
    return(elt.obj)
}
