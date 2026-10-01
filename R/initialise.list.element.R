`initialise.list.element` <-
function (elt.list, geometry = c("1D", "2D"), name) 
{
    N.elt <- length(elt.list) + 1
    elt.list[[N.elt]] <- create.element(geometry, name)
    names(elt.list)[N.elt] <- name
    return(elt.list)
}
