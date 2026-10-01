`plot.simulation.list` <-
function (x, what = c("species", "reactions", "elements"), plot.info, 
    ...) 
{
    if (what == "species") {
        plot.species.1D(x, plot.info)
    }
    if (what == "reactions") {
        plot.reactions.1D(x, plot.info)
    }
    if (what == "elements") {
    }
}
