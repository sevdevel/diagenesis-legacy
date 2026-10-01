`updating.composite.species` <-
function (spec.table, SL) 
{
    for (i in 1:length(SL)) {
        if (SL[[i]]$type != "composite") 
            next
        spec.name <- names(SL)[i]
        ind <- which(rownames(spec.table) == spec.name)
        if (SL[[spec.name]]$geometry == "1D") {
            SL[[spec.name]]$C <- 0
            SL[[spec.name]]$tran <- 0
            SL[[spec.name]]$kin.reac <- 0
            SL[[spec.name]]$eq.reac <- 0
            SL[[spec.name]]$reac <- 0
            SL[[spec.name]]$irr <- 0
            SL[[spec.name]]$ddt <- 0
            SL[[spec.name]]$C.up <- 0
            SL[[spec.name]]$C.down <- 0
            SL[[spec.name]]$flux.up <- 0
            SL[[spec.name]]$flux.down <- 0
            SL[[spec.name]]$int.kin.reac <- 0
            SL[[spec.name]]$int.eq.reac <- 0
            SL[[spec.name]]$int.reac <- 0
            SL[[spec.name]]$int.irr <- 0
            SL[[spec.name]]$int.ddt <- 0
            SL[[spec.name]]$deficit <- 0
            for (j in 1:ncol(spec.table)) {
                SL[[spec.name]]$C <- SL[[spec.name]]$C + spec.table[ind, 
                  j] * SL[[names(spec.table)[j]]]$C
                SL[[spec.name]]$tran <- SL[[spec.name]]$tran + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$tran
                SL[[spec.name]]$eq.reac <- SL[[spec.name]]$eq.reac + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$eq.reac
                SL[[spec.name]]$kin.reac <- SL[[spec.name]]$kin.reac + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$kin.reac
                SL[[spec.name]]$reac <- SL[[spec.name]]$reac + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$reac
                SL[[spec.name]]$irr <- SL[[spec.name]]$irr + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$irr
                SL[[spec.name]]$ddt <- SL[[spec.name]]$ddt + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$ddt
                SL[[spec.name]]$C.up <- SL[[spec.name]]$C.up + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$C.up
                SL[[spec.name]]$C.down <- SL[[spec.name]]$C.down + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$C.down
                SL[[spec.name]]$flux.up <- SL[[spec.name]]$flux.up + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$flux.up
                SL[[spec.name]]$flux.down <- SL[[spec.name]]$flux.down + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$flux.down
                SL[[spec.name]]$adv.flux <- SL[[spec.name]]$adv.flux + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$adv.flux
                SL[[spec.name]]$dif.flux <- SL[[spec.name]]$dif.flux + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$dif.flux
                SL[[spec.name]]$flux <- SL[[spec.name]]$flux + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$flux
                SL[[spec.name]]$int.kin.reac <- SL[[spec.name]]$int.kin.reac + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$int.kin.reac
                SL[[spec.name]]$int.eq.reac <- SL[[spec.name]]$int.eq.reac + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$int.eq.reac
                SL[[spec.name]]$int.reac <- SL[[spec.name]]$int.reac + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$int.reac
                SL[[spec.name]]$int.irr <- SL[[spec.name]]$int.irr + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$int.irr
                SL[[spec.name]]$int.ddt <- SL[[spec.name]]$int.ddt + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$int.ddt
                SL[[spec.name]]$deficit <- SL[[spec.name]]$deficit + 
                  spec.table[ind, j] * SL[[names(spec.table)[j]]]$deficit
            }
        }
        if (SL[[spec.name]]$geometry == "2D") {
            SL[[spec.name]]$C <- 0
            SL[[spec.name]]$tran <- 0
            SL[[spec.name]]$kin.reac <- 0
            SL[[spec.name]]$eq.reac <- 0
            SL[[spec.name]]$reac <- 0
            SL[[spec.name]]$irr <- 0
            SL[[spec.name]]$ddt <- 0
            SL[[spec.name]]$C.x.up <- 0
            SL[[spec.name]]$C.x.down <- 0
            SL[[spec.name]]$flux.x.up <- 0
            SL[[spec.name]]$flux.x.down <- 0
            SL[[spec.name]]$C.y.up <- 0
            SL[[spec.name]]$C.y.down <- 0
            SL[[spec.name]]$flux.y.up <- 0
            SL[[spec.name]]$flux.y.down <- 0
            SL[[spec.name]]$int.flux.x.up <- 0
            SL[[spec.name]]$int.flux.x.down <- 0
            SL[[spec.name]]$int.flux.y.up <- 0
            SL[[spec.name]]$int.flux.y.down <- 0
            SL[[spec.name]]$int.kin.reac <- 0
            SL[[spec.name]]$int.eq.reac <- 0
            SL[[spec.name]]$int.reac <- 0
            SL[[spec.name]]$int.irr <- 0
            SL[[spec.name]]$int.ddt <- 0
            SL[[spec.name]]$deficit <- 0
            for (j in 1:ncol(spec.table)) {
                chem.spec <- names(spec.table)[j]
                SL[[spec.name]]$C <- SL[[spec.name]]$C + spec.table[i, 
                  j] * SL[[names(spec.table)[j]]]$C
                SL[[spec.name]]$tran <- SL[[spec.name]]$tran + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$tran
                SL[[spec.name]]$eq.reac <- SL[[spec.name]]$eq.reac + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$eq.reac
                SL[[spec.name]]$kin.reac <- SL[[spec.name]]$kin.reac + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$kin.reac
                SL[[spec.name]]$reac <- SL[[spec.name]]$reac + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$reac
                SL[[spec.name]]$irr <- SL[[spec.name]]$irr + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$irr
                SL[[spec.name]]$ddt <- SL[[spec.name]]$ddt + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$ddt
                SL[[spec.name]]$C.x.up <- SL[[spec.name]]$C.x.up + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$C.x.up
                SL[[spec.name]]$C.x.down <- SL[[spec.name]]$C.x.down + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$C.x.down
                SL[[spec.name]]$C.y.up <- SL[[spec.name]]$C.y.up + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$C.y.up
                SL[[spec.name]]$C.y.down <- SL[[spec.name]]$C.y.down + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$C.y.down
                SL[[spec.name]]$flux.x.up <- SL[[spec.name]]$flux.x.up + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$flux.x.up
                SL[[spec.name]]$flux.x.down <- SL[[spec.name]]$flux.x.down + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$flux.x.down
                SL[[spec.name]]$flux.y.up <- SL[[spec.name]]$flux.y.up + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$flux.y.up
                SL[[spec.name]]$flux.y.down <- SL[[spec.name]]$flux.y.down + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$flux.y.down
                SL[[spec.name]]$int.flux.x.up <- SL[[spec.name]]$int.flux.x.up + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.flux.x.up
                SL[[spec.name]]$int.flux.x.down <- SL[[spec.name]]$int.flux.x.down + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.flux.x.down
                SL[[spec.name]]$int.flux.y.up <- SL[[spec.name]]$int.flux.y.up + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.flux.y.up
                SL[[spec.name]]$int.flux.y.down <- SL[[spec.name]]$int.flux.y.down + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.flux.y.down
                SL[[spec.name]]$int.kin.reac <- SL[[spec.name]]$int.kin.reac + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.kin.reac
                SL[[spec.name]]$int.eq.reac <- SL[[spec.name]]$int.eq.reac + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.eq.reac
                SL[[spec.name]]$int.reac <- SL[[spec.name]]$int.reac + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.reac
                SL[[spec.name]]$int.irr <- SL[[spec.name]]$int.irr + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.irr
                SL[[spec.name]]$int.ddt <- SL[[spec.name]]$int.ddt + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$int.ddt
                SL[[spec.name]]$deficit <- SL[[spec.name]]$deficit + 
                  spec.table[i, j] * SL[[names(spec.table)[j]]]$deficit
            }
        }
    }
    return(SL)
}
