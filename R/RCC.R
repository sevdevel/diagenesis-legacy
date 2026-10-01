`RCC` <-
function (RC, SV, SV.ref) 
{
    RCC <- rep(0, ncol(RC))
    for (i in 1:ncol(RC)) RCC[i] <- sqrt(sum((RC[, i]/(abs(SV[, 
        i]) + SV.ref[i]))^2)/nrow(RC))
    return(RCC)
}
