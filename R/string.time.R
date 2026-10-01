`string.time` <-
function (t, unit = c("yr", "d", "hr", "min", "sec")) 
{
    if (unit[1] == "d") 
        t <- t/365.25
    if (unit[1] == "hr") 
        t <- t/(365.25 * 24)
    if (unit[1] == "min") 
        t <- t/(365.25 * 24 * 60)
    if (unit[1] == "sec") 
        t <- t/(365.25 * 24 * 60 * 60)
    years <- floor(t)
    y.str <- paste(years)
    t <- (t - years) * 365.25
    days <- floor(t)
    if (days < 10) 
        d.str <- paste(NULL, NULL, days)
    if ((days < 100) & (days >= 10)) 
        d.str <- paste(NULL, days)
    if (days >= 100) 
        d.str <- paste(days)
    t <- (t - days) * 24
    hours <- floor(t)
    if (hours < 10) 
        h.str <- paste(NULL, hours)
    else h.str <- paste(hours)
    t <- (t - hours) * 60
    mins <- floor(t)
    if (mins < 10) 
        m.str <- paste(NULL, mins)
    else m.str <- paste(mins)
    t <- (t - mins) * 60
    secs <- floor(t)
    if (secs < 10) 
        s.str <- paste(NULL, secs)
    else s.str <- paste(secs)
    t.string <- paste(y.str, "yr", d.str, "d", h.str, "hr", m.str, 
        "min", s.str, "sec")
    return(t.string)
}
