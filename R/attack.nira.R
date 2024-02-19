attack.nira <- function(attack, measure, net_obj, dt) {
  
  
  if (attack == "cascade" & measure == "alleviating") {
    rand.df <- random.att.nira(net_obj, dt)
    att.df <- cas.att.allev(net_obj, dt)
    
    return(
      list(
        "cascade attack alleviating scores" = data.frame(att.df),
        "random attack" = data.frame(rand.df)
      )
    )
    
  }
  
  if (attack == "cascade" & measure == "aggravating") {
    rand.df <- random.att.nira(net_obj, dt)
    att.df <- cas.att.aggrav(net_obj, dt)
    
    return(
      list(
        "cascade attack aggravating scores" = data.frame(att.df),
        "random attack" = data.frame(rand.df)
      )
    )
    
  }
}
