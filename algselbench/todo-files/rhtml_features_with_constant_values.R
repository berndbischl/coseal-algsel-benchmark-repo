<br>
  <h2> Useless Features </h2>
  
  In this section, you'll find all features that adopted only one value (and eventually NA's).<br>
  <br>
  
  <!--begin.rcode, results = "asis"
res = findUselessInstances(astask)
cat("Feature Values:\n")
if(nrow(res$feature.values) >= 1) {
  print(xtable(res$feature.values, digits = c(0, 4, 0)), "html")
} else {
  cat("There were no entirely useless feature values.")
}
end.rcode-->
  
  <br>
  <!--begin.rcode, results = "asis"
cat("Runstatus of Feature Steps:\n")
if(nrow(res$feature.runstatus) >= 1) {
  print(xtable(res$feature.runstatus), "html")
} else {
  cat("\nNone of the feature steps consisted of unique values (except for 'ok').")
}
end.rcode-->