#### Useless Algorithms did not make much sense

<h2> Useless Algorithms </h2>
  
  Here, you'll find all the algorithms that always resulted in the same performance (and eventually NA's):<br>
  <br>
  
  <!--begin.rcode, results = "asis"
res = findUselessInstances(astask)$algo.runs
if(nrow(res) >= 1) {
  print(xtable(res), "html")
} else {
  cat("There were no entirely useless algorithms.")
}
end.rcode-->
  
<br>
<br>
  