load("llama_results.RData")

for(i in 1:length(resLong)) {
    md = res[res$id == i,]
    file = paste(paste(md$prob, md$algo, md$model, sep = "-"), ".csv", sep = "")

    df = do.call(rbind, by(resLong[[i]]$predictions, resLong[[i]]$predictions$instance_id, head, n = 1))

    write.csv(df[,c("instance_id", "algorithm")], file = paste("csvs", file, sep = "/"), row.names = FALSE)
}
