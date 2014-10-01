optimise = function(bitvs, ldf) {
    scores = parallelMap(function(bitv) {
        sel = (bitv == 1)
        ldf$features = ldf$features[sel]
        model = regression(makeLearner("regr.randomForest"), ldf)
        score = mean(parscores(ldf, model))
        return(score)
    }, bitvs, simplify = TRUE, level = "outer")
    return(scores)
}
