testtask1 = parseASTask("../qbf_2011")

testtask2 = parseASTask("../sat12-indu")

makeTestTask3 = function() {

  iids = c("i1", "i2", "i3")

  desc = makeS3Obj("Description",
    task_id = "foo",
    performance_measures = "m",
    performance_type = "runtime",
    maximize = FALSE,
    algorithm_cutoff_time = 100,
    features_deterministic = c("f1", "f2", "f2"),
    features_stochastic = character(0),
    algorithms_deterministic = c("a1", "a2"),
    algorithms_stochastic = character(0),
    feature_steps = list(
      s1 = c("f1", "f2"),
      s2 = c("f2", "f3")
    )
  )

  rs1 = c("ok", "presolved", "crash")
  rs2 = c("ok", "ok", "ok")
  c1 = c(20, 30, NA)
  c2 = c(10, 20, 10)
  f1 = f2 = f3 = c(1, 2, 3)
  feats = data.frame(instance_id = iids, repetition = 1, f1 = f1, f2 = f2, f3 = f3)
  algo.runs = rbind(
    data.frame(instance_id = iids, repetition = 1L, algo = "a1", m = c(30, 30, 30)),
    data.frame(instance_id = iids, repetition = 1L, algo = "a1", m = c(30, 30, 30))
  )
  makeS3Obj("ASTask",
    desc = desc,
    feature.values = feats,
    feature.runstatus = data.frame(instance_id = iids, repetition = 1L, s1 = rs1, s2 = rs2),
    feature.costs = data.frame(instance_id = iids, repetition = 1L, s1 = c1, s2 = c2),
    algo.runs = algo.runs
  )
}

testtask3 = makeTestTask3()
