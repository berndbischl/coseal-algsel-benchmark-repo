INST_DIR = file.path("..", "..", "inst")
testscenario1 = parseASScenario(file.path(INST_DIR, "qbf_2011"))
testscenario2 = parseASScenario(file.path(INST_DIR, "sat12-indu"))

makeTestScenario3 = function() {

  iids = c("i1", "i2", "i3")

  desc = makeS3Obj("ASScenarioDesc",
    scenario_id = "foo",
    performance_measures = "m",
    performance_type = c(m = "runtime"),
    maximize = c(m = FALSE),
    algorithm_cutoff_time = 100,
    features_deterministic = c("f1", "f2", "f3"),
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
  f1 = 1:3
  f2 = 2:4
  f3 = 3:5
  feats = data.frame(instance_id = iids, repetition = 1, f1 = f1, f2 = f2, f3 = f3)
  algo.runs = rbind(
    data.frame(instance_id = iids, repetition = 1L, algorithm = "a1",
      m = c(30, 90, 70), runstatus = c("ok", "ok", "ok")),
    data.frame(instance_id = iids, repetition = 1L, algorithm = "a2",
      m = c(50, 30, 10), runstatus = c("ok", "ok", "crash"))
  )
  algo.runstatus = data.frame(instance_id = iids, repetition = 1L,
      a1 = c("ok", "ok", "ok"), a2 = c("ok", "ok", "crash"))
  algo.perf = data.frame(instance_id = iids, repetition = 1L,
      a1 = c(30, 90, 70), a2 = c(50, 30, 10))
  makeS3Obj("ASScenario",
    desc = desc,
    feature.values = feats,
    feature.runstatus = data.frame(instance_id = iids, repetition = 1L, s1 = rs1, s2 = rs2),
    feature.costs = data.frame(instance_id = iids, repetition = 1L, s1 = c1, s2 = c2),
    algo.runs = algo.runs,
    algo.runstatus = algo.runstatus,
    algo.perf = list(m = algo.perf)
  )
}

testscenario3 = makeTestScenario3()
testscenario4 = makeTestScenario3()
testscenario4$algo.runs$runstatus = factor(c("ok", "ok", "crash", "ok", "ok", "crash"))
testscenario4$algo.runstatus$a1 = factor(c("ok", "ok", "crash"))
