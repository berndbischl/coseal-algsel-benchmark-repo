testtask1 = parseASTask("../qbf_2011")

testtask2 = parseASTask("../sat12-indu")

makeTestTask3 = function() {

  iids = c("i1", "i2", "i3")

  desc = makeS3Obj("Description",
    features_deterministic = c("f1", "f2", "f2"),
    features_stochastic = character(0),
    feature_steps = list(
      s1 = c("f1", "f2"),
      s2 = c("f2", "f3")
    )
  )

  rs1 = c("ok", "presolved", "crash")
  rs2 = c("ok", "ok", "ok")
  c1 = c(2, 3, NA)
  c2 = c(1, 2, 1)
  testtask3 = makeS3Obj("ASTask",
    desc = desc,
    feature.runstatus = data.frame(instance_id = iids, repetition = 1L, s1 = rs1, s2 = rs2),
    feature.costs = data.frame(instance_id = iids, repetition = 1L, s1 = c1, s2 = c2)
  )
}

testtask3 = makeTestTask3()
