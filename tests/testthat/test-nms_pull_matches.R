context("test-nms_pull_matches")

describe("nms_pull_matches", {
  it("pulls the expected names", {
    vft <- luquillo_stem_random_tiny
    expected <-  c("gx", "gy")
    expect_equal(nms_pull_matches(vft, c("x", "PX", "gx", "gy")), expected)
  })

  it("fails with unnamed input", {
    expect_error(nms_pull_matches("unnnamed", "x"), "is not TRUE")
  })
})

