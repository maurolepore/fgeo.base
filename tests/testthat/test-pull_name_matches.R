context("test-pull_name_matches")

describe("pull_name_matches", {
  it("pulls the expected names", {
    vft <- fgeo.data::luquillo_stem_random_tiny
    expected <-  c("gx", "gy")
    expect_equal(pull_name_matches(vft, c("x", "PX", "gx", "gy")), expected)
  })

  it("fails with unnamed input", {
    expect_error(pull_name_matches("unnnamed", "x"), "is not TRUE")
  })
})

