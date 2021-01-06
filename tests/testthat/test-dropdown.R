context("dropdownServer")

testServer(dropdownServer, {
  session$setInputs(dropdown = "CrazyShip")
  expect_equal(dropdown(), "CrazyShip")
  expect_equal(session$returned(), "CrazyShip")
  
  session$setInputs(dropdown = "SmallShip")
  expect_equal(dropdown(), "SmallShip")
  expect_equal(session$returned(), "SmallShip")


})
