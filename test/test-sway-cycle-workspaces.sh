load 'lib/bats-support/load'
load 'lib/bats-assert/load'
load 'lib/bats-file/load'
load 'lib/bats-mock/stub'

@test "1 A missing command should return an error indicating the reason" {
  stub swaymsg

  run ../bin/sway-cycle-workspaces

  assert_failure
  assert_line "Missing: COMMAND"
}

@test "2 If an insufficient number of workspaces are given to determine a next or previous one an error message should indicate that" {
  SWAYMSG_OUTPUT="'{}'"
  stub swaymsg "--raw --type get_workspaces : echo ${SWAYMSG_OUTPUT}"

  run ../bin/sway-cycle-workspaces next

  assert_failure
  assert_line --partial "zero or one lines/workspaces provided"
}

@test "3 If the first swaymsg call fails an error message should indicate that" {
  stub swaymsg "--raw --type get_workspaces : exit 1"

  run ../bin/sway-cycle-workspaces next

  assert_failure
  assert_line --partial "Tried to execute 'swaymsg' to retrieve a workspace layout but"
}

@test "4" {
  stub swaymsg "--raw --type get_workspaces : exit 1"

  run ../bin/sway-cycle-workspaces next

  assert_failure
  assert_line --partial "Tried to execute 'swaymsg' to retrieve a workspace layout but"
}

teardown() {
  unstub swaymsg
}