load 'lib/bats-support/load'
load 'lib/bats-assert/load'
load 'lib/bats-file/load'
load 'lib/bats-mock/stub'

@test "1 If a command is missing an error message should indicate that" {
  stub swaymsg

  run ../bin/sway-cycle-workspaces

  assert_failure
  assert_line "Missing: COMMAND"
}

@test "2 If an insufficient number of workspaces are given to determine a next or previous one an error message should indicate that" {
  EMPTY_SWAYMSG_WORKSPACE_QUERY_RESULT="'{}'"
  stub swaymsg "--raw --type get_workspaces : echo ${EMPTY_SWAYMSG_WORKSPACE_QUERY_RESULT}"

  run ../bin/sway-cycle-workspaces next

  assert_failure
  assert_line --partial "zero or one lines/workspaces provided"
}

@test "3 If the first swaymsg call (query workspaces) fails an error message should indicate that" {
  stub swaymsg "--raw --type get_workspaces : exit 1"

  run ../bin/sway-cycle-workspaces next

  assert_failure
  assert_line --partial "Tried to execute 'swaymsg' to retrieve a workspace layout but"
}

@test "4 If the second swaymsg call (set new workspace) fails an error message should indicate that" {
  SWAYMSG_WORKSPACE_QUERY_RESULT="'[{\"output\":\"a\",\"name\":\"1\",\"focused\":\"true\"}, {\"output\":\"a\",\"name\":\"2\",\"focused\":\"false\"}]'"
  stub swaymsg \
    "--raw --type get_workspaces : echo ${SWAYMSG_WORKSPACE_QUERY_RESULT}" \
    "workspace number 2 : exit 1"

  run ../bin/sway-cycle-workspaces next

  assert_failure
  assert_line --partial "Tried to execute 'swaymsg' to change the workspace but"
}

teardown() {
  unstub swaymsg
}