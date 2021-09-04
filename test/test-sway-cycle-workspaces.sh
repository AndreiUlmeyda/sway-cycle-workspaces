load 'lib/bats-support/load'
load 'lib/bats-assert/load'
load 'lib/bats-file/load'
load 'lib/bats-mock/stub'

@test "1 A missing command should return a respective error" {
  stub swaymsg

  run ../bin/sway-cycle-workspaces

  assert_failure
  assert_line "Missing: COMMAND"
}

@test "2 An empty workspace configuration should lead to failure" {
  swaymsg-output="'{}'"
  stub swaymsg "--raw --type get_workspaces : echo ${swaymsg-output}"

  run ../bin/sway-cycle-workspaces next

  assert_failure
  assert_line --partial "zero or one lines/workspaces provided"
}

teardown() {
  unstub swaymsg
}

#@test "2 " {
#  stub swaymsg \
#        "--raw --type get_workspaces : echo '{}'" \
#        "workspace number : echo ''"
#  run ../bin/sway-cycle-workspaces next
#  assert_success
#
#  unstub swaymsg
#}