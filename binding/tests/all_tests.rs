mod infra;

// Your tests go here!
success_tests! {
    add1: "73",
    add: "15",
    nested_arith: "25",
    binding: "5",
    binding1: "6",
    multi_bind: "9",
    nested_bind: "54",
    nested_scope_dup: "1",
}

failure_tests! {
    unbound_id: "Unbound variable identifier x",
    duplicate_binding: "Duplicate binding",
    nested_bind_dup: "Duplicate binding",
    nested_bind_dup_unbound: "Duplicate binding",
}
