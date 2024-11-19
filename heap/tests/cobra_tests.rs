mod infra;

// Your tests go here!
success_tests! {
    {
        name: false_val,
        file: "false_val.snek",
        expected: "false",
    },
    {
        name: input_compare_1,
        file: "input_compare.snek",
        input: "2",
        expected: "false",
    },
    {
        name: input_compare_2,
        file: "input_compare.snek",
        input: "10",
        expected: "true",
    },
    {
        name: true_val,
        file: "true_val.snek",
        expected: "true",
    },
    {
        name: add1,
        file: "add1.snek",
        expected: "13",
    },
    {
        name: sub1,
        file: "sub1.snek",
        expected: "11",
    },
    {
        name: plus,
        file: "plus.snek",
        expected: "5",
    },
    {
        name: minus,
        file: "minus.snek",
        expected: "12",
    },
    {
        name: times,
        file: "times.snek",
        input: "32874972489",
        expected: "295874752401",
    },
    {
        name: isnum0,
        file: "isnum0.snek",
        expected: "false",
    },
    {
        name: isnum1,
        file: "isnum1.snek",
        expected: "true",
    },
    {
        name: block,
        file: "block.snek",
        expected: "-6",
    },
    {
        name: equal_type1,
        file: "equal_type1.snek",
        input: "15",
        expected: "false",
    },
    {
        name: equal_type2,
        file: "equal_type2.snek",
        input: "2",
        expected: "true",
    },
    {
        name: equal0,
        file: "equal0.snek",
        expected: "false",
    },
    {
        name: set,
        file: "set.snek",
        expected: "16",
    },
    {
        name: loop0,
        file: "loop.snek",
        expected: "5",
    },
    {
        name: input1,
        file: "input1.snek",
        input: "9",
        expected: "15",
    },
    {
        name: input0,
        file: "input0.snek",
        input: "323334",
        expected: "323334",
    },
    {
        name: if0,
        file: "if.snek",
        input: "1223",
        expected: "1223",
    },
    {
        name: greater,
        file: "greater.snek",
        expected: "true",
    },
    {
        name: greaterequal,
        file: "greaterequal.snek",
        expected: "true",
    },
    {
        name: fact_20,
        file: "factorial.snek",
        input: "20",
        expected: "2432902008176640000",
    },
    {
        name: times_no_overflow,
        file: "times_overflow.snek",
        input: "130311999481",
        expected: "4611686018408811613",
    },
}

runtime_error_tests! {
    {
        name: invalid_argument,
        file: "invalid_argument.snek",
        expected: "invalid argument",
    },
    {
        name: input_compare_3,
        file: "input_compare.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: equal_type1_error,
        file: "equal_type1.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: equal_type2_error,
        file: "equal_type2.snek",
        input: "true",
        expected: "invalid argument",
    },
    {
        name: fact_21,
        file: "factorial.snek",
        input: "21",
        expected: "overflow",
    },
    {
        name: times_overflow,
        file: "times_overflow.snek",
        input: "130311999482",
        expected: "overflow",
    },
    {
        name: add_overflow,
        file: "add_overflow.snek",
        expected: "overflow",
    },
}

static_error_tests! {
    {
        name: number_bounds_fail,
        file: "number_bounds_fail.snek",
        expected: "Invalid",
    },
    {
        name: duplicate_binding,
        file: "dup.snek",
        expected: "Duplicate binding",
    },
    {
        name: unbound,
        file: "unbound.snek",
        expected: "Unbound variable identifier unbound",
    },
    {
        name: loose_break,
        file: "break.snek",
        expected: "break",
    },
    {
        name: invalid_id,
        file: "invalid_id.snek",
        expected: "invalid",
    },
}
