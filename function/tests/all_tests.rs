mod infra;

// Your tests go here!
success_tests! {
    {
        name: fact,
        file: "fact.snek",
        input: "10",
        expected: "3628800",
    },
    {
        name: even_odd_1,
        file: "even_odd.snek",
        input: "10",
        expected: "10\ntrue\ntrue",
    },
    {
        name: even_odd_2,
        file: "even_odd.snek",
        input: "9",
        expected: "9\nfalse\nfalse",
    },
    {
        name: quick_brown_fox,
        file: "quick_brown_fox.snek",
        expected: "-3776",
    }
}

runtime_error_tests! {}

static_error_tests! {
    {
        name: duplicate_params,
        file: "duplicate_params.snek",
        expected: "",
    }
}
