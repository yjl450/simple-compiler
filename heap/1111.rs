use std::env;

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64) -> i64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    // print error message according to writeup
    let err = match errcode {
        556677 => "invalid argument (type mismatch)".to_string(),
        112233 => "calculation overflow".to_string(),
        _ => format!("unknown error code {}", errcode),
    };
    eprintln!("{} {}", err, errcode);
    std::process::exit(1);
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val: i64) -> i64 {
    println!("{}", parse_output(val));
    return val;
}

fn parse_input(input: &str) -> i64 {
    // parse the input string into internal value representation
    if input == "true" {
        3
    } else if input == "false" {
        1
    } else {
        input.parse::<i64>().unwrap() << 1
    }
}

fn parse_output(output: i64) -> String {
    match output {
        1 => "false".to_string(),
        3 => "true".to_string(),
        _ => format!("{}", output >> 1),
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() == 2 { &args[1] } else { "false" };
    let input = parse_input(&input);
    
    let i: i64 = unsafe { our_code_starts_here(input) };
    println!("{}", parse_output(i));
}
