use std::env;
use std::mem;

static mut HEAP: [u64; 100000] = [0; 100000];

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM (which Rust uses) that ensures
    // it does not add an underscore in front of the name, which happens on OSX
    // Courtesy of Max New
    // (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: i64, heap: *mut u64) -> i64;
}

#[no_mangle]
#[export_name = "\x01snek_error"]
pub fn snek_error(errcode: i64) {
    // print error message according to writeup
    let err = match errcode {
        556677 => "invalid argument (type mismatch)".to_string(),
        112233 => "calculation overflow".to_string(),
        778899 => "Index out of bounds".to_string(),
        123456 => "Get from non-vec object".to_string(),
        _ => format!("unknown error code {}", errcode),
    };
    eprintln!("{} {}", err, errcode);
    std::process::exit(1);
}

#[no_mangle]
#[export_name = "\x01snek_print"]
fn snek_print(val: i64) -> i64 {
    println!("{}", print_content(&val));
    return val;
}

fn print_content(val: &i64) -> String {
    if *val == 3 {
        return "false".to_string();
    } else if *val == 7 {
        return "true".to_string();
    } else if *val & 1 == 0 {
        return format!("{}", val >> 1);
    } else {
        let ptr: *const i64 = unsafe { mem::transmute::<i64, *const i64>(val - 1) };
        let len: i64 = unsafe { *ptr };
        let mut string: String = "(vec".to_string();
        for i in 0..len {
            let current = unsafe { *ptr.add(i as usize + 1) };
            string += &(" ".to_string() + &print_content(&current));
        }
        string += ")";
        return string;
    }
}

fn parse_arg(v: &Vec<String>) -> i64 {
    if v.len() <= 1 {
        return 3; // default false
    }
    let s = &v[1];
    if s == "true" {
        7
    } else if s == "false" {
        3
    } else {
        s.parse::<i64>().unwrap() << 1
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = parse_arg(&args);
    let i: i64 = unsafe { our_code_starts_here(input, HEAP.as_mut_ptr()) };
    snek_print(i);
}
