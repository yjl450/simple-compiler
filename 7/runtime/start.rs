use std::{collections::HashSet, env};

type SnekVal = u64;

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(i64)]
pub enum ErrCode {
    InvalidArgument = 1,
    Overflow = 2,
    IndexOutOfBounds = 3,
    InvalidVecSize = 4,
    OutOfMemory = 5,
}

const TRUE: u64 = 7;
const FALSE: u64 = 3;

static mut HEAP_START: *const u64 = std::ptr::null();
static mut HEAP_END: *const u64 = std::ptr::null();

#[link(name = "our_code")]
extern "C" {
    // The \x01 here is an undocumented feature of LLVM that ensures
    // it does not add an underscore in front of the name.
    // Courtesy of Max New (https://maxsnew.com/teaching/eecs-483-fa22/hw_adder_assignment.html)
    #[link_name = "\x01our_code_starts_here"]
    fn our_code_starts_here(input: u64, heap_start: *const u64, heap_end: *const u64) -> u64;
}

#[export_name = "\x01snek_error"]
pub extern "C" fn snek_error(errcode: i64) {
    if errcode == ErrCode::InvalidArgument as i64 {
        eprintln!("invalid argument");
    } else if errcode == ErrCode::Overflow as i64 {
        eprintln!("overflow");
    } else if errcode == ErrCode::IndexOutOfBounds as i64 {
        eprintln!("index out of bounds");
    } else if errcode == ErrCode::InvalidVecSize as i64 {
        eprintln!("vector size must be non-negative");
    } else {
        eprintln!("an error ocurred {}", errcode);
    }
    std::process::exit(errcode as i32);
}

#[export_name = "\x01snek_print"]
pub unsafe extern "C" fn snek_print(val: SnekVal) -> SnekVal {
    println!("{}", snek_str(val, &mut HashSet::new()));
    val
}

/// This function is called when the program needs to allocate `count` words of memory and there's no
/// space left. The function should try to clean up space by triggering a garbage collection. If there's
/// not enough space to hold `count` words after running the garbage collector, the program should terminate
/// with an `out of memory` error.
///
/// Args:
///     * `count`: The number of words the program is trying to allocate, including an extra word for
///       the size of the vector and an extra word to store metadata for the garbage collector, e.g.,
///       to allocate a vector of size 5, `count` will be 7.
///     * `heap_ptr`: The current position of the heap pointer (i.e., the value stored in `%r15`). It
///       is guaranteed that `heap_ptr + 8 * count > HEAP_END`, i.e., this function is only called if
///       there's not enough space to allocate `count` words.
///     * `stack_base`: A pointer to the "base" of the stack.
///     * `curr_rbp`: The value of `%rbp` in the stack frame that triggered the allocation.
///     * `curr_rsp`: The value of `%rsp` in the stack frame that triggered the allocation.
///
/// Returns:
///
/// The new heap pointer where the program should allocate the vector (i.e., the new value of `%r15`)
///
#[export_name = "\x01snek_try_gc"]
pub unsafe extern "C" fn snek_try_gc(
    count: isize,
    heap_ptr: *const u64,
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> *const u64 {
    let new_ptr = snek_gc(heap_ptr, stack_base, curr_rbp, curr_rsp);
    let free_space = (HEAP_END as isize - new_ptr as isize) / 8;
    // println!(
    //     "free space: {:?} count {:?}, newptr {:?}, end {:?}",
    //     free_space, count, new_ptr, HEAP_END
    // );
    if free_space < count {
        eprintln!("out of memory");
        std::process::exit(ErrCode::OutOfMemory as i32)
    }
    new_ptr
}

pub unsafe fn mark_heap(ptr: *mut u64, heap_ptr: *const u64) {
    let gc_word = *ptr;
    if gc_word == 1 {
        return;
    } else {
        *ptr = 1;
    }
    let size = *ptr.add(1) as usize;
    for i in 0..size {
        let mut elem = ptr.add(2 + i).read();
        if elem != TRUE
            && elem != FALSE
            && elem != 1
            && elem & 0x1 == 1
            && elem <= heap_ptr as u64
            && elem >= HEAP_START as u64
        {
            elem -= 1;
            mark_heap(elem as *mut u64, heap_ptr);
        }
    }
}

/// This function should trigger garbage collection and return the updated heap pointer (i.e., the new
/// value of `%r15`). See [`snek_try_gc`] for a description of the meaning of the arguments.
#[export_name = "\x01snek_gc"]
pub unsafe extern "C" fn snek_gc(
    heap_ptr: *const u64,
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) -> *const u64 {
    let mut new_heap_ptr = HEAP_START as *mut u64;
    // snek_print_stack(stack_base, curr_rbp, curr_rsp);
    // Step 1 - Mark
    // println!("before mark ----------------------------");
    // heap_print(heap_ptr);
    let mut ptr = stack_base;
    while ptr >= curr_rsp {
        let mut val = *ptr;
        if !(val != TRUE
            && val != FALSE
            && val != 1
            && val & 0x1 == 1
            && val <= heap_ptr as u64
            && val >= HEAP_START as u64)
        {
            // not a ptr to vec
            ptr = ptr.sub(1);
            continue;
        }

        val -= 1;
        mark_heap(val as *mut u64, heap_ptr);
        ptr = ptr.sub(1);
    }
    // println!("after mark ----------------------------");
    // heap_print(heap_ptr);

    // Step 2 - Forward
    let mut fwd: *mut u64 = HEAP_START as *mut u64;
    let mut orig: *mut u64 = HEAP_START as *mut u64;
    while fwd < HEAP_END as *mut u64 && *fwd != 0 {
        // find first empty spot
        let size = *fwd.add(1) as usize;
        fwd = fwd.add(2 + size);
    }

    orig = fwd as *mut u64;

    while orig < heap_ptr as *mut u64 && fwd < heap_ptr as *mut u64 {
        while *orig != 1 && orig < heap_ptr as *mut u64 {
            // find first accessible spot
            let size = *orig.add(1) as usize;
            orig = orig.add(2 + size);
        }
        if *orig == 1 {
            // println!("forwarding {:?} to {:?}", orig, fwd);
            *orig = fwd as u64;
            let size = *orig.add(1) as usize;
            fwd = fwd.add(size + 2);
        }
    }
    // println!("after forward ----------------------------");
    // heap_print(heap_ptr);
    // Step 3 - Redirect
    // 3.1 redirect stack addr
    let mut ptr: *mut u64 = stack_base as *mut u64;
    while ptr >= curr_rsp as *mut u64 {
        let mut val = *ptr;
        if !(val != TRUE
            && val != FALSE
            && val != 1
            && val & 0x1 == 1
            && val <= heap_ptr as u64
            && val >= HEAP_START as u64)
        {
            // not a ptr to vec
            ptr = ptr.sub(1);
            continue;
        }
        val -= 1;
        if *(val as *const u64) == 0 || *(val as *const u64) == 1 {
            ptr = ptr.sub(1);
            continue;
        }
        *ptr = *(val as *const u64) + 1;
        ptr = ptr.sub(1);
    }

    // 3.2 redirect heap addr
    let mut ptr = HEAP_START;
    while ptr < heap_ptr {
        let val = *ptr; // redirect addr
        let size = *ptr.add(1) as usize;
        if val == 0 || val == 1 {
            ptr = ptr.add(size + 2);
            continue;
        }
        for i in 0..size {
            let mut elem = ptr.add(2 + i).read();
            if elem != TRUE
                && elem != FALSE
                && elem != 1
                && elem & 0x1 == 1
                && elem <= heap_ptr as u64
                && elem >= HEAP_START as u64
            {
                if ((elem - 1) as *const u64).read() == 1 {
                    continue;
                }
                let temp_ptr = ptr.add(2 + i) as *mut u64;
                *temp_ptr = *((elem - 1) as *const u64) + 1;
            }
        }
        ptr = ptr.add(size + 2);
    }

    // println!("after redirect ----------------------------");
    // heap_print(heap_ptr);

    // Step 4 - Compact
    let mut ptr: *mut u64 = HEAP_START as *mut u64;
    while ptr < heap_ptr as *mut u64{
        if *ptr == 1 {
            *ptr = 0;
            let size = *ptr.add(1) as usize;
            ptr = ptr.add(size + 2);
            new_heap_ptr = ptr;
            continue;
        }
        if *ptr == 0 {
            let size = *ptr.add(1) as usize;
            for i in 0..size + 2 {
                let tmp: *mut u64 = ptr.add(i) as *mut u64;
                *tmp = 0;
            }
            ptr = ptr.add(size + 2);
            continue;
        }

        let size = *ptr.add(1) as usize;
        let fwd = *ptr as *mut u64;
        fwd.add(1).write(size as u64);
        for i in 2..size + 2 {
            fwd.add(i).write(ptr.add(i).read());
        }
        for i in 0..size + 2 {
            (ptr as *mut u64).add(i).write(0);
        }
        new_heap_ptr = fwd.add(size + 2);
        ptr = ptr.add(size + 2);
    }
    // println!("after compact ----------------------------");
    // heap_print(new_heap_ptr);
    // snek_print_stack(stack_base, curr_rbp, curr_rsp);
    // Step 5 - Update heap_ptr

    // println!("new heap ptr: {:?}", new_heap_ptr);
    new_heap_ptr
}

pub unsafe fn heap_print(heap_ptr: *const u64) {
    println!("heap *****************************");
    let mut ptr = HEAP_START;
    while ptr < HEAP_END {
        let val = *ptr;
        println!("{ptr:?}: {:#0x}", val);
        ptr = ptr.add(1);
    }
    println!("heap end *************************");
}

/// A helper function that can be called with the `(snek-printstack)` snek function. It prints the stack
/// See [`snek_try_gc`] for a description of the meaning of the arguments.
#[export_name = "\x01snek_print_stack"]
pub unsafe extern "C" fn snek_print_stack(
    stack_base: *const u64,
    curr_rbp: *const u64,
    curr_rsp: *const u64,
) {
    let mut ptr = stack_base;
    println!("-----------------------------------------");
    while ptr >= curr_rsp {
        let val = *ptr;
        println!("{ptr:?}: {:#0x}", val);
        ptr = ptr.sub(1);
    }
    println!("----------------------------------------- stack end");
}

unsafe fn snek_str(val: SnekVal, seen: &mut HashSet<SnekVal>) -> String {
    if val == TRUE {
        format!("true")
    } else if val == FALSE {
        format!("false")
    } else if val & 1 == 0 {
        format!("{}", (val as i64) >> 1)
    } else if val == 1 {
        format!("nil")
    } else if val & 1 == 1 {
        if !seen.insert(val) {
            return "[...]".to_string();
        }
        let addr = (val - 1) as *const u64;
        let size = addr.add(1).read() as usize;
        let mut res = "[".to_string();
        for i in 0..size {
            let elem = addr.add(2 + i).read();
            res = res + &snek_str(elem, seen);
            if i < size - 1 {
                res = res + ", ";
            }
        }
        seen.remove(&val);
        res + "]"
    } else {
        format!("unknown value: {val}")
    }
}

fn parse_input(input: &str) -> u64 {
    match input {
        "true" => TRUE,
        "false" => FALSE,
        _ => (input.parse::<i64>().unwrap() << 1) as u64,
    }
}

fn parse_heap_size(input: &str) -> usize {
    input.parse::<usize>().unwrap()
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() >= 2 { &args[1] } else { "false" };
    let heap_size = if args.len() >= 3 { &args[2] } else { "10000" };
    let input = parse_input(&input);
    let heap_size = parse_heap_size(&heap_size);

    // Initialize heap
    let mut heap: Vec<u64> = Vec::with_capacity(heap_size);
    unsafe {
        HEAP_START = heap.as_mut_ptr();
        HEAP_END = HEAP_START.add(heap_size);
    }

    let i: u64 = unsafe { our_code_starts_here(input, HEAP_START, HEAP_END) };
    unsafe { snek_print(i) };
}
