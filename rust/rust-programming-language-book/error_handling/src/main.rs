use std::fs::File;
use std::io::ErrorKind;
use std::io;
use std::io::Read;
use std::io::File;
/** Topic: Error Handling **/

fn panic_example(name: &str) -> &str {
    if name.len() != 6 {
        panic!("The string is not equal to six!!!!!!!!!!!!");
    }
    return name;
}

/** Topic: Result Type **/
// Simple usage of Result Type
fn open_file(file_name: &str)  {
    // This uses the Result Type by default
    let f = File::open(file_name);
    let f: File = match f {
        Ok(file) => file,
        Err(error) => match error.kind() {
            ErrorKind::NotFound => match File::create("test.txt") {
                Ok(fc) => fc,
                Err(e) => panic!("Problems creating the file: {:?}", e)
            },
            other_error => panic!("Problems opening the file: {:?}", other_error),
        }
    };
}

// This is more idiomatic Rust
fn open_file_rusty(file_name: &str) {
    let f = File::open(file_name).unwrap_or_else(|error| {
        if error.kind() == ErrorKind::NotFound {
            File::create(file_name).unwrap_or_else(|error| {
                panic!("Problem: {:?}", error);
            })
        } else {
            panic!("Problem opening the file: {:?}", error)
        }
    });
}


/** Example: Error Matching **/
fn error_matching(file_name: &str) {
    open_file(file_name);
}

/** Topic: Propagating Errors **/

// Read a username from a file
fn read_username_from_file() -> Result<String, io::Error> {
    let f = File::open("yo.txt");

    let mut f = match f {
        Ok(file) => file,
        Err(e) => return Err(e)
    };

    let mut s = String::new();

    match f.read_to_string(&mut s) {
        Ok(_) => Ok(s),
        Err(e) => Err(e)
    }
}

// ?? - shortcut for Errors
fn read_username_from_file_shortcut() -> Result<String, io::Error> {
    let mut f = File::open("yo.txt")?;
    let mut s = String::new();
    f.read_to_string(&mut s);
    Ok(s)
}

fn main() {
    // This panics and throws an error
    // panic_example("hello");

    open_file("test.txt");
    open_file_rusty("test.txt");
    error_matching("test.txt");

    // Another way to handle errors:
    let f = File::open("test.txt").expect("Failed to open");
}
