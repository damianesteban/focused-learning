use std::fs::File;
use std::io::ErrorKind;
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

    let f = match f {
        Ok(file) => file,
        Err(ref error) if error.kind() == ErrorKind::NotFound => {
            match File::create(file_name) {
                Ok(fc) => fc,
                Err(e) => {
                    panic!(
                        "Tried to create file but there was a problem: {:?}",
                        e
                    )
                }
            }
        },
        Err(error) => {
            panic!("There was a SERIOUS problem opening the file: {:?}", error);
        },
    };
}


/** Example: Error Matching **/
fn error_matching(file_name: &str) {
    open_file(file_name);
}

/** Shortcuts for Panic on Error: unwrap and expect  **/

fn main() {
    // This panics and throws an error
    // panic_example("hello");

    open_file("good_file.txt");
    error_matching("test.txt");
}
