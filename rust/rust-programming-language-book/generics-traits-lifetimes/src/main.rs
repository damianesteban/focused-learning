/** Topic: Generics **/

/** Example: Generic Function **/

fn largest<T: PartialOrd + Copy>(list: &[T]) -> T {
    let mut largest = list[0];

    for &item in list.iter() {
        if item > largest {
            largest = item;
        }
    }
    largest
}

struct Point<T> {
    x: T,
    y: T,
}

/** Example: Implement methods on generics **/
impl<T> Point<T> {
    fn x(&self) -> &T {
        &self.x
    }
}

/** Topic: Traits **/

pub trait Summary {
    fn summarize(&self) -> String;

    // Example: Default Implementations
    fn summary_beginning(&self) -> String {
        String::from("Summary now beginning...")
    }
}

/** Example: Implementation of Trait **/
pub struct Article {
    pub title: String,
    pub author: String,
    pub content: String,
}

/** Example: Trait Bounds (Generic / Type Constraints) **/
pub fn notify<T: Summary>(item: T) {
    println!("Breaking news! {}", item.summarize());
}

impl Summary for Article {
    fn summarize(&self) -> String {
        format!("{}: {}", self.title, self.author)
    }
}

/** Topic: Borrowing and Lifetimes **/

// The function below will throw errors
//fn borrowing_check() {
//    let r;                 // ---------+-- 'a
//                                 //          |
//    {                            //          |
//        let x = 5;          // -+-- 'b  |
//        r = &x;                  //  |       |
//    }                            // -+       |
//                                 //          |
//    println!("r: {}", r);        //          |
//}                                // ---------+

/** Example: Lifetime Annotation Syntax **/
/**
    &i32        // a reference
    &'a i32     // a reference with an explicit lifetime
    &'a mut i32 // a mutable reference with an explicit lifetime
**/

// This function needs lifetime annotations
fn longest<'a>(x: &'a str, y: &'a str) -> &str {
    if x.len() > y.len() {
        x
    } else {
        y
    }
}

// TODO: - Continue with "Lifetime Annotations in Function Signatures"
// Review code from above function

fn main() {
    // Generic Struct
    let _integer = Point { x: 5, y: 10 };
    let _float = Point { x: 5.0, y: 10.0 };


    // The largest function (generic)
    let xs = vec![34, 50, 27, 100, 654];

    let result = largest(&xs);
    println!(
        "The largest number is {}",
        result
    );

    borrowing_check();
}
