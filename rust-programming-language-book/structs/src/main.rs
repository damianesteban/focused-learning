
/** Let's play with Structs **/
struct User {
    email: String,
    sign_in_count: u64,
    active: bool
}

fn userTest() {
    let user1 = User {
        email: String::from("falls@walls.com"),
        sign_in_count: 1,
        active: true,
    };

    // We can't do this! Its immutable!
    // user1.email = String::from("balls");

    let mut user2 = User {
        email: String::from("someone@example.com"),
        active: true,
        sign_in_count: 1,
    };

    // But this is ok.
    user2.email = String::from("anotheremail@example.com");

    // It’s often useful to create a new instance of a struct that
    // uses most of an old instance’s values but changes some.
    // You’ll do this using struct update syntax.
    let user3 = User {
        email: user2.email,
        active: user1.active,
        sign_in_count: user1.sign_in_count
    };

    // And this is even fancier:
    let user4 = User {
        email: String::from("wee@weeeee.com"),
        ..user1
    };

}

fn user_factory(email: String) -> User {
    // Implicit return
    User {
        email,
        active: true,
        sign_in_count: 1,
    }
}

/** Example Project: Rectangles **/

// 1. Basic
fn area(width: u32, height: u32) -> u32 {
    width * height
}

fn rec() {
    let width1 = 30;
    let height1 = 50;

    println!(
        "The area of the rectangle is {} square pixels.",
        area(width1, height1)
    )
}

// 2. Using Tuples
fn area1(dimensions: (u32, u32)) -> u32 {
    dimensions.0 * dimensions.1
}

fn rec1() {
    let rect1 = (30, 50);

    println!(
        "The area of the rectangle is {} square pixels.",
        area1(rect1)
    )
}

// 3. Using Structs

/** Example: Print a Struct **/

// We cannot print out a Struct by default, we need to give it a Derived Trait

#[derive(Debug)]
struct Rectangle {
    width: u32,
    height: u32,
}

impl Rectangle {
    // method
    fn area(&self) -> u32 {
        self.width * self.height
    }
    // method
    fn can_hold(&self, rectangle: Rectangle) -> bool {
        rectangle.area() <= self.area()
    }

    // Associated Method (static method)
    fn make_square(size: u32) -> Rectangle {
        Rectangle { width: size, height: size }
    }
}

fn area2(rectangle: &Rectangle) -> u32 {
    rectangle.width * rectangle.height
}

fn rec2() {
    let rect1 = Rectangle { width: 30, height: 50 };

    println!(
        "The area of the rectangle is {} square pixels.",
        area2(&rect1)
    );
    println!("rect1 is {:?}", &rect1);
}

/** Methods on Structs **/
// NOTE: - Yes, I know it was a bit silly to use a Square instead of a Rectangle for this example...
// but it is very late and I didn't want to name the Struct "Rectangle1"
#[derive(Debug)]
struct Square {
    width: u32,
    height: u32,
}

impl Square {
    // method
    fn area(&self) -> u32 {
        self.width * self.height
    }
    // method
    fn can_hold(&self, square: Square) -> bool {
        square.area() <= self.area()
    }

    // Associated Method (static method)
    fn make_square(size: u32) -> Square {
        Square { width: size, height: size }
    }
}

fn square1() {
    let square1 = Square { height: 50, width: 50 };

    println!(
        "The area of the square is {} square pixels...also, we made a square! Check it: {:#?}",
        square1.area(), Square::make_square(15)
    )
}


fn main() {
    square1()
}
