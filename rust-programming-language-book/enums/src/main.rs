/** Topic: Enums  **/

/** Example: IP Address Struct with an Enum **/
enum IpAddrKind {
    V4,
    V6
}

struct IpAddr {
    kind: IpAddrKind,
    address: String
}

fn ip_addresses() {
    let home = IpAddr {
        kind: IpAddrKind::V4,
        address: String::from("127.0.0.1")
    };

    let loopback = IpAddr {
        kind: IpAddrKind::V6,
        address: String::from("::1")
    };
}

// pattern matching on the enum
//fn which_ip_type(ip: IpAddr) -> &str {
//    match ip.ip_type {
//        IpAddrKind::V4 => "IP Version 6!",
//        IpAddrKind::v4 => "IP Version 4!"
//    }
//}

// We could also represent the above *without* a Struct
enum IpAddress {
    V4(String),
    V6(String)
}

fn ip_address() {
    let home = IpAddress::V4(String::from("127.0.0.1"));
    let loopback = IpAddress::V6(String::from("::1"));
}

/** Example: Embedded types in an Enum **/
// TODO: - Do more with this enum eventually
enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(i32, i32, i32)
}

impl Message {
    fn call(&self) -> &str {
      "Message has been called!"
    }
}

/** Example: US State and Coin Enum **/

#[derive(Debug)]
enum UsState {
    Alabama,
    Alaska,
    NewYork
}

enum Coin {
    Penny,
    Nickel,
    Dime,
    Quarter(UsState)
}

fn coin_value_in_cents(coin: Coin) -> u32 {
    match coin {
        Coin::Penny => 1,
        Coin::Nickel => 5,
        Coin::Dime => 10,
        Coin::Quarter(state) => {
            println!("State quarter from {:?}!", state);
            25
        }
    }
}



fn coin_test() {
     let test_coin = Coin::Quarter(UsState::Alabama);
     coin_value_in_cents(test_coin);
}

/** Topic: Option<T> **/

fn option_stuff() {
    let x: i8 = 5;
    let y: Option<i8> = Some(5);
}

// Various ways of dealing with Optionals
// match!
fn plus_one(x: Option<i32>) -> Option<i32> {
    match x {
        None => None,
        Some(i) => Some(i + 1)
    }
}

fn option_test() {
    let five = Some(5);
    let six = plus_one(five);
    let none = plus_one(None);

    println!("{:#?}", plus_one(six));
    println!("{:#?}", plus_one(none));


    let some_u8_value = 0u8;

    // match!
    let xx = match some_u8_value {
        1 => println!("one"),
        2 => println!("two"),
        _ => ()
    };

    println!("{:?}", xx);

    let some_other_u8_value = Some(0u8);

    // if let!
    if let Some(0u8) = some_other_u8_value {
        println!("It is 0u8!!!!!!!")
    }
}

/** Example: Count all non-Quarter Coins using either match or if-let **/

fn non_quarter_coin_count_match(coin: Coin) {
    let mut count = 0;
    match coin {
        Coin::Quarter(state) => println!("State quarter from {:?}!", state),
        _ => count += 1,
    }
}

fn non_quarter_coin_count_if_let(coin: Coin) {
    let mut count = 0;
    if let Coin::Quarter(state) = coin {
        println!("State quarter from {:?}!", state);
    } else {
        count += 1;
    }
}

fn main() {

}



