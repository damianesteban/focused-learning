/** Topic: Data Structures
- Vector
- String
- Hash Map
**/

// Vectors
fn vectors() {
    // How to create vectors
    let v: Vec<i32> = Vec::new();
    let vv = vec![1, 2, 3];

    // mutable
    let mut vvv = Vec::new();
    vvv.push(6);

    for i in &mut vvv {
        *i += 50;
    }

    // Reading vectors
    let xs = vec![1, 2, 3, 4, 5];

    let third: &i32 = &v[2];
    let third: Option<&i32> = v.get(2);

}

fn enum_vectors() {
    enum SpreadSheetCell {
        Int(i32),
        Float(f64),
        Text(String)
    }

    let row = vec![
        SpreadSheetCell::Int(3),
        SpreadSheetCell::Text(String::from("blue")),
        SpreadSheetCell::Float(10.12)
    ];
}

// Strings
fn strings() {
    // new, empty string
    let mut s = String::new();

    let data = "initial contents";

    let s = data.to_string();

    // Updating a string
    let mut ss = String::from("foo");
    ss.push_str("barrrr");

    // Concatenation
    let s1 = String::from("Yo, ");
    let s2: String = String::from("Dude!");
    let s3 = s1 + &s2;

    // sliicing
    let hello = "hellllloooo";
    let spliced = &hello[0..4];
}

// Hash Maps
use std::collections::HashMap;

fn hash_maps() {
    let mut scores = HashMap::new();
    scores.insert(String::from("Blue"), 10);
    scores.insert(String::from("Yellow"), 15);

    // create hash map with collect
    let teams = vec![String::from("Blue"), String::from("Yellow")];
    let initial_scores = vec![10, 15];

    let collected_scores: HashMap<_, _> = teams.iter().zip(initial_scores.iter()).collect();
    println!("collected scores: {:?}", collected_scores);

    // Accessing Values
    let team_name = String::from("Blue");
    let blue_score = collected_scores.get(&team_name); // Optional

    // Iteration
    for (key, value) in &scores {
        println!("{}, {}", key, value);
    }
}

fn main() {
    hash_maps();
}
