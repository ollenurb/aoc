mod day1;
mod day2;
mod day3;
mod day4;
pub mod util;

use std::io::{BufRead, BufReader};

// Load the input file given a particular day
fn load_input(day: usize) -> impl Iterator<Item = String> {
    let file = std::fs::OpenOptions::new()
        .read(true)
        .open(format!("inputs/{day}.txt"))
        .expect(&format!("Failed to access data for day{day}"));

    let buffered_file = BufReader::new(file);

    buffered_file
        .lines()
        .map(|line| line.expect("Failed to read line from input file"))
}

fn main() {
    let day: usize = std::env::args()
        .nth(1)
        .expect("Usage: cargo run -- [day]")
        .parse()
        .expect("Invalid day number");

    let input = load_input(day);
    let solution = match day {
        1 => day1::solve(input),
        2 => day2::solve(input),
        3 => day3::solve(input),
        4 => day4::solve(input),
        _ => unreachable!(),
    };

    println!("================ Day {:0>2} ================", day);
    println!("First Part: {}", solution.0);
    println!("Second Part: {}", solution.1);
    println!("========================================");
}
