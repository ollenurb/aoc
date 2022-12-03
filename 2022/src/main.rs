mod day1;
mod day2;
mod day3;

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
        .map(|line| line.expect("Failed to read line from data file"))
}

fn main() {
    let in3 = load_input(3);



    day3::solve(in3);
}