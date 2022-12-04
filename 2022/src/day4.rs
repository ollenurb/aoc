pub use crate::util::BiFunctor;

type InputItem = (Range, Range);

type Range = (usize, usize);

// Simple line parsing routine
pub fn parse_line(str: String) -> InputItem {
    str.split_once(',')
       .unwrap()
       .bimap(|r| {
           r.split_once('-')
            .unwrap()
            .bimap(|v| v.parse().unwrap())
       })
}

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) {
    let content: Vec<InputItem> = content.map(|s| parse_line(s)).collect();
    solve_first(&content);
    solve_second(&content);
}

pub fn solve_first(content: &Vec<InputItem>) {
    let res: usize = content.iter().filter_map(|(r1, r2)| {
        if r1.0 <= r2.0 && r1.1 >= r2.1 {
            Some(1)
        } else if r2.0 <= r1.0 && r2.1 >= r1.1 {
            Some(1)
        } else {
            None
        }
    })
    .sum();

    println!("First Part: {}", res);
}

pub fn solve_second(content: &Vec<InputItem>) {
    let res: usize = content.iter().filter_map(|(r1, r2)| {
        if r1.0 <= r2.0 && r1.1 >= r2.0 {
            Some(1)
        } else if r2.0 <= r1.0 && r2.1 >= r1.0 {
            Some(1)
        } else {
            None
        }
    })
    .sum();

    println!("Second Part: {}", res);
}
