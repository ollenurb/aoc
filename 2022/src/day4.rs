pub use crate::util::BiFunctor;

type InputItem = (Range, Range);

type Range = (usize, usize);

// Simple line parsing routine
fn parse_line(str: String) -> InputItem {
    str.split_once(',')
        .unwrap()
        .bimap(|r| r.split_once('-').unwrap().bimap(|v| v.parse().unwrap()))
}

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) -> (String, String) {
    let content: Vec<InputItem> = content.map(|s| parse_line(s)).collect();
    (
        solve_first(&content).to_string(),
        solve_second(&content).to_string()
    )
}

fn solve_first(content: &Vec<InputItem>) -> usize {
    content
        .iter()
        .filter_map(|(r1, r2)| {
            if r1.0 <= r2.0 && r1.1 >= r2.1 {
                Some(1)
            } else if r2.0 <= r1.0 && r2.1 >= r1.1 {
                Some(1)
            } else {
                None
            }
        })
        .sum()
}

fn solve_second(content: &Vec<InputItem>) -> usize {
    content
        .iter()
        .filter_map(|(r1, r2)| {
            if r1.0 <= r2.0 && r1.1 >= r2.0 {
                Some(1)
            } else if r2.0 <= r1.0 && r2.1 >= r1.0 {
                Some(1)
            } else {
                None
            }
        })
        .sum()
}
