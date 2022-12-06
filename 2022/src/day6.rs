use std::collections::HashSet;

// Solve both problems of this day
pub fn solve(mut content: impl Iterator<Item = String>) -> (String, String) {
    let content: String = content.next().unwrap();
    (solve_first(&content), solve_second(&content))
}

fn solve_first(content: &String) -> String {
    let size = 4;

    let index = content
        .as_bytes()
        .windows(size)
        .map(|w| w.iter().collect::<HashSet<&u8>>().len() == size)
        .take_while(|v| !*v)
        .count();

    (index + size).to_string()
}

fn solve_second(content: &String) -> String {
    let size = 14;

    let index = content
        .as_bytes()
        .windows(size)
        .map(|w| w.iter().collect::<HashSet<&u8>>().len() == size)
        .take_while(|v| !*v)
        .count();

    (index + size).to_string()
}

