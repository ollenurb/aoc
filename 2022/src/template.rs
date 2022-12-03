type InputItem = usize;

// Simple line parsing routine
pub fn parse_line(str: String) -> InputItem {
    todo!("Parse one line in input file")
}

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) {
    let content: Vec<InputItem> = content.map(|s| parse_line(s)).collect();
    solve_first(&content);
    solve_second(&content);
}

pub fn solve_first(content: &Vec<InputItem>) {
    todo!("Implement First Part")
}

pub fn solve_second(content: &Vec<InputItem>) {
    todo!("Implement Second Part")
}
