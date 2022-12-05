type InputItem = usize;

// Simple line parsing routine
fn parse_line(str: String) -> InputItem {
    todo!("Parse one line in input file")
}

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) -> (String, String) {
    let content: Vec<InputItem> = content.map(|s| parse_line(s)).collect();
    (solve_first(&content), solve_second(&content))
}

fn solve_first(content: &Vec<InputItem>) -> usize {
    todo!("Implement First Part")
}

fn solve_second(content: &Vec<InputItem>) -> usize {
    todo!("Implement Second Part")
}
