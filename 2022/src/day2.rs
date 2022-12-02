
type InputItem = (GameResult, GameResult);

pub enum GameResult {
    Rock(),
    Paper(),
    Scissor(),
}

// Convert a character to a particular GameResult
fn char_to_result(chr: char) -> GameResult {
    match chr {
        'A' | 'X' => GameResult::Rock(),
        'B' | 'Y' => GameResult::Paper(),
        'C' | 'Z' => GameResult::Scissor(),
        _ => panic!("Unsupported format")
    }
}

// Simpe parsing line
pub fn parse_line(str: String) -> InputItem {
    let fst = str.chars().nth(0).unwrap();
    let snd = str.chars().nth(1).unwrap();

    (char_to_result(fst), char_to_result(snd))
}

pub fn solve(content: impl Iterator<Item = String>) {
    // Parse the content
    let content: Vec<InputItem> = content.map(|s| parse_line(s)).collect();
    solve_first(&content);
    solve_second(&content);
}

pub fn solve_first(content: &Vec<InputItem>) {
    content.iter()
           .map(|s| match s {

    
           })

    todo!("I'm working on it!")
}

pub fn solve_second(content: &Vec<InputItem>) {
    todo!("I'm working on it!")
}
