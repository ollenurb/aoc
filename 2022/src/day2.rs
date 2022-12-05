type InputItem = (usize, usize);

// Simple line parsing routine
pub fn parse_line(str: String) -> InputItem {
    let fst = str.chars().nth(0).unwrap();
    let snd = str.chars().nth(2).unwrap();
    (fst as usize, snd as usize)
}

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) -> (String, String) {
    let content: Vec<InputItem> = content.map(|s| parse_line(s)).collect();
    (
        solve_first(&content).to_string(),
        solve_second(&content).to_string()
    )
}

fn compl(op: usize) -> usize {
    (op % 3) + 1
}

pub fn solve_first(content: &Vec<InputItem>) -> usize {
    content.iter().fold(0, |acc, i| {
        let (opp, me) = (i.0 - 64, i.1 - 87);

        me + if opp == me {
            3
        } else if compl(me) == opp {
            0
        } else {
            6
        } + acc
    })
}

fn solve_second(content: &Vec<InputItem>) -> usize {
    content.iter().fold(0, |acc, i| {
        let (opp, outcome) = (i.0 - 64, (i.1 - 88) * 3);

        outcome
            + acc
            + match outcome {
                0 => compl(compl(opp)),
                3 => opp,
                6 => compl(opp),
                _ => panic!("Unable to handle this"),
            }
    })
}
