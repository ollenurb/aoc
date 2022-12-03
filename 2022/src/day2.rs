type InputItem = (u32, u32);

// Simple line parsing routine
pub fn parse_line(str: String) -> InputItem {
    let fst = str.chars().nth(0).unwrap();
    let snd = str.chars().nth(2).unwrap();
    (fst as u32, snd as u32)
}

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) {
    // Parse the file content
    let content: Vec<InputItem> = content.map(|s| parse_line(s)).collect();
    // Pass the parsed content to the solvers
    solve_first(&content);
    solve_second(&content);
}

fn compl(op: u32) -> u32 {
    (op % 3) + 1
}

pub fn solve_first(content: &Vec<InputItem>) {
    let res = content.iter()
        .fold(0, |acc, i| {
            let (opp, me) = (i.0 - 64, i.1 - 87);

            me + if opp == me {
                3
            } else if compl(me) == opp  {
                0
            } else {
                6
            } + acc
        });

    println!("First Part {}", res);
}

pub fn solve_second(content: &Vec<InputItem>) {
    let res = content.iter()
        .fold(0, |acc, i| {
            let (opp, outcome) = (i.0 - 64, (i.1 - 88) * 3);

            outcome + acc + match outcome {
                0 => compl(compl(opp)),
                3 => opp,
                6 => compl(opp),
                _ => panic!("Unable to handle this")
            }
        });

    println!("Second Part {}", res);
}
