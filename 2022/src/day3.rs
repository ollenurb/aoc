use std::collections::HashSet;

type InputItem = String;

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) {
    let content: Vec<InputItem> = content.collect();
    solve_first(&content);
    solve_second(&content);
}

// Returns the score of a given character
pub fn score(c: char) -> u32 {
    match c {
        'a'..='z' => c as u32 - 96,
        'A'..='Z' => c as u32 - 38,
        _ => unreachable!(),
    }
}

pub fn solve_first(content: &Vec<InputItem>) {

    let res: u32 = content.iter()
        .flat_map(|l| {
            let mid = l.len() / 2;
            let (fh, sh) = l.split_at(mid);

            let mut sh: HashSet<char> = HashSet::from_iter(sh.chars());

            fh.chars().filter_map(move |c| {
                if sh.remove(&c) {
                    Some(score(c))
                } else {
                    None
                }
            })

        }).sum();

    println!("{}", res);
}

pub fn solve_second(content: &Vec<InputItem>) {
    todo!("Implement Second Part")
}
