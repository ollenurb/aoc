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
        .map(|l| {
            let mid = l.len() / 2;
            let (fh, sh) = l.split_at(mid);

            // Create 2 sets from the splitted string
            let sh: HashSet<char> = HashSet::from_iter(sh.chars());
            let fh: HashSet<char> = HashSet::from_iter(fh.chars());

            // Intersect then sum
            sh.intersection(&fh)
              .map(|f| score(*f))
              .sum::<u32>()
        }).sum();

    println!("First Part: {}", res);
}

pub fn solve_second(content: &Vec<InputItem>) {
    let res: u32 = content.chunks(3)
        .map(|c| {
            c.iter()
             .map(|r| HashSet::from_iter(r.chars()))
             .reduce(|int: HashSet<char>, set| int.intersection(&set).map(|i| *i).collect())
             .unwrap()
             .iter()
             .map(|c| score(*c))
             .sum::<u32>()
        }).sum();

    println!("Second Part {}", res);
}
