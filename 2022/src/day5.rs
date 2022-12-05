use std::{str::FromStr, string::ParseError};

type Stacks = Vec<Vec<char>>;

#[derive(Clone, Copy)]
struct Instruction {
    pub quantity: usize,
    pub from: usize,
    pub to: usize,
}

impl FromStr for Instruction {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let nums: Vec<usize> = s.split(' ').filter_map(|x| x.parse().ok()).collect();

        Ok(Instruction {
            quantity: nums[0],
            from: nums[1],
            to: nums[2],
        })
    }
}

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) -> (String, String) {
    // Collect everything
    let content: Vec<_> = content.collect();

    // Consider only the "header" of this input file
    let mut first_chunk: Vec<String> = content.iter()
        .take_while(|s| !s.is_empty())
        .map(|s| s.chars().skip(1).step_by(4).collect())
        .collect();

    // Determine the number of skips that must be taken to ignore the header
    let n_skips = first_chunk.len() + 1;

    // We need the last line and to push the items in a reverse order
    first_chunk.reverse();
    let mut stacks: Stacks = first_chunk
        .remove(0)
        .chars()
        .map(|_| Vec::new())
        .collect();

    // Push the items to the corresponding stack
    first_chunk.iter()
        .for_each(|s| {
            s.chars()
             .enumerate()
             .filter(|(_, c)| !c.is_whitespace())
             .for_each(|(i, c)| stacks[i].push(c))
        });

    let instructions = content.iter()
        .skip(n_skips)
        .map(|s| s.parse().unwrap())
        .collect();

    (
        solve_first(stacks.clone(), &instructions),
        solve_second(stacks, &instructions)
    )
}


fn solve_first(mut stacks: Stacks, instructions: &Vec<Instruction>) -> String {

    instructions.iter().for_each(|i| {
        let lower = stacks[i.from - 1].len() - i.quantity;
        let removed: Vec<char> = stacks[i.from - 1].drain(lower..).collect();

        for ele in removed.iter().rev() {
            stacks[i.to - 1].push(*ele);
        }
    });


    stacks.iter()
        .filter_map(|s| s.last())
        .collect()

}

fn solve_second(mut stacks: Stacks, instructions: &Vec<Instruction>) -> String {

    instructions.iter().for_each(|i| {
        let lower = stacks[i.from - 1].len() - i.quantity;
        let removed: Vec<char> = stacks[i.from - 1].drain(lower..).collect();

        for ele in removed {
            stacks[i.to - 1].push(ele);
        }
    });

    stacks.iter()
        .filter_map(|s| s.last())
        .collect()
}

