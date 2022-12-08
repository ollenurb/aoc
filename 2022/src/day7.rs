use std::collections::HashMap;
type InputItem = Entry;

#[derive(Debug)]
enum Entry {
    Directory(String),
    List(),
    File(u32),
    PopDir(),
    PushDir(String),
}

// Simple line parsing routine
fn parse_line(str: String) -> Entry {
    let mut tokens = str.split(' ');

    tokens
        .next()
        .and_then(|c| match c {
            "$" => tokens.next().and_then(|cmd| match cmd {
                "cd" => tokens.next().map(|dir| {
                    if dir != ".." { Entry::PushDir(dir.to_string()) }
                    else { Entry::PopDir() }
                }),
                "ls" => Some(Entry::List()),
                _ => None,
            }),
            "dir" => tokens.next().map(|dir| Entry::Directory(dir.to_string())),
            n => Some(Entry::File(n.parse().unwrap())),
        })
        .unwrap()
}

fn compute_dir_sizes(content: &Vec<InputItem>) -> Vec<u32> {
    let mut dir_sizes: HashMap<String, u32> = HashMap::new();
    let mut cur_dir: Vec<&str> = Vec::new();

    content.iter().for_each(|e| match e {
        Entry::PushDir(name) => {
            cur_dir.push(&name);
        },
        Entry::PopDir() => {
            cur_dir.pop();
        },
        Entry::File(size) => {
            cur_dir.iter()
                .scan(String::new(), |dir, cur| {
                    dir.push_str(cur);
                    Some(dir.clone())
                })
                .for_each(|d| {
                    let tot_size = dir_sizes.entry(d).or_insert(0);
                    *tot_size += size;
                });
        }
        Entry::List() => { },
        Entry::Directory(_) => { },
    });

    dir_sizes.values().map(|e| *e).collect()
}


// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) -> (String, String) {
    let content: Vec<InputItem> = content.map(|s| parse_line(s)).collect();
    (solve_first(&content), solve_second(&content))
}

fn solve_first(content: &Vec<InputItem>) -> String {
    compute_dir_sizes(&content)
        .iter()
        .filter(|s| s < &&100000)
        .sum::<u32>()
        .to_string()
}

fn solve_second(content: &Vec<InputItem>) -> String {
    let sizes = compute_dir_sizes(&content);
    let root_sz = sizes.iter().max().unwrap();
    let unused = 70000000 - root_sz;
    sizes
        .iter()
        .filter(|s| (*s + unused) > 30000000)
        .min().unwrap()
        .to_string()
}
