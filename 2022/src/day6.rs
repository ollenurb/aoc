use std::collections::HashSet;

// Solve both problems of this day
pub fn solve(mut content: impl Iterator<Item = String>) -> (String, String) {
    let content: String = content.next().unwrap();
    (solve_first(&content), solve_second(&content))
}

fn solve_first(content: &String) -> String {
    content
        .chars()
        .scan(HashSet::new(), |state: &mut HashSet<char>, c| {
            // Manage the state
            if !state.contains(&c) {
                state.insert(c);
            } else {
                state.clear();
                state.insert(c);
            }

            // We reached the end of the header, stop here
            if state.len() == 4 {
                None
            } else {
                Some(c)
            }
        })
        .count()
        .to_string()
}

fn solve_second(content: &String) -> String {
    "Not yet implemented".to_string()
}
