type ItemType = Option<usize>;

pub fn solve(content: impl Iterator<Item = String>) -> (String, String) {
    // Parse the content
    let content: Vec<ItemType> = content.map(|s| s.parse().ok()).collect();
    (
        solve_first(&content).to_string(),
        solve_second(&content).to_string()
    )
}

fn solve_first(content: &Vec<ItemType>) -> usize {
    content.iter().fold((0, 0), |state: (usize, usize), value| {
        let (counter, max) = state;

        match value {
            Some(val) => (val + counter, max),
            None => (0, if counter > max { counter } else { max }),
        }
    }).1
}

fn solve_second(content: &Vec<ItemType>) -> usize {
    let (mut buckets, _) = content.iter().fold((Vec::new(), 0), |state: (Vec<usize>, usize), value| {
            let (mut buckets, acc) = state;

            match value {
                Some(val) => (buckets, acc + val),
                None => {
                    buckets.push(acc);
                    (buckets, 0)
                }
            }
        });

    // Sort buckets
    buckets.sort_by(|a, b| b.cmp(a));
    buckets[..3].iter().sum()
}
