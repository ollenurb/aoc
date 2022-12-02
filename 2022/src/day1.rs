pub fn solve(content: impl Iterator<Item = String>) {
    // Parse the content
    let content: Vec<Option<u32>> = content.map(|s| s.parse().ok()).collect();
    solve_first(&content);
    solve_second(&content);
}

pub fn solve_first(content: &Vec<Option<u32>>) {
    let (_, result) = content.iter().fold((0, 0), |state: (u32, u32), value| {
        let (counter, max) = state;

        match value {
            Some(val) => (val + counter, max),
            None => (0, if counter > max { counter } else { max }),
        }
    });

    println!("First Part: {}", result);
}

pub fn solve_second(content: &Vec<Option<u32>>) {
    let (mut buckets, _) = content.iter().fold((Vec::new(), 0), |state: (Vec<u32>, u32), value| {
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
    let result: u32 = buckets[..3].iter().sum();

    println!("Second Part: {}", result);
}
