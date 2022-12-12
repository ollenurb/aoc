struct Matrix {
    pub rows: usize,
    pub cols: usize,
    pub data: Vec<Vec<usize>>,
}

// Simple line parsing routine
fn parse_line(str: String) -> Vec<usize> {
    str.chars()
        .filter_map(|s| s.to_digit(10))
        .map(|s| s as usize)
        .collect()
}

fn is_visible(r: usize, c: usize, m: &Matrix) -> bool {
    if r == 0 || c == 0 || r == m.rows-1 || c == m.cols-1 {
        return true;
    }

    let cur = m.data[r][c];

    // left
    (c+1..m.cols)
        .map(|i| m.data[r][i])
        .all(|x| x < cur) ||
    // right
    (0..c)
        .map(|i| m.data[r][i])
        .all(|x| x < cur) ||
    // down
    (r+1..m.rows)
        .map(|i| m.data[i][c])
        .all(|x| x < cur) ||
    // up
    (0..r)
        .map(|i| m.data[i][c])
        .all(|x| x < cur)
}

// Solve both problems of this day
pub fn solve(content: impl Iterator<Item = String>) -> (String, String) {
    let content: Vec<Vec<usize>> = content.map(|s| parse_line(s)).collect();

    let content = Matrix {
        rows: content.len(),
        cols: content.first().unwrap().len(),
        data: content,
    };
    
    (solve_first(&content), solve_second(&content))
}

fn solve_first(content: &Matrix) -> String {
    (0..content.rows)
        .flat_map(|r| (0..content.cols).map(move |c| (r, c)))
        .filter(|(r, c)| is_visible(*r, *c, &content))
        .count()
        .to_string()
}

fn solve_second(content: &Matrix) -> String {
    "To solve".to_string()
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_first() {
        println!("Helo woldo");
    }

}
