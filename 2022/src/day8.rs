struct Matrix {
    pub rows: usize,
    pub cols: usize,
    pub data: Vec<Vec<usize>>,
}

pub trait IterExt: Iterator {
    fn count_while_incl<P>(self, predicate: P) -> usize
    where
        Self: Sized,
        P: Fn(&Self::Item) -> bool,
    {
        self.scan(true, move |state, x| {
            if *state {
                *state = predicate(&x);
                Some(x)
            } else {
                None
            }
        })
        .count()
    }
}

impl <I: Iterator> IterExt for I { } 

// Simple line parsing routine
fn parse_line(str: String) -> Vec<usize> {
    str.chars()
        .filter_map(|s| s.to_digit(10))
        .map(|s| s as usize)
        .collect()
}

fn is_visible(r: usize, c: usize, m: &Matrix) -> bool {
    if r == 0 || c == 0 || r == m.rows - 1 || c == m.cols - 1 {
        return true;
    }

    let cur = m.data[r][c];

    // left
    (c+1..m.cols).all(|i| m.data[r][i] < cur) ||
    // right
    (0..c).all(|i| m.data[r][i] < cur) ||
    // down
    (r+1..m.rows).all(|i| m.data[i][c] < cur) ||
    // up
    (0..r).all(|i| m.data[i][c] < cur)
}

fn scenic_score(r: usize, c: usize, m: &Matrix) -> usize {
    if r == 0 || c == 0 || r == m.rows - 1 || c == m.cols - 1 {
        return 0;
    }

    let cur = m.data[r][c];

    // left
    let left = (0..c)
        .rev()
        .map(|i| m.data[r][i])
        .count_while_incl(|x| x < &cur);

    // right
    let right = (c + 1..m.cols)
        .map(|i| m.data[r][i])
        .count_while_incl(|x| x < &cur);

    // down
    let down = (r + 1..m.rows)
        .map(|i| m.data[i][c])
        .count_while_incl(|x| x < &cur);

    // up
    let up = (0..r)
        .rev()
        .map(|i| m.data[i][c])
        .count_while_incl(|x| x < &cur);

    let scores = vec![up, down, left, right];
    scores.iter().map(|s| s + (s == &0) as usize).product()
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
    (0..content.rows)
        .flat_map(|r| (0..content.cols).map(move |c| (r, c)))
        .map(|(r, c)| scenic_score(r, c, &content))
        // .inspect(|i| println!("sc: {}", i))
        .max()
        .unwrap()
        .to_string()
}

#[cfg(test)]
mod tests {

    use crate::day8::solve;
    #[test]
    fn test_first() {
        let content = "30373\n25512\n65332\n33549\n35390"
            .lines()
            .into_iter()
            .map(|s| s.to_string());
        let (s1, s2) = solve(content);
        println!("s1: {}, s2: {}", s1, s2);
    }
}
