mod parse;

use parse::{NaiveParser, OptimizedParser, RegexParser};

fn correct_word(n: usize) -> String {
    // can only start with a
    let mut words = vec!["a".to_string()];

    for _ in 0..rand::random_range(0..n) {
        // 50% to choose alternative in (a|b\1b)*

        if rand::random_bool(0.5) {
            words.push("a".to_string());
        } else {
            words.push("b".to_string() + words.last().unwrap() + "b");
        }
    }

    let left = words.join("");

    let n = words
        .last()
        .unwrap()
        .as_bytes()
        .iter()
        .filter(|x| **x == b'b')
        .count()
        / 2;

    let middle = if rand::random_bool(0.5) {
        // Generate b^n a b^n

        "b".repeat(n) + "a" + &"b".repeat(n)
    } else {
        // Generate b^n a b^m a b^n

        let m = if n == 0 {
            0
        } else {
            rand::random_range(n..2 * n)
        };

        "b".repeat(n) + "a" + &"b".repeat(m) + "a" + &"b".repeat(n)
    };

    let right = words.last().unwrap().clone();

    left + "c" + &middle + "c" + &right
}

fn random_word(n: usize) -> String {
    (0..5)
        .map(|i| (i, rand::random_range(0..n)))
        .map(|(i, m)| {
            if i % 2 == 1 {
                "c".to_string()
            } else {
                (0..m)
                    .map(|_| if rand::random_bool(0.5) { 'a' } else { 'b' })
                    .collect::<String>()
            }
        })
        .collect()
}

fn fuzz(max_tests: usize) -> bool {
    (0..max_tests).all(|i| {
        let (expected, w) = if rand::random_bool(0.5) {
            (false, random_word(1000))
        } else {
            (true, correct_word(100))
        };

        let naive = NaiveParser::try_parse(&w);
        let optim = OptimizedParser::try_parse(&w);

        if naive != optim {
            let re = RegexParser::try_parse(&w);

            if re.is_some() {
                if re != naive {
                    println!("Naive parser failed. w: {w}");
                    println!("Naive: {naive:?}");
                    println!("RE: {re:?}");
                    return false;
                }

                if re != optim {
                    println!("Optimized parser failed. w: {w}");
                    println!("Optimized: {optim:?}");
                    println!("RE: {re:?}");
                    return false;
                }
            }

            if expected {
                if naive != Some(expected) {
                    println!("Naive parser failed. w: {w}");
                    println!("Naive: {naive:?}");
                    println!("Expected (may be false): {expected:?}");
                    return false;
                }

                if optim != Some(expected) {
                    println!("Optimized parser failed. w: {w}");
                    println!("Optimized: {optim:?}");
                    println!("Expected (may be false): {expected:?}");
                    return false;
                }
            } else {
                println!("may be false positive. w: {w}");
            }
        }

        println!("{i}");

        true
    })
}

fn bench_correct() {
    use std::time::Instant;

    let mut data = vec![];

    for (m, n) in [
        (5000, 2500),
        (10000, 5000),
        (75000, 50000),
        (750000, 500000),
        (1500000, 1000000),
    ] {
        let mut correct_words = vec![];

        while correct_words.len() != 10_000 {
            let w = correct_word(n);

            if w.len() >= m {
                correct_words.push(w);
            }
        }

        let avg_length = correct_words.iter().map(|x| x.len()).sum::<usize>() / correct_words.len();

        {
            let now = Instant::now();
            let mut correct = 0;

            for word in &correct_words {
                if NaiveParser::parse(&word) {
                    correct += 1;
                }
            }

            let elapsed = now.elapsed() / (correct_words.len() as u32);

            data.push((avg_length, "naive", elapsed, correct));
        }

        {
            let now = Instant::now();
            let mut correct = 0;

            for word in &correct_words {
                if OptimizedParser::parse(&word) {
                    correct += 1;
                }
            }

            let elapsed = now.elapsed() / (correct_words.len() as u32);

            data.push((avg_length, "optim", elapsed, correct));
        }
    }

    println!("{data:?}");
}

fn bench_random() {
    use std::time::Instant;

    let mut data = vec![];

    for (m, n) in [
        (5000, 2500),
        (10000, 5000),
        (75000, 50000),
        (750000, 500000),
        (1500000, 1000000),
    ] {
        let mut random_words = vec![];

        while random_words.len() != 10_000 {
            let w = correct_word(n);

            if w.len() >= m && w.len() <= 3 * m {
                random_words.push(w);
            }
            // println!("{}", random_words.len());
        }

        let avg_length = random_words.iter().map(|x| x.len()).sum::<usize>() / random_words.len();

        {
            let now = Instant::now();
            let mut correct = 0;

            for word in &random_words {
                if NaiveParser::parse(&word) {
                    correct += 1;
                }
            }

            let elapsed = now.elapsed() / (random_words.len() as u32);

            data.push((avg_length, "naive", elapsed, correct));
        }

        {
            let now = Instant::now();
            let mut correct = 0;

            for word in &random_words {
                if OptimizedParser::parse(&word) {
                    correct += 1;
                }
            }

            let elapsed = now.elapsed() / (random_words.len() as u32);

            data.push((avg_length, "optim", elapsed, correct));
        }
        println!("{data:?}");
    }

    println!("{data:?}");
}

fn main() {
    // if fuzz(5000) {
    //     println!("OK");
    // } else {
    //     println!("Failed");
    // }

    // bench_correct();
    // bench_random();
}
