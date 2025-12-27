use rand::Rng;
use std::{
    collections::{HashSet, LinkedList},
    sync::{Arc, RwLock},
    thread,
};

fn main() {
    let n = 4;
    let m = 100;

    let list = Arc::new(RwLock::new(LinkedList::new()));

    let mut handles = vec![];

    for _ in 0..n {
        let list_clone = Arc::clone(&list);

        let handle = thread::spawn(move || {
            let mut rng = rand::rng();
            for _ in 0..m {
                let num = rng.random_range(0..=1000);

                let exists = {
                    let list_read = list_clone.read().unwrap();
                    list_read.iter().any(|&x| x == num)
                };

                if !exists {
                    let mut list_write = list_clone.write().unwrap();

                    if !list_write.iter().any(|&x| x == num) {
                        list_write.push_back(num);
                    }
                }
            }
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    let list_read = list.read().unwrap();

    let unique = {
        let mut uniq = HashSet::new();
        list_read.iter().all(move |x| uniq.insert(x))
    };

    if unique {
        println!("ok");
    } else {
        println!("contains repeating numbers");
    }

    println!("total: {}", list_read.len());
}
