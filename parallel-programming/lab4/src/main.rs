use std::{
    fmt::Display, //
    sync::Arc,
    time::Duration,
};

use tokio::{
    sync::{Mutex, mpsc},
    time,
};

const NUM_PHILOSOPHERS: usize = 5;
const MIN_TIME: u64 = 1000;
const MAX_TIME: u64 = 5000;

#[derive(Clone, Debug)]
struct Message {
    id: usize,
    action: String,
}

impl Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}: {}", self.id, self.action)
    }
}

struct ForkPool {
    forks: Vec<Arc<Mutex<()>>>,
}

impl ForkPool {
    fn new(num_forks: usize) -> Self {
        let forks = (0..num_forks).map(|_| Arc::new(Mutex::new(()))).collect();

        Self { forks }
    }

    fn get_forks(&self, id: usize) -> (Arc<Mutex<()>>, Arc<Mutex<()>>) {
        let left = self.forks[id].clone();
        let right = self.forks[(id + 1) % self.forks.len()].clone();

        (left, right)
    }
}

async fn philosopher(
    id: usize,
    forks: Arc<ForkPool>,
    tx: mpsc::Sender<Message>,
) -> anyhow::Result<()> {
    loop {
        let thinking_time = gen_time().await;
        tx.send(Message {
            id,
            action: "think".to_string(),
        })
        .await?;
        time::sleep(Duration::from_millis(thinking_time)).await;

        tx.send(Message {
            id,
            action: "wait".to_string(),
        })
        .await?;

        let (left, right) = forks.get_forks(id);

        if id + 1 == NUM_PHILOSOPHERS {
            let _right_lock = right.lock().await;
            tx.send(Message {
                id,
                action: format!("took right fork, id = {}", (id + 1) % NUM_PHILOSOPHERS),
            })
            .await?;

            let _left_lock = left.lock().await;
            tx.send(Message {
                id,
                action: format!("took left fork, id = {id}"),
            })
            .await?;
        } else {
            let _left_lock = left.lock().await;
            tx.send(Message {
                id,
                action: format!("took left fork, id = {id}"),
            })
            .await?;

            let _right_lock = right.lock().await;
            tx.send(Message {
                id,
                action: format!("took right fork, id = {}", (id + 1) % NUM_PHILOSOPHERS),
            })
            .await?;
        }

        tx.send(Message {
            id,
            action: "eat".to_string(),
        })
        .await?;

        let eating_time = gen_time().await;
        time::sleep(Duration::from_millis(eating_time)).await;

        tx.send(Message {
            id,
            action: format!("put right fork, id = {}", (id + 1) % NUM_PHILOSOPHERS),
        })
        .await?;

        tx.send(Message {
            id,
            action: format!("put left fork, id = {}", id),
        })
        .await?;

        tx.send(Message {
            id,
            action: "wait".to_string(),
        })
        .await?;
    }
}

async fn gen_time() -> u64 {
    use rand::Rng;
    let mut rng = rand::rng();
    rng.random_range(MIN_TIME..=MAX_TIME)
}

#[tokio::main]
async fn main() {
    let (tx, mut rx) = mpsc::channel::<Message>(100);
    let forks = Arc::new(ForkPool::new(NUM_PHILOSOPHERS));

    (0..NUM_PHILOSOPHERS).for_each(|i| {
        let tx_clone = tx.clone();
        let forks_clone = forks.clone();

        tokio::spawn(async move {
            philosopher(i, forks_clone, tx_clone).await.unwrap();
        });
    });

    drop(tx);

    let timeout = time::sleep(Duration::from_secs(10));
    tokio::pin!(timeout);

    loop {
        tokio::select! {
            Some(msg) = rx.recv() =>
                println!("{msg}"),

            _ = &mut timeout =>
                break,

            else => break,
        }
    }
}
