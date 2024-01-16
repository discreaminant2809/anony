use tokio::select;
use tokio::sync::mpsc;
use tokio::time::{sleep, Duration};

#[tokio::main]
async fn main() {
    let (tx1, rx1) = mpsc::channel(32);
    let (tx2, rx2) = mpsc::channel(32);
    let (cancel_tx, mut cancel_rx) = mpsc::channel(1);

    // Spawn a task to simulate sending values to channels
    tokio::spawn(async move {
        for i in 0..5 {
            if tx1.send(i).await.is_err() {
                break;
            }
            if tx2.send(format!("Message {}", i)).await.is_err() {
                break;
            }
            sleep(Duration::from_millis(500)).await;
        }
    });

    // Spawn a task to simulate a cancellation after a delay
    tokio::spawn(async move {
        sleep(Duration::from_millis(5_008)).await;
        cancel_tx.send(()).await.unwrap();
    });

    // It'd be random at this point: either the first or the second branch will be executed
    select! {
        _ = anony::join!(task_one(rx1), task_two(rx2)) => {
            println!("Both tasks completed");
        }
        _ = cancel_rx.recv() => {
            println!("Operation was cancelled");
        }
    }
}

async fn task_one(mut rx: mpsc::Receiver<i32>) {
    while let Some(value) = rx.recv().await {
        println!("Task one received: {}, Now processing", value);
        // Simulate work
        sleep(Duration::from_secs(1)).await;
        println!(r#"Task one's completed the message "{}""#, value);
    }
}

async fn task_two(mut rx: mpsc::Receiver<String>) {
    while let Some(value) = rx.recv().await {
        println!("Task two received: {}. Now processing", value);
        // Simulate work
        sleep(Duration::from_secs(1)).await;
        println!(r#"Task two's completed the message "{}""#, value);
    }
}
