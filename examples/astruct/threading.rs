use std::{
    io,
    num::NonZeroUsize,
    panic::{catch_unwind, AssertUnwindSafe},
    thread::{self, JoinHandle},
};

use anony::r#struct;
use scopeguard::{guard, ScopeGuard};

/// Imagine we're implementing a thread pool
///
/// If you still remember, we've done this in the Rust Book!
/// But this time we'll use more complex language's features, and demomnstrate our [`r#struct!`] macro
pub struct ThreadPool {
    threads: Vec<JoinHandle<()>>,
    task_sender: spmc::Sender<Task>,
}

impl ThreadPool {
    // I like using `NonZero` types whenever I wanna tell that zero values are forbidden
    pub fn new(n_threads: NonZeroUsize) -> Result<Self, io::Error> {
        let (task_sender, task_receiver) = spmc::channel();

        // In this case we have 2 guarded subjects that we can't just have (mutable) references to
        // because of... the borrow checker!
        // To solve this, normally we'd create another local struct containing two fields for the 2 subjects
        // But `r#struct!` handles this boilerplate for us!
        // The guard calls the destructor fn when there's an error while spawning threads, or panic
        // We have to ensure that all spawned threads are destroyed
        let mut guard = guard(
            r#struct! {
                // How many threads have been spawned?
                n_spawned: 0_usize,
                // On something going wrong, where should we signal all spanwed threads to?
                task_sender,
            },
            |mut guard| {
                for _ in 0..guard.n_spawned {
                    guard.task_sender.send(Task::Terminate).unwrap();
                }
            },
        );

        (0..n_threads.get())
            .map(|_| {
                let task_receiver = task_receiver.clone();
                let thread = thread::Builder::new().spawn(move || {
                    while let Task::Execute { f } = task_receiver.recv().unwrap() {
                        let _ = catch_unwind(AssertUnwindSafe(f));
                    }
                })?;

                guard.n_spawned += 1;

                Ok(thread)
            })
            .collect::<Result<_, _>>()
            .map(|threads| ThreadPool {
                threads,
                task_sender: ScopeGuard::into_inner(guard).task_sender,
            })
    }

    pub fn spawn(&mut self, f: impl FnOnce() + Send + 'static) {
        self.task_sender
            .send(Task::Execute { f: Box::new(f) })
            .unwrap();
    }

    #[inline]
    pub fn shutdown(mut self) {
        self.terminate_all_threads();
    }

    fn terminate_all_threads(&mut self) {
        for _ in 0..self.threads.len() {
            self.task_sender.send(Task::Terminate).unwrap();
        }

        std::mem::take(&mut self.threads)
            .into_iter()
            .map(JoinHandle::join)
            .for_each(Result::unwrap);
    }
}

impl Drop for ThreadPool {
    #[inline]
    fn drop(&mut self) {
        self.terminate_all_threads();
    }
}

enum Task {
    Execute {
        f: Box<dyn FnOnce() + Send + 'static>,
    },
    Terminate,
}
