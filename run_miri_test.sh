cargo +nightly miri t --all-features \
    --test join \
    --test join_cyclic \
    --test try_join \
    --test try_join_cyclic \
    -- should_poll_all_and_in_correct_ord