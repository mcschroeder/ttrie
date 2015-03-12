#!/bin/bash

main() {
    THREADS="[1,2,4,6,8,10,12,14,16]"
    TX_SIZE="[(1,1),(2,1),(3,1),(4,1),(5,1)]"
    NUM_TX="200000"
    NUM_PREFILL="1000000"

    mkdir -p "results/1"
    bench $THREADS $NUM_TX $NUM_TX "[(1,1)]" "(1,0,0)" "results/1/insert"
    bench $THREADS $NUM_TX $NUM_TX "[(1,1)]" "(0,1,0)" "results/1/lookup"
    bench $THREADS $NUM_TX $NUM_TX "[(1,1)]" "(0,0,1)" "results/1/delete"

    mkdir -p "results/5"
    bench $THREADS 0            $NUM_TX $TX_SIZE "(80,10,10)" "results/5/insert"
    bench $THREADS $NUM_PREFILL $NUM_TX $TX_SIZE "(10,80,10)" "results/5/lookup"
    bench $THREADS $NUM_PREFILL $NUM_TX $TX_SIZE "(10,10,80)" "results/5/delete"    
    bench $THREADS 0            $NUM_TX $TX_SIZE "(33,33,33)" "results/5/balanced"

    #sudo shutdown -h now
}

# bench threads num_prefill num_tx tx_size op_dist name
# op_dist = (inserts,updates,lookups,deletes)
bench() {
    unbuffer cabal bench bench1 --benchmark-options="$1 $2 $3 $4 $5 --regress allocated:iters +RTS -T" |
    tee "$6.txt"
}

main "$@"
