#!/bin/bash

main() {
    THREADS="[1,2,4,6,8,10,12,14,16]"
    TX_SIZE="[(1,1),(2,1),(3,1),(4,1),(5,1)]"
    NUM_TX="200000"
    NUM_PREFILL="1000000"

    mkdir -p "results/1"
    bench $THREADS $NUM_TX "[(1,1)]" 0       "(1,0,0,0)" "results/1/insert"
    bench $THREADS $NUM_TX "[(1,1)]" $NUM_TX "(0,1,0,0)" "results/1/update"
    bench $THREADS $NUM_TX "[(1,1)]" $NUM_TX "(0,0,1,0)" "results/1/lookup"
    bench $THREADS $NUM_TX "[(1,1)]" $NUM_TX "(0,0,0,1)" "results/1/delete"

    mkdir -p "results/5"
    bench $THREADS $NUM_TX $TX_SIZE 0            "(70,10,10,10)" "results/update/5/insert"
    bench $THREADS $NUM_TX $TX_SIZE $NUM_PREFILL "(10,70,10,10)" "results/update/5/update"
    bench $THREADS $NUM_TX $TX_SIZE $NUM_PREFILL "(10,10,70,10)" "results/update/5/lookup"
    bench $THREADS $NUM_TX $TX_SIZE $NUM_PREFILL "(10,10,10,70)" "results/update/5/delete"    
    bench $THREADS $NUM_TX $TX_SIZE $NUM_PREFILL "(25,25,25,25)" "results/update/5/balanced-prefill"

#    sudo shutdown -h now
}

bench() {
    unbuffer cabal bench bench --benchmark-options="$1 $2 $3 $4 $5 +RTS -T" | tee "$6.txt"
#    curl -F $6.txt=@$6.txt https://XXXXXXXXXXXXXXX@neocities.org/api/upload
}

main "$@"
