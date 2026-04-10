#!/usr/bin/env bash
set -euo pipefail

REVIEWS_URL="https://mcauleylab.ucsd.edu/public_datasets/data/amazon_v2/categoryFiles/Sports_and_Outdoors.json.gz"
META_URL="https://mcauleylab.ucsd.edu/public_datasets/data/amazon_v2/metaFiles2/meta_Sports_and_Outdoors.json.gz"
REVIEWS_5CORE_URL="https://mcauleylab.ucsd.edu/public_datasets/data/amazon_v2/categoryFilesSmall/Sports_and_Outdoors_5.json.gz"
RATINGS_URL="https://mcauleylab.ucsd.edu/public_datasets/data/amazon_v2/categoryFilesSmall/Sports_and_Outdoors.csv"

mkdir -p "data"
REVIEWS_GZ="data/Sports_and_Outdoors.json.gz"
META_GZ="data/meta_Sports_and_Outdoors.json.gz"
REVIEWS_5CORE_GZ="data/Sports_and_Outdoors_5.json.gz"
RATINGS_CSV="data/Sports_and_Outdoors.csv"

download_if_missing() {
  local url="$1"
  local output="$2"

  if [[ -f "$output" ]]; then
    echo "Found: $output"
    return
  fi

  echo "Downloading: $url"
  curl -fL --retry 3 --retry-delay 2 -o "$output" "$url" &>/dev/null
  echo "Downloaded: $output"
}

unpack_if_missing() {
  local input_gz="$1"
  local output_json="${input_gz%.gz}"

  if [[ -f "$output_json" ]]; then
    echo "Already unpacked: $output_json"
    return
  fi

  echo "Unpacking: $input_gz -> $output_json"
  gzip -dc "$input_gz" > "$output_json"
}

download_if_missing "$REVIEWS_URL" "$REVIEWS_GZ" && unpack_if_missing "$REVIEWS_GZ" &
pid_reviews=$!
download_if_missing "$META_URL" "$META_GZ" && unpack_if_missing "$META_GZ" &
pid_meta=$!
download_if_missing "$REVIEWS_5CORE_URL" "$REVIEWS_5CORE_GZ" && unpack_if_missing "$REVIEWS_5CORE_GZ" &
pid_reviews_5core=$!
download_if_missing "$RATINGS_URL" "$RATINGS_CSV" &
pid_ratings=$!

wait "$pid_reviews"
wait "$pid_meta"
wait "$pid_reviews_5core"
wait "$pid_ratings"
