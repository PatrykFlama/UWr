#!/usr/bin/env bash
set -euo pipefail

REVIEWS_URL="https://mcauleylab.ucsd.edu/public_datasets/data/amazon_v2/categoryFiles/Sports_and_Outdoors.json.gz"
META_URL="https://mcauleylab.ucsd.edu/public_datasets/data/amazon_v2/metaFiles2/meta_Sports_and_Outdoors.json.gz"

mkdir -p "data"
REVIEWS_GZ="data/Sports_and_Outdoors.json.gz"
META_GZ="data/meta_Sports_and_Outdoors.json.gz"

download_if_missing() {
  local url="$1"
  local output="$2"

  if [[ -f "$output" ]]; then
    echo "Found: $output"
    return
  fi

  echo "Downloading: $url"
  if command -v curl >/dev/null 2>&1; then
    curl -fL --retry 3 --retry-delay 2 -o "$output" "$url"
  elif command -v wget >/dev/null 2>&1; then
    wget -O "$output" "$url"
  else
    echo "Error: neither curl nor wget is available." >&2
    exit 1
  fi
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

download_if_missing "$REVIEWS_URL" "$REVIEWS_GZ" &
pid_dl_reviews=$!
download_if_missing "$META_URL" "$META_GZ" &
pid_dl_meta=$!
wait "$pid_dl_reviews"
wait "$pid_dl_meta"

unpack_if_missing "$REVIEWS_GZ" &
pid_unpack_reviews=$!
unpack_if_missing "$META_GZ" &
pid_unpack_meta=$!
wait "$pid_unpack_reviews"
wait "$pid_unpack_meta"
