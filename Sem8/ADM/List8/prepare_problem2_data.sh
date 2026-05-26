#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")" && pwd)"
EXTERNAL_DIR="$SCRIPT_DIR/external"
TS2VEC_DIR="$EXTERNAL_DIR/ts2vec_official"
TREP_DIR="$EXTERNAL_DIR/t_rep_official"
TMP_DIR="${TMPDIR:-/tmp}/ecg200_ucr"

mkdir -p "$EXTERNAL_DIR"

if [[ ! -d "$TS2VEC_DIR" ]]; then
  git clone --depth 1 https://github.com/zhihanyue/ts2vec.git "$TS2VEC_DIR"
fi

if [[ ! -d "$TREP_DIR" ]]; then
  git clone --depth 1 https://github.com/Let-it-Care/T-Rep.git "$TREP_DIR"
fi

if [[ ! -f "$TS2VEC_DIR/datasets/UCR/ECG200/ECG200_TRAIN.tsv" || \
      ! -f "$TS2VEC_DIR/datasets/UCR/ECG200/ECG200_TEST.tsv" || \
      ! -f "$TREP_DIR/datasets/UCR/ECG200/ECG200_TRAIN.tsv" || \
      ! -f "$TREP_DIR/datasets/UCR/ECG200/ECG200_TEST.tsv" ]]; then
  mkdir -p "$TMP_DIR"
  curl -fL https://timeseriesclassification.com/aeon-toolkit/ECG200.zip -o "$TMP_DIR/ECG200.zip"
  unzip -o "$TMP_DIR/ECG200.zip" -d "$TMP_DIR"

  mkdir -p "$TS2VEC_DIR/datasets/UCR/ECG200" "$TREP_DIR/datasets/UCR/ECG200"
  cp "$TMP_DIR/ECG200_TRAIN.txt" "$TS2VEC_DIR/datasets/UCR/ECG200/ECG200_TRAIN.tsv"
  cp "$TMP_DIR/ECG200_TEST.txt" "$TS2VEC_DIR/datasets/UCR/ECG200/ECG200_TEST.tsv"
  cp "$TMP_DIR/ECG200_TRAIN.txt" "$TREP_DIR/datasets/UCR/ECG200/ECG200_TRAIN.tsv"
  cp "$TMP_DIR/ECG200_TEST.txt" "$TREP_DIR/datasets/UCR/ECG200/ECG200_TEST.tsv"
fi

echo "Problem 2 data is ready in $EXTERNAL_DIR"
