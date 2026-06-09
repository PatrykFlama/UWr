#!/usr/bin/env bash
set -euo pipefail

curl -L -o ./semantic-segmentation-of-aerial-imagery.zip https://www.kaggle.com/api/v1/datasets/download/humansintheloop/semantic-segmentation-of-aerial-imagery
unzip semantic-segmentation-of-aerial-imagery.zip

