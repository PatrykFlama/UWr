# Advanced Data Mining — Compressed Final Exam Notes

This is the shorter revision version of `FINAL_EXAM_NOTES.md`. It preserves every major lecture area while omitting most derivations, examples, experimental results, and secondary variants.

Use it for the second reading and final recall. For unclear concepts, return to the corresponding section in the full notes.

---

- [Advanced Data Mining — Compressed Final Exam Notes](#advanced-data-mining--compressed-final-exam-notes)
- [1. Self-supervised learning](#1-self-supervised-learning)
  - [Core idea](#core-idea)
  - [RotNet](#rotnet)
  - [Contrastive learning](#contrastive-learning)
- [2. Attention and Transformers](#2-attention-and-transformers)
  - [Attention](#attention)
  - [Transformer blocks](#transformer-blocks)
  - [Position and comparison with RNNs](#position-and-comparison-with-rnns)
- [3. Graph signals and GNNs](#3-graph-signals-and-gnns)
  - [Graph signal processing](#graph-signal-processing)
  - [GNN and GCN](#gnn-and-gcn)
- [4. Recommender systems](#4-recommender-systems)
  - [Setup and evaluation](#setup-and-evaluation)
  - [Matrix factorization and PMF](#matrix-factorization-and-pmf)
  - [BPR](#bpr)
  - [Other recommendation models](#other-recommendation-models)
- [5. Classical time-series methods](#5-classical-time-series-methods)
  - [Setup, stationarity, and validation](#setup-stationarity-and-validation)
  - [ARIMA family](#arima-family)
  - [Errors](#errors)
  - [DTW, clustering, and shapelets](#dtw-clustering-and-shapelets)
- [6. Neural time-series models](#6-neural-time-series-models)
  - [RNN and BPTT](#rnn-and-bptt)
  - [Teacher forcing](#teacher-forcing)
  - [LSTM and GRU](#lstm-and-gru)
  - [Modern sequence models](#modern-sequence-models)
  - [Transformers for time series](#transformers-for-time-series)
- [7. SSL for time series](#7-ssl-for-time-series)
  - [General issues](#general-issues)
  - [TS2Vec](#ts2vec)
  - [T-Rep](#t-rep)
- [8. Vision and geospatial data](#8-vision-and-geospatial-data)
  - [Remote sensing](#remote-sensing)
  - [CNN, segmentation, and ViT](#cnn-segmentation-and-vit)
  - [Satellite SSL](#satellite-ssl)
- [9. Trajectory clustering and segmentation](#9-trajectory-clustering-and-segmentation)
  - [DBSCAN and OPTICS](#dbscan-and-optics)
  - [TRACLUS](#traclus)
  - [SeqScan](#seqscan)
- [10. Dynamic graphs](#10-dynamic-graphs)
  - [Two different settings](#two-different-settings)
  - [EvolveGCN](#evolvegcn)
  - [GRNN and gated GRNN](#grnn-and-gated-grnn)
- [11. Last-minute comparison table](#11-last-minute-comparison-table)
  - [Final recurring principles](#final-recurring-principles)


---

# 1. Self-supervised learning

## Core idea

**Self-supervised learning (SSL)** creates supervision from unlabeled data. An encoder is trained on a **pretext task**, then reused for a real **downstream task**.

- supervised: train from real labels;
- unsupervised: discover structure without labels;
- self-supervised: generate targets or relationships from the data;
- semi-supervised: only some examples are labeled;
- transfer learning: replace the old head and reuse a pretrained model;
- fine-tuning: continue training pretrained layers.

Evaluate representations with simple downstream models, especially a **linear probe**, rather than only the pretext loss. Also test transfer, robustness, clustering, and generalization.

## RotNet

Rotate images by one of several fixed angles and predict the rotation. Solving the task can require learning object shape and scene semantics.

Key conditions:

- include the original orientation;
- avoid interpolation artifacts that reveal the answer;
- remove the rotation head before transfer.

Other pretext tasks: colorization, jigsaw solving, relative patch position, ego-motion prediction.

## Contrastive learning

- positive pairs should have similar embeddings;
- negatives should remain distinguishable;
- **alignment** pulls positives together;
- **uniformity** spreads embeddings and prevents collapse.

The critical design choice is what counts as a positive pair. A wrong choice teaches a false invariance.

> **Essential formula — InfoNCE**
>
> $$
> \ell=-\log
> \frac{\exp(\operatorname{sim}(z,z^+)/\tau)}
> {\exp(\operatorname{sim}(z,z^+)/\tau)+
> \sum_j\exp(\operatorname{sim}(z,z_j^-)/\tau)}.
> $$

---

# 2. Attention and Transformers

## Attention

Attention is content-based retrieval:

- query: what is needed?
- key: what does an element offer?
- value: what content is returned?

> **Essential formula — scaled dot-product attention**
>
> $$
> Q=XW_Q,\qquad K=XW_K,\qquad V=XW_V,
> $$
>
> $$
> \operatorname{Attention}(Q,K,V)
> =\operatorname{softmax}
> \left(\frac{QK^\top}{\sqrt{d_k}}+M\right)V.
> $$

$1/\sqrt{d_k}$ prevents excessively large logits. A causal mask $M$ uses $-\infty$ at forbidden future positions.

- **self-attention:** $Q,K,V$ come from the same sequence;
- **cross-attention:** decoder queries attend to encoder keys and values;
- **multi-head attention:** several independent retrieval patterns are learned in parallel.

## Transformer blocks

Encoder:

1. multi-head self-attention;
2. residual connection and normalization;
3. position-wise feed-forward network;
4. residual connection and normalization.

Decoder:

1. masked self-attention;
2. cross-attention;
3. feed-forward network;
4. residual connections and normalization.

During training, decoder targets are shifted right so each position predicts the next token without seeing it.

## Position and comparison with RNNs

Attention alone does not know order, so positional encodings are added.

Transformers:

- allow direct interaction between distant positions;
- train in parallel;
- handle long dependencies better than vanilla RNNs;
- require positional information;
- have $O(T^2)$ full-attention cost;
- remain sequential during autoregressive inference.

---

# 3. Graph signals and GNNs

## Graph signal processing

A graph is $G=(V,E,W)$. A graph signal assigns a value or feature vector to every node.

A graph shift operator $S$ may be an adjacency matrix, Laplacian, normalized adjacency, or random-walk matrix.

- $Sx$: one-hop aggregation;
- $S^2x$: two-hop information;
- $S^kx$: information up to $k$ hops.

> **Essential formula — graph filter**
>
> $$
> H(S)x=\sum_{k=0}^{K-1}h_kS^kx.
> $$

Powers of $S$ propagate through the graph; coefficients determine the importance of each neighborhood radius.

## GNN and GCN

A GNN layer:

1. aggregates neighbor information;
2. transforms features;
3. applies a nonlinearity.

> **Essential formula — Kipf–Welling GCN**
>
> $$
> \widetilde A=A+I,\qquad
> \widehat A=\widetilde D^{-1/2}
> \widetilde A\widetilde D^{-1/2},
> $$
>
> $$
> H^{(\ell+1)}
> =\sigma\left(\widehat AH^{(\ell)}W^{(\ell)}\right).
> $$

- self-loops preserve the node’s own features;
- normalization prevents high-degree nodes from dominating;
- after $L$ layers, information comes from approximately $L$ hops.

Deep-GNN failures:

- **oversmoothing:** node embeddings become indistinguishable;
- **oversquashing:** too much distant information is compressed into fixed-size vectors.

---

# 4. Recommender systems

## Setup and evaluation

Inputs:

- users and items;
- explicit ratings or implicit interactions;
- optional user/item metadata;
- possibly timestamps and interaction order.

Implicit absence is not necessarily dislike: it may be lack of exposure.

Main problems: sparsity, cold start, popularity/exposure bias, false negatives, noise, fairness, and scalability.

Important metrics:

- Precision@K: fraction of top-$K$ results that are relevant;
- Recall@K: fraction of all relevant items retrieved;
- Hit Rate: whether at least one relevant item appears;
- MRR: emphasizes rank of the first relevant item;
- NDCG: rewards highly ranked and graded relevance;
- AUC: probability that a positive outranks a negative.

## Matrix factorization and PMF

> **Essential formula — matrix factorization**
>
> $$
> \hat r_{ui}=p_u^\top q_i,\qquad R\approx PQ^\top.
> $$

Learn user and item vectors so their dot product predicts compatibility. PMF gives a probabilistic interpretation: Gaussian priors yield squared error with $L_2$ regularization as MAP estimation.

Variants:

- FunkSVD optimizes observed interactions directly;
- NMF uses nonnegative factors;
- SVD++ adds implicit-history information.

## BPR

BPR optimizes pairwise ranking using triples $(u,i,j)$:

- $i$: observed positive;
- $j$: sampled unobserved item.

> **Essential formula — BPR**
>
> $$
> \mathcal L_{\mathrm{BPR}}
> =-\sum_{(u,i,j)}
> \log\sigma(\hat x_{ui}-\hat x_{uj})
> +\lambda\|\Theta\|^2.
> $$

The negative sampler matters because an unobserved item may be a false negative.

## Other recommendation models

**Factorization Machine:** models sparse tabular features and pairwise interactions through latent vectors.

$$
\hat y=w_0+\sum_iw_ix_i+
\sum_{i<j}\langle v_i,v_j\rangle x_ix_j.
$$

**LightFM:** sums embeddings of active user/item features. It combines interactions with metadata and helps cold start.

**NCF/NeuMF:** learns a nonlinear user–item interaction function using GMF and MLP branches. Binary cross-entropy is common for implicit data.

**SASRec:** causally masked Transformer over a user’s item sequence. It predicts the shifted next-item sequence using item and positional embeddings.

**LightGCN:** user–item bipartite graph propagation without feature transforms or nonlinearities.

$$
V^{(\ell)}=D^{-1/2}AD^{-1/2}V^{(\ell-1)},\qquad
V=\frac1{L+1}\sum_{\ell=0}^{L}V^{(\ell)}.
$$

**SGL/SimGCL:** combine recommendation loss with contrastive learning. SimGCL uses embedding noise instead of complicated graph augmentations. A typical objective is BPR + InfoNCE.

**DirectAU:** directly optimizes representation alignment and uniformity.

---

# 5. Classical time-series methods

## Setup, stationarity, and validation

Forecasting maps a history window to a future horizon:

$$
x_{t-L+1:t}\mapsto\hat y_{t+1:t+H}.
$$

Weak stationarity requires:

$$
\mathbb E[X_t]=\mu,\qquad
\operatorname{Cov}(X_t,X_{t+k})=\gamma_k.
$$

Non-stationarity can come from trend, seasonality, level shifts, anomalies, or changing variance. Use detrending, seasonal adjustment, differencing, or suitable transformations.

Validation rules:

- split chronologically;
- fit scalers only on training data;
- use only information available at decision time;
- inspect errors separately by horizon;
- compare with naive and seasonal-naive baselines.

## ARIMA family

$$
\text{AR}(p):\quad
X_t=c+\sum_{i=1}^{p}\phi_iX_{t-i}+\varepsilon_t.
$$

$$
\text{MA}(q):\quad
X_t=c+\varepsilon_t+
\sum_{j=1}^{q}\theta_j\varepsilon_{t-j}.
$$

ARMA combines AR and MA. ARIMA$(p,d,q)$ applies ARMA after differencing $d$ times:

$$
\Delta X_t=X_t-X_{t-1}.
$$

- $p$: lagged observations;
- $d$: differencing order;
- $q$: lagged errors.

A random walk is AR(1) with $\phi_1=1$ and is non-stationary.

Model selection:

$$
AIC=2k-2\log L,\qquad
BIC=k\log N-2\log L.
$$

Lower is better; BIC penalizes complexity more strongly for large $N$.

## Errors

$$
MSE=\frac1T\sum_t(x_t-\hat x_t)^2,
\qquad
MAE=\frac1T\sum_t|x_t-\hat x_t|.
$$

- MSE emphasizes large errors;
- MAE is more robust;
- MAPE is unstable near zero;
- probabilistic forecasts need calibration-aware metrics such as NLL or CRPS.

## DTW, clustering, and shapelets

DTW aligns similar shapes despite local time shifts.

$$
D(i,j)=d(s_i,t_j)+
\min\{D(i-1,j),D(i,j-1),D(i-1,j-1)\}.
$$

It requires monotonic endpoint-to-endpoint alignment and costs $O(nm)$.

Ordinary k-means uses arithmetic means because they minimize squared Euclidean error. With DTW, use **DBA**, which aligns cluster members to a candidate center and averages values matched to each center position.

Time-series classification options:

- vector + kNN/SVM;
- DTW nearest neighbor;
- feature extraction such as interval mean/std/slope;
- shapelets;
- neural representations.

A **shapelet** is a discriminative subsequence. Represent a series by minimum distances to shapelets:

$$
M_{ik}=
\min_j\frac1L\sum_{\ell=1}^{L}
(x^{(i)}_{j+\ell-1}-s_\ell^{(k)})^2.
$$

Shapelets can be enumerated and selected by information gain or learned jointly with a classifier using a differentiable soft minimum.

---

# 6. Neural time-series models

## RNN and BPTT

$$
h_t=\tanh(W_xx_t+W_hh_{t-1}+b),\qquad
\hat y_t=g(h_t).
$$

The hidden state compresses history. BPTT differentiates through the unrolled sequence.

Repeated temporal Jacobians cause:

- vanishing gradients if their scale is below $1$;
- exploding gradients if above $1$.

Gradient clipping controls explosion:

$$
g\leftarrow g\min\left(1,\frac{\tau}{\|g\|_2}\right).
$$

## Teacher forcing

During training, a decoder receives the true previous target; at inference, it receives its own prediction. This mismatch causes exposure bias and recursive error accumulation.

Alternatives:

- scheduled sampling;
- direct multi-horizon prediction;
- sequence-to-sequence decoding.

## LSTM and GRU

LSTM uses a cell state and gates:

$$
c_t=f_t\odot c_{t-1}+i_t\odot\widetilde c_t,
\qquad
h_t=o_t\odot\tanh(c_t).
$$

- forget gate: what old memory remains;
- input gate: what new information is stored;
- output gate: what memory is exposed.

The additive cell path helps long-term gradient flow.

GRU has one hidden state:

$$
h_t=(1-z_t)\odot h_{t-1}+z_t\odot\widetilde h_t.
$$

- reset gate controls how old state forms the candidate;
- update gate blends old and new state;
- GRU is simpler and often faster;
- LSTM has more explicit memory capacity.

## Modern sequence models

State-space form:

$$
s_t=A_ts_{t-1}+B_tx_t,\qquad y_t=C_ts_t.
$$

- **RWKV:** Transformer-like training with recurrent inference;
- **xLSTM:** modern scalar or matrix LSTM memories;
- **Mamba:** input-dependent selective state-space parameters, linear scaling, and constant-size inference state;
- **hybrids:** combine efficient state memory with attention-based retrieval.

## Transformers for time series

Benefits:

- direct access to old events;
- parallel training;
- flexible cross-channel interactions.

Use:

- encoder-only for classification, imputation, representation learning;
- decoder-only for causal forecasting;
- encoder–decoder for history-to-horizon mapping.

**Patching** shortens the token sequence, encodes local shape, and reduces attention cost.

Pitfalls:

- quadratic full attention;
- autoregressive error accumulation;
- overfitting calendar artifacts;
- direct forecasts may average multimodal futures.

Foundation models:

- Chronos: tokenized numeric values;
- TimesFM: patched decoder-only forecasting;
- Moirai and MOMENT: general multi-domain time-series models;
- diffusion models: generate multiple plausible futures by denoising.

---

# 7. SSL for time series

## General issues

A timestamp-level encoder produces:

$$
f_\theta(X_i)=[z_{i1},\ldots,z_{iT}].
$$

Representations should preserve relevant shape, level, time, context, and instance identity.

Time-series augmentations are dangerous:

- scaling may alter meaning;
- cropping may cross regimes;
- nearby points may differ at anomalies;
- distant points may match due to periodicity.

SSL does not prevent leakage. Pretraining and preprocessing must respect deployment time.

## TS2Vec

TS2Vec learns timestamp representations using:

1. two overlapping random crops;
2. independent masking after input projection;
3. a dilated CNN encoder;
4. temporal contrast;
5. instance contrast;
6. hierarchical max pooling.

**Contextual consistency:** the same original timestamp in two different overlapping contexts is a positive pair.

- temporal contrast separates different times within one series;
- instance contrast separates different series at the same time;
- hierarchical contrast repeats the objective at increasingly coarse temporal scales.

Downstream use:

- classification: global pool + simple classifier;
- forecasting: last timestamp vector + linear model;
- anomaly detection: difference between masked and unmasked representations;
- subsequence retrieval: pool the chosen interval.

## T-Rep

T-Rep adds a learned time embedding:

$$
h_\psi(t)=
[\omega_0t+\phi_0,
\sin(\omega_1t+\phi_1),\ldots].
$$

It keeps TS2Vec contrast and adds:

1. predict divergence between two time embeddings from their representations;
2. predict a nearby representation from current representation plus target-time embedding.

TS2Vec mainly learns robust contextual identity. T-Rep adds a smoother, explicit geometry of time, useful for forecasting, anomalies, and missing data.

Newer directions include masked reconstruction, universal masking, UniTS task tokenization, and Time-MoE scaling.

---

# 8. Vision and geospatial data

## Remote sensing

Main resolutions:

- spatial: ground area per pixel;
- temporal: revisit frequency;
- spectral: number and width of wavelength bands.

Healthy vegetation absorbs red and reflects near-infrared:

$$
NDVI=\frac{NIR-Red}{NIR+Red}.
$$

Data:

- RGB;
- multispectral;
- hyperspectral hypercube.

Preprocessing may correct striping, dropped lines, bit errors, atmospheric scattering, and geometric misalignment. Image registration is essential for time-series change analysis.

## CNN, segmentation, and ViT

Convolution output size:

$$
W'=\left\lfloor\frac{W-K+2P}{S}\right\rfloor+1.
$$

CNN properties:

- local shared filters;
- translation equivariance;
- deeper layers have larger receptive fields;
- striding/pooling downsamples.

Semantic segmentation predicts a class for every pixel. An FCN downsamples for context and upsamples for resolution.

**U-Net:** encoder–decoder with skip connections. Deep features provide semantic context; shallow features restore precise localization.

**ViT:** image patches become tokens, receive positional embeddings, and pass through a Transformer. A classification token is used for image classification.

**Knowledge distillation:** a student matches teacher probabilities, usually using KL divergence plus ground-truth cross-entropy.

## Satellite SSL

**MAE:** mask image patches, encode visible patches, and reconstruct missing ones.

**SatMAE:** extends MAE across spatial, temporal, and spectral dimensions.

- consistent masking uses the same masked positions across times/bands and prevents direct copying;
- independent masking gives different visible evidence across times/bands.

**Presto:** lightweight pixel time-series encoder combining Sentinel data, NDVI, weather, land cover, location, and topography.

$$
\mathcal L=
\mathcal L_{\mathrm{MSE}}+
\lambda\frac{N_{\mathrm{cat}}}{N_{\mathrm{cont}}}
\mathcal L_{\mathrm{CE}}.
$$

MSE reconstructs continuous variables; cross-entropy reconstructs categorical variables.

---

# 9. Trajectory clustering and segmentation

## DBSCAN and OPTICS

DBSCAN parameters:

- $\varepsilon$: neighborhood radius;
- MinPts: minimum points for a dense region.

Point types:

- core: enough neighbors;
- border: near a core but not itself core;
- noise: neither.

DBSCAN finds arbitrary shapes and noise without preselecting cluster count, but one global $\varepsilon$ performs poorly with varying densities.

OPTICS uses:

$$
\operatorname{reach\_dist}(p,q)=
\max(\operatorname{core\_dist}(p),d(p,q)).
$$

It outputs a reachability ordering:

- valleys are clusters;
- peaks are transitions/noise;
- multiple density levels are visible.

## TRACLUS

Goal: find common sub-trajectories across many moving objects.

Pipeline:

1. partition each trajectory at characteristic points;
2. cluster resulting line segments with DBSCAN-like density clustering;
3. build a representative trajectory.

Segment distance combines:

- perpendicular separation $d_\perp$;
- parallel displacement $d_\parallel$;
- angular difference $d_\theta$.

$$
\operatorname{dist}(L_i,L_j)=
w_\perp d_\perp+
w_\parallel d_\parallel+
w_\theta d_\theta.
$$

Partitioning uses Minimum Description Length:

$$
MDL=L(H)+L(D\mid H).
$$

- $L(H)$ penalizes a complex partition;
- $L(D\mid H)$ penalizes inaccurate approximation.

The approximate partitioner is greedy and may miss the global optimum. Segment clusters must involve enough distinct original trajectories, not only many segments from one path.

## SeqScan

Goal: segment one ordered trajectory into:

- stay regions;
- transitions;
- noise.

A stay region must be spatially dense and satisfy minimum presence:

$$
P(S)\ge\delta.
$$

SeqScan processes points in time order, maintains a context for the current stay and a pool for a possible next stay, and switches when the pool forms a new Minimal Stay Region.

Key difference:

- TRACLUS groups common route pieces across trajectories;
- SeqScan finds temporally separate stays within one trajectory.

---

# 10. Dynamic graphs

## Two different settings

**EvolveGCN:** graph topology and node set may change over time.

**GRNN/GGRNN:** graph topology is fixed, but node signals evolve over time.

Do not confuse them.

## EvolveGCN

Problem with GCN followed by an RNN over node embeddings: every node needs a persistent identity and history. This fails for new, disappearing, or disjoint node sets.

EvolveGCN evolves model weights:

$$
W_t=\operatorname{RNN}(W_{t-1}),
$$

$$
H_t^{(\ell+1)}
=\sigma\left(\widehat A_t
H_t^{(\ell)}W_t^{(\ell)}\right).
$$

Node embeddings are computed fresh from the current graph.

- **EvolveGCN-H:** GCN weights are the hidden state of a GRU; summarized current node embeddings are the input. Prefer when node features are informative.
- **EvolveGCN-O:** an LSTM maps previous GCN weights to new weights without node-embedding input. Prefer when structural evolution dominates.

Each EGCU first updates weights and then performs graph convolution.

## GRNN and gated GRNN

For fixed topology:

$$
z_t=\sigma\left(A(S)x_t+B(S)z_{t-1}\right),
$$

where $A(S)$ and $B(S)$ are graph filters.

This combines:

- spatial aggregation through graph filters;
- temporal memory through recurrence;
- parameter count independent of graph size;
- permutation equivariance and local computation.

Gating addresses temporal gradients and spatial imbalance:

- **time gate:** one scalar decision for the whole graph at a time;
- **node gate:** one decision per node;
- **edge gate:** one decision per edge, similar to graph attention.

Mnemonic:

- time gating: **when** to remember;
- node gating: **where** to remember;
- edge gating: **which connections** transmit information.

---

# 11. Last-minute comparison table

| Compare | Key distinction |
|---|---|
| SSL vs transfer | SSL creates pretraining supervision; transfer reuses a trained model |
| Contrastive vs masked learning | relationships between views vs reconstruction from context |
| RNN vs Transformer vs Mamba | compressed recurrent state vs direct attention vs selective state-space memory |
| CNN vs ViT | built-in local spatial bias vs global patch attention |
| MF vs LightGCN vs SASRec | static compatibility vs interaction graph vs ordered history |
| Euclidean vs DTW | fixed alignment vs monotonic learned alignment |
| k-means vs DBSCAN | centroid partitioning vs density connectivity with noise |
| DBSCAN vs OPTICS | one density scale vs reachability structure across scales |
| TRACLUS vs SeqScan | common sub-routes across objects vs stays in one ordered path |
| Static GCN vs EvolveGCN | fixed graph model vs recurrently evolving GCN weights |
| EvolveGCN vs GRNN | changing graph snapshots vs changing signals on a fixed graph |
| TS2Vec vs T-Rep | contextual multiscale contrast vs contrast plus explicit learned time geometry |

## Final recurring principles

1. Match model structure to data structure.
2. Parameter sharing gives scalability.
3. Masking defines what information a model may use.
4. Normalization controls numerical and structural scale.
5. Evaluation must reproduce deployment conditions.
6. Every added flexibility introduces a failure mode.
7. Always compare against strong simple baselines.
