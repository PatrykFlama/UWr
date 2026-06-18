# Advanced Data Mining — Final Exam Notes

These notes are designed to be self-contained and short enough to read twice in one day. They combine repeated material and emphasize concepts, model intuition, assumptions, comparisons, and typical failure modes. Formulas worth recognizing or remembering are placed in separate **Formula boxes**.

---

- [Advanced Data Mining — Final Exam Notes](#advanced-data-mining--final-exam-notes)
- [1. Learning paradigms and self-supervised learning](#1-learning-paradigms-and-self-supervised-learning)
  - [1.1 Why representations matter](#11-why-representations-matter)
  - [1.2 Learning strategies](#12-learning-strategies)
  - [1.3 Evaluating representation quality](#13-evaluating-representation-quality)
  - [1.4 RotNet](#14-rotnet)
  - [1.5 Contrastive learning: alignment and uniformity](#15-contrastive-learning-alignment-and-uniformity)
- [2. Attention and Transformers](#2-attention-and-transformers)
  - [2.1 Why attention was introduced](#21-why-attention-was-introduced)
  - [2.2 Self-attention, cross-attention, and masking](#22-self-attention-cross-attention-and-masking)
  - [2.3 Multi-head attention](#23-multi-head-attention)
  - [2.4 Transformer encoder](#24-transformer-encoder)
  - [2.5 Transformer decoder](#25-transformer-decoder)
  - [2.6 Positional information](#26-positional-information)
  - [2.7 Transformers versus RNNs](#27-transformers-versus-rnns)
- [3. Graph signals, graph filters, and GNNs](#3-graph-signals-graph-filters-and-gnns)
  - [3.1 Graph definitions](#31-graph-definitions)
  - [3.2 Graph signals and shift operators](#32-graph-signals-and-shift-operators)
  - [3.3 Graph filters and graph convolution](#33-graph-filters-and-graph-convolution)
  - [3.4 From graph convolution to a GNN](#34-from-graph-convolution-to-a-gnn)
  - [3.5 Kipf–Welling GCN](#35-kipfwelling-gcn)
- [4. Recommender systems](#4-recommender-systems)
  - [4.1 Problem setup and feedback](#41-problem-setup-and-feedback)
  - [4.2 Main recommender families](#42-main-recommender-families)
  - [4.3 Matrix factorization](#43-matrix-factorization)
    - [Related matrix-factorization variants](#related-matrix-factorization-variants)
  - [4.4 Probabilistic matrix factorization](#44-probabilistic-matrix-factorization)
  - [4.5 Ranking metrics](#45-ranking-metrics)
  - [4.6 Bayesian Personalized Ranking](#46-bayesian-personalized-ranking)
  - [4.7 Alignment and uniformity in recommendation](#47-alignment-and-uniformity-in-recommendation)
  - [4.8 Factorization Machines](#48-factorization-machines)
  - [4.9 LightFM](#49-lightfm)
  - [4.10 Neural Collaborative Filtering](#410-neural-collaborative-filtering)
  - [4.11 SASRec: sequential recommendation](#411-sasrec-sequential-recommendation)
  - [4.12 LightGCN](#412-lightgcn)
  - [4.13 Contrastive graph recommendation and SimGCL](#413-contrastive-graph-recommendation-and-simgcl)
  - [4.14 Current recommender challenges](#414-current-recommender-challenges)
- [5. Classical time-series methods](#5-classical-time-series-methods)
  - [5.1 Definitions and tasks](#51-definitions-and-tasks)
  - [5.2 Stationarity and preprocessing](#52-stationarity-and-preprocessing)
  - [5.3 Forecasting baselines](#53-forecasting-baselines)
  - [5.4 AR, MA, ARMA, and ARIMA](#54-ar-ma-arma-and-arima)
  - [5.5 Model-order and parameter selection](#55-model-order-and-parameter-selection)
  - [5.6 Time-series validation and errors](#56-time-series-validation-and-errors)
  - [5.7 Time-series distances and DTW](#57-time-series-distances-and-dtw)
  - [5.8 Time-series clustering](#58-time-series-clustering)
  - [5.9 Time-series classification](#59-time-series-classification)
  - [5.10 Shapelets](#510-shapelets)
    - [Brute-force shapelet discovery](#brute-force-shapelet-discovery)
    - [Learning shapelets](#learning-shapelets)
- [6. Neural sequence models for time series](#6-neural-sequence-models-for-time-series)
  - [6.1 Turning a time series into supervised examples](#61-turning-a-time-series-into-supervised-examples)
  - [6.2 Vanilla RNN](#62-vanilla-rnn)
  - [6.3 Vanishing and exploding gradients](#63-vanishing-and-exploding-gradients)
  - [6.4 Multi-step forecasting and teacher forcing](#64-multi-step-forecasting-and-teacher-forcing)
  - [6.5 LSTM](#65-lstm)
  - [6.6 GRU](#66-gru)
  - [6.7 Practical recurrent modeling](#67-practical-recurrent-modeling)
  - [6.8 Modern recurrent and state-space models](#68-modern-recurrent-and-state-space-models)
    - [RWKV](#rwkv)
    - [xLSTM](#xlstm)
    - [S4 and Mamba](#s4-and-mamba)
    - [Complexity intuition](#complexity-intuition)
  - [6.9 Transformers for time series](#69-transformers-for-time-series)
    - [Patching](#patching)
    - [Architecture choice](#architecture-choice)
    - [Probabilistic heads](#probabilistic-heads)
    - [Long-horizon pitfalls](#long-horizon-pitfalls)
  - [6.10 Time-series foundation models and generative forecasting](#610-time-series-foundation-models-and-generative-forecasting)
- [7. Self-supervised learning for time series](#7-self-supervised-learning-for-time-series)
  - [7.1 What a time-series representation should preserve](#71-what-a-time-series-representation-should-preserve)
  - [7.2 Why augmentations are difficult](#72-why-augmentations-are-difficult)
  - [7.3 Contrastive and masked objectives](#73-contrastive-and-masked-objectives)
  - [7.4 TS2Vec](#74-ts2vec)
    - [Architecture](#architecture)
    - [Contextual consistency](#contextual-consistency)
    - [Dual contrast](#dual-contrast)
    - [Hierarchical contrast](#hierarchical-contrast)
    - [Downstream use](#downstream-use)
  - [7.5 T-Rep](#75-t-rep)
    - [Time2Vec intuition](#time2vec-intuition)
    - [Four T-Rep pretext tasks](#four-t-rep-pretext-tasks)
    - [TS2Vec versus T-Rep](#ts2vec-versus-t-rep)
  - [7.6 Newer directions](#76-newer-directions)
- [8. Deep learning for vision and geospatial data](#8-deep-learning-for-vision-and-geospatial-data)
  - [8.1 Remote sensing fundamentals](#81-remote-sensing-fundamentals)
  - [8.2 Remote-sensing errors and preprocessing](#82-remote-sensing-errors-and-preprocessing)
  - [8.3 Vision tasks](#83-vision-tasks)
  - [8.4 CNN concepts](#84-cnn-concepts)
    - [Pooling and stride](#pooling-and-stride)
  - [8.5 Semantic segmentation and U-Net](#85-semantic-segmentation-and-u-net)
  - [8.6 Vision Transformer](#86-vision-transformer)
  - [8.7 Knowledge distillation](#87-knowledge-distillation)
  - [8.8 Satellite image time series](#88-satellite-image-time-series)
  - [8.9 MAE and SatMAE](#89-mae-and-satmae)
  - [8.10 Presto](#810-presto)
- [9. Trajectory clustering and segmentation](#9-trajectory-clustering-and-segmentation)
  - [9.1 Clustering versus segmentation](#91-clustering-versus-segmentation)
  - [9.2 DBSCAN](#92-dbscan)
  - [9.3 OPTICS](#93-optics)
  - [9.4 TRACLUS overview](#94-traclus-overview)
  - [9.5 TRACLUS line-segment distance](#95-traclus-line-segment-distance)
  - [9.6 TRACLUS partitioning with MDL](#96-traclus-partitioning-with-mdl)
  - [9.7 TRACLUS segment clustering and representative paths](#97-traclus-segment-clustering-and-representative-paths)
  - [9.8 SeqScan](#98-seqscan)
- [10. Dynamic graphs and graph recurrent models](#10-dynamic-graphs-and-graph-recurrent-models)
  - [10.1 Two distinct temporal-graph settings](#101-two-distinct-temporal-graph-settings)
  - [10.2 Why a static GCN is insufficient](#102-why-a-static-gcn-is-insufficient)
  - [10.3 EvolveGCN: evolve weights, not node embeddings](#103-evolvegcn-evolve-weights-not-node-embeddings)
  - [10.4 EvolveGCN-H](#104-evolvegcn-h)
  - [10.5 EvolveGCN-O](#105-evolvegcn-o)
  - [10.6 Evolving Graph Convolution Unit](#106-evolving-graph-convolution-unit)
  - [10.7 Graph Recurrent Neural Networks](#107-graph-recurrent-neural-networks)
  - [10.8 Why gate a GRNN](#108-why-gate-a-grnn)
  - [10.9 Time, node, and edge gating](#109-time-node-and-edge-gating)
    - [Time gating](#time-gating)
    - [Node gating](#node-gating)
    - [Edge gating](#edge-gating)
  - [10.10 EvolveGCN versus GRNN/GGRNN](#1010-evolvegcn-versus-grnnggrnn)
- [11. Final comparison review](#11-final-comparison-review)
  - [11.1 High-value conceptual comparisons](#111-high-value-conceptual-comparisons)
    - [Supervised, self-supervised, and transfer learning](#supervised-self-supervised-and-transfer-learning)
    - [Contrastive versus masked learning](#contrastive-versus-masked-learning)
    - [RNN versus Transformer versus SSM](#rnn-versus-transformer-versus-ssm)
    - [CNN versus ViT](#cnn-versus-vit)
    - [Matrix factorization versus LightGCN versus SASRec](#matrix-factorization-versus-lightgcn-versus-sasrec)
    - [Euclidean distance versus DTW](#euclidean-distance-versus-dtw)
    - [DBSCAN versus OPTICS](#dbscan-versus-optics)
    - [TRACLUS versus SeqScan](#traclus-versus-seqscan)
    - [Static GCN versus LightGCN versus EvolveGCN versus GRNN](#static-gcn-versus-lightgcn-versus-evolvegcn-versus-grnn)
  - [11.2 Questions you should be able to answer](#112-questions-you-should-be-able-to-answer)
  - [11.3 Recurring principles across the course](#113-recurring-principles-across-the-course)
    - [Structure should match the data](#structure-should-match-the-data)
    - [Parameter sharing creates scalability](#parameter-sharing-creates-scalability)
    - [Normalization controls scale](#normalization-controls-scale)
    - [Masking controls information availability](#masking-controls-information-availability)
    - [Evaluation must match deployment](#evaluation-must-match-deployment)
    - [More flexible models introduce new failure modes](#more-flexible-models-introduce-new-failure-modes)
- [Coverage note](#coverage-note)


---

# 1. Learning paradigms and self-supervised learning

## 1.1 Why representations matter

A learned **representation** or **embedding** is a vector that preserves information useful for later tasks. Instead of training a separate model from raw data for every task, we can train a reusable encoder and attach a small task-specific head.

The central motivation for **self-supervised learning (SSL)** is:

- unlabeled data are abundant;
- labels are expensive;
- an artificial task can force a model to learn reusable structure;
- a small labeled set can then be used for a downstream task.

The artificial task is the **pretext task**. The actual target application is the **downstream task**.

## 1.2 Learning strategies

- **Supervised learning:** both inputs $X$ and human-provided targets $Y$ are available. The model is trained from scratch for the target task.
- **Unsupervised learning:** only $X$ is available. The goal is to discover structure, for example clusters or latent factors.
- **Self-supervised learning:** only $X$ is originally available, but synthetic targets or relationships are created from $X$. Examples: predict image rotation, reconstruct masked values, or match two views of the same object.
- **Semi-supervised learning:** labels are available for only part of the data.
- **Transfer learning:** reuse a model trained on one task or dataset, remove or replace its old head, and attach a new head.
- **Fine-tuning:** continue training some or all pretrained layers on the new task. It is not the same as merely replacing the head.

The important distinction is that SSL describes **how pretraining supervision is created**, while transfer learning describes **how a trained model is reused**.

## 1.3 Evaluating representation quality

The pretext-task loss alone is not enough: a model can solve a pretext task using shortcuts that do not transfer.

Useful evaluation methods include:

- **linear evaluation / linear probing:** freeze the encoder and train a linear classifier;
- clustering in representation space;
- visualization such as t-SNE, used only as supporting evidence;
- downstream-task performance after transfer or fine-tuning;
- robustness to missing data, noise, and distribution shift;
- generalization to other datasets;
- computational efficiency.

A good representation allows a simple downstream model to perform well.

## 1.4 RotNet

**RotNet** creates labels by rotating an image by one of a small set of angles, normally $0^\circ,90^\circ,180^\circ,270^\circ$, and training a classifier to identify the rotation.

Why this can work:

- recognizing orientation requires understanding object shape and scene semantics;
- targets are generated automatically;
- the learned backbone can later be transferred to another vision task.

Important design details:

- include the original orientation;
- use rotations that avoid interpolation artifacts;
- avoid transformations whose artifacts reveal the label;
- after pretraining, remove the rotation-classification head.

> **Formula box — RotNet objective**
>
> For image $X_i$, rotation operation $g(X_i\mid y)$, and predicted probability $F^y(\cdot\mid\theta)$:
>
> $$
> \mathcal L(\theta)
> =-\frac{1}{N}\sum_{i=1}^{N}\frac{1}{K}\sum_{y=1}^{K}
> \log F^y(g(X_i\mid y)\mid\theta).
> $$
>
> Conceptually, this is ordinary cross-entropy on automatically generated rotation labels.

Other visual pretext tasks from the lectures:

- colorizing a grayscale image;
- predicting the relative position of image patches;
- solving a jigsaw puzzle;
- predicting ego-motion.

The general lesson is that a good pretext task must require the semantic information wanted by downstream tasks, while avoiding easy shortcuts.

## 1.5 Contrastive learning: alignment and uniformity

Contrastive learning builds:

- **positive pairs**, which should obtain similar representations;
- **negative pairs**, which should remain distinguishable.

Two desirable properties are:

- **alignment:** representations of related objects should be close;
- **uniformity:** representations should not all collapse into a small region; they should use the available embedding space.

Choosing positives and negatives is part of the model. A wrong positive-pair assumption teaches a false invariance.

---

# 2. Attention and Transformers

## 2.1 Why attention was introduced

In an encoder–decoder RNN, compressing a long input into one fixed vector creates a bottleneck. Attention lets the decoder dynamically retrieve the most relevant input representations for each output.

The database analogy is:

- **query $Q$:** what information is needed?
- **key $K$:** what does each stored element offer?
- **value $V$:** what content should be returned?

Attention compares a query with keys, converts similarities to weights, and returns a weighted combination of values.

## 2.2 Self-attention, cross-attention, and masking

- **Self-attention:** queries, keys, and values come from the same sequence. Each token creates a context-dependent representation using other tokens.
- **Encoder–decoder or cross-attention:** queries come from the decoder; keys and values come from encoder outputs.
- **Masked/causal self-attention:** a position cannot attend to future positions. This is required for autoregressive generation.

The causal mask contains $0$ for allowed positions and $-\infty$ for forbidden positions before softmax. Forbidden positions therefore obtain probability zero.

> **Formula box — scaled dot-product attention**
>
> $$
> Q=XW_Q,\qquad K=XW_K,\qquad V=XW_V
> $$
>
> $$
> \operatorname{Attention}(Q,K,V)
> =\operatorname{softmax}\left(\frac{QK^\top}{\sqrt{d_k}}+M\right)V.
> $$
>
> $M=0$ in unmasked attention. In causal attention it contains $-\infty$ above the permitted region.
>
> The division by $\sqrt{d_k}$ prevents dot products from becoming too large as dimensionality grows, which would make softmax excessively sharp.

## 2.3 Multi-head attention

One attention pattern may not capture every relation. **Multi-head attention** uses separate projections for each head:

$$
\text{head}_j=\operatorname{Attention}(QW_j^Q,KW_j^K,VW_j^V).
$$

The heads are concatenated and projected:

$$
\operatorname{MHA}(Q,K,V)
=\operatorname{Concat}(\text{head}_1,\ldots,\text{head}_h)W_O.
$$

Different heads can specialize in different relationships or representation subspaces. A head is not guaranteed to have a clean human interpretation, but the architecture gives the model multiple independent retrieval mechanisms.

## 2.4 Transformer encoder

A standard encoder layer contains:

1. multi-head self-attention;
2. residual connection and normalization;
3. position-wise feed-forward network;
4. another residual connection and normalization.

Stacking layers gradually builds richer contextual representations. Residual connections:

- improve gradient flow;
- preserve the previous representation when useful;
- make deep training more stable.

The feed-forward network processes each token separately using the same parameters. Attention mixes information **between tokens**; the feed-forward network transforms features **within each token**.

## 2.5 Transformer decoder

A decoder layer contains:

1. masked self-attention over the generated prefix;
2. cross-attention to encoder outputs;
3. a feed-forward network;
4. residual connections and normalization around the blocks.

During training, the target sequence is **shifted right**. For target “I like cats”, the decoder might receive `<start> I like` and be trained to output `I like cats`.

The final decoder representation is projected to vocabulary size, followed by softmax.

## 2.6 Positional information

Self-attention alone is permutation-equivariant: without position information, it cannot distinguish token order. Positional encodings are added to token embeddings.

> **Formula box — sinusoidal positional encoding**
>
> $$
> PE(pos,2i)=\sin\left(\frac{pos}{10000^{2i/d}}\right),
> $$
>
> $$
> PE(pos,2i+1)=\cos\left(\frac{pos}{10000^{2i/d}}\right).
> $$
>
> Learned positional embeddings are another valid choice.

## 2.7 Transformers versus RNNs

Transformers became dominant because:

- any visible pair of positions can interact in one attention layer;
- training is parallel across sequence positions;
- long-range relationships do not require passing through every intermediate state;
- they avoid the specifically recurrent form of vanishing/exploding gradients.

Limitations:

- full attention has $O(T^2)$ time and memory in sequence length;
- positional information must be supplied;
- large data and compute are often needed;
- autoregressive inference is still sequential.

---

# 3. Graph signals, graph filters, and GNNs

## 3.1 Graph definitions

A weighted graph can be written as $G=(V,E,W)$:

- $V$: nodes;
- $E$: edges;
- $W$: edge weights.

Graphs may be:

- directed or undirected;
- weighted or unweighted;
- static or dynamic.

For the convention used in the graph lecture, edge $(i,j)$ means that node $j$ can influence node $i$. In directed graphs, $(i,j)\neq(j,i)$. In undirected weighted graphs, both connectivity and weights are symmetric.

The neighborhood $N_i$ contains nodes connected to $i$. A weighted degree is the sum of incident edge weights.

## 3.2 Graph signals and shift operators

A **graph signal** assigns a value or feature vector to every node. For one feature:

$$
x\in\mathbb R^N.
$$

A **graph shift operator** $S\in\mathbb R^{N\times N}$ encodes graph structure. Common choices include:

- adjacency matrix;
- graph Laplacian;
- normalized adjacency;
- random-walk matrix.

Multiplication $Sx$ diffuses or aggregates information from one-hop neighbors. Repeated multiplication expands the receptive field:

- $Sx$: one-hop information;
- $S^2x$: up to two-hop information;
- $S^kx$: up to $k$-hop information.

This is local computation because entries of $S$ are zero outside edges and, optionally, self-connections.

## 3.3 Graph filters and graph convolution

A graph filter is a polynomial in $S$. The same coefficients can be applied on different graphs; the graph-specific behavior comes from the graph’s own $S$.

> **Formula box — graph filtering**
>
> $$
> H(S)=\sum_{k=0}^{K-1}h_kS^k,
> \qquad
> y=H(S)x=\sum_{k=0}^{K-1}h_kS^kx.
> $$
>
> With multiple input and output features:
>
> $$
> Y=\sum_{k=0}^{K-1}S^kXH_k.
> $$
>
> Powers of $S$ propagate across the graph; $H_k$ mixes feature channels.

This generalizes ordinary convolution:

- a time series is a signal on a line graph;
- an image is a signal on a grid graph;
- a graph convolution performs analogous local aggregation on an irregular graph.

## 3.4 From graph convolution to a GNN

A GNN stacks:

1. graph aggregation/filtering;
2. learned feature transformation;
3. pointwise nonlinearity.

After $L$ message-passing layers, a node can use information from roughly an $L$-hop neighborhood.

A generic message-passing view is:

1. construct messages from neighbors;
2. aggregate messages using a permutation-invariant operation;
3. update the node state.

Parameter sharing is essential: the same rule is applied to every node, so parameter count need not grow with the number of nodes.

## 3.5 Kipf–Welling GCN

The GCN simplifies a general graph filter to immediate-neighbor aggregation, shares the transformation for self and neighbor contributions, adds self-loops, and normalizes by degrees.

> **Formula box — GCN layer**
>
> Add self-loops:
>
> $$
> \widetilde A=A+I.
> $$
>
> Let $\widetilde D_{ii}=\sum_j\widetilde A_{ij}$. Define:
>
> $$
> \widehat A=\widetilde D^{-1/2}\widetilde A\widetilde D^{-1/2}.
> $$
>
> Then:
>
> $$
> H^{(\ell+1)}
> =\sigma\left(\widehat A H^{(\ell)}W^{(\ell)}\right),
> \qquad H^{(0)}=X.
> $$

Interpretation:

- $\widetilde A$ includes the node itself;
- normalization prevents high-degree nodes from dominating merely because they have many neighbors;
- $\widehat AH^{(\ell)}$ aggregates;
- $W^{(\ell)}$ transforms features;
- $\sigma$ adds nonlinearity.

Potential deep-GNN problems:

- **oversmoothing:** node embeddings become too similar;
- **oversquashing:** information from a rapidly growing neighborhood is compressed into a fixed-size vector.

---

# 4. Recommender systems

## 4.1 Problem setup and feedback

We have:

- users $U$;
- items $I$;
- historical interactions such as $(u,i)$, $(u,i,r)$, or $(u,i,r,t)$;
- optional user/item metadata.

The goal is usually to return a personalized top-$K$ list.

Feedback can be:

- **explicit:** ratings or direct preferences;
- **implicit:** clicks, views, purchases, listens.

Implicit feedback is abundant but noisy. A missing interaction does not necessarily mean dislike; it may mean the user never saw the item.

Representations of interaction data:

- user–item matrix;
- interaction sequence;
- bipartite or more general graph.

## 4.2 Main recommender families

- **Collaborative filtering (CF):** infer preferences from interaction patterns among users and items.
- **Content-based filtering (CBF):** use item/user attributes.
- **Hybrid methods:** combine interactions and metadata.

Important problems:

- **cold start:** no or few interactions for a new user/item;
- extreme sparsity;
- popularity and exposure bias;
- false negatives in implicit data;
- scalability;
- noisy feedback.

## 4.3 Matrix factorization

Assign user embedding $p_u\in\mathbb R^D$ and item embedding $q_i\in\mathbb R^D$.

> **Formula box — matrix factorization**
>
> $$
> \hat r_{ui}=p_u^\top q_i,
> \qquad
> R\approx PQ^\top.
> $$
>
> A regularized explicit-rating objective:
>
> $$
> \mathcal L=
> \frac12\sum_{u,i}I_{ui}(r_{ui}-p_u^\top q_i)^2
> +\frac{\lambda_U}{2}\sum_u\|p_u\|^2
> +\frac{\lambda_I}{2}\sum_i\|q_i\|^2.
> $$

To recommend:

1. score candidate items;
2. remove items that should not be recommended, commonly already consumed ones;
3. sort by score;
4. return the top $K$.

### Related matrix-factorization variants

- **SVD:** algebraic low-rank decomposition. A raw sparse interaction matrix must usually be completed or handled specially because ordinary SVD expects a fully specified matrix.
- **NMF:** factorization with nonnegative factors, often giving additive, parts-based representations.
- **FunkSVD:** directly optimizes latent factors on observed ratings rather than applying literal matrix SVD.
- **SVD++:** adds information from a user’s implicit-feedback history to the user representation.

Practical score transforms:

- a $1,\ldots,K$ rating can be scaled to $[0,1]$ with $(x-1)/(K-1)$;
- an unbounded dot product can be mapped to $[0,1]$ with a sigmoid;
- a user representation can combine a user-specific vector with an aggregate of embeddings of interacted items.

## 4.4 Probabilistic matrix factorization

PMF interprets:

- user and item embeddings as Gaussian-distributed latent variables;
- observed ratings as Gaussian around $p_u^\top q_i$.

Maximizing the posterior is equivalent to squared-error minimization with $L_2$ regularization. This gives a probabilistic justification for the standard MF objective.

The observation indicator $I_{ui}$ ensures that only known interactions contribute. Training scales with the number of observations rather than all possible matrix entries.

## 4.5 Ranking metrics

For implicit recommendation, ranking quality is usually more meaningful than rating error.

> **Formula box — common top-$K$ metrics**
>
> $$
> \operatorname{Precision@K}
> =\frac{\#\text{ relevant items in top }K}{K},
> $$
>
> $$
> \operatorname{Recall@K}
> =\frac{\#\text{ relevant items in top }K}
> {\#\text{ relevant items}}.
> $$
>
> Hit Rate is $1$ for a user if at least one relevant item is in the top $K$, otherwise $0$, then averaged.
>
> If the first relevant item is at rank $r_u$:
>
> $$
> \operatorname{MRR}=\frac1{|U|}\sum_u\frac1{r_u}.
> $$
>
> $$
> DCG@K=\sum_{r=1}^{K}\frac{2^{rel_r}-1}{\log_2(r+1)},
> \qquad
> NDCG@K=\frac{DCG@K}{IDCG@K}.
> $$
>
> NDCG rewards relevant items more when they occur near the top and can use graded relevance.

**AUC** measures how often a random positive item is scored above a random negative. MRR emphasizes the first relevant result; Recall@K emphasizes coverage; NDCG emphasizes rank-sensitive graded quality.

Evaluation must avoid information leakage. For sequential recommendation, a common split uses:

- last user interaction for testing;
- second-last for validation;
- earlier interactions for training.

## 4.6 Bayesian Personalized Ranking

BPR directly optimizes pairwise ranking for implicit feedback.

Training data contain triples:

$$
(u,i,j),\quad i\in I_u^+,\quad j\notin I_u^+,
$$

where $i$ is observed and $j$ is a sampled unobserved item.

> **Formula box — BPR**
>
> $$
> \hat x_{uij}=\hat x_{ui}-\hat x_{uj},
> $$
>
> $$
> \mathcal L_{\text{BPR}}
> =-\sum_{(u,i,j)}\log\sigma(\hat x_{uij})
> +\lambda\|\Theta\|^2.
> $$
>
> The loss is small when the positive item receives a higher score than the negative item.

Negative sampling is therefore part of the method. Unobserved items can be false negatives.

## 4.7 Alignment and uniformity in recommendation

For positive user–item pairs, **alignment** pulls their normalized embeddings together. **Uniformity** spreads user and item embeddings across the hypersphere, reducing collapse and often improving ranking.

The conceptual tradeoff:

- alignment alone can collapse representations;
- uniformity alone does not encode preferences;
- a useful model needs both.

**DirectAU** is the recommender approach that directly optimizes this alignment–uniformity decomposition instead of treating it only as an analysis tool.

## 4.8 Factorization Machines

A **Factorization Machine (FM)** models sparse tabular data with first-order terms and pairwise feature interactions. Instead of learning a separate parameter for every feature pair, it factorizes interaction weights through latent vectors.

> **Formula box — second-order FM**
>
> $$
> \hat y(x)=w_0+\sum_iw_ix_i+
> \sum_{i<j}\langle v_i,v_j\rangle x_ix_j.
> $$

This combines the flexibility of polynomial feature interactions with parameter sharing similar to matrix factorization, making it useful for sparse recommender features.

## 4.9 LightFM

LightFM combines collaborative and content information:

- a user is represented by the sum of embeddings of its active user features;
- an item is represented by the sum of embeddings of its active item features;
- feature biases are summed similarly;
- the score is based on a user–item dot product plus biases.

This helps cold start because a new item can still be represented by metadata features. It is an example of **information fusion**.

- **Early/representation-level fusion:** combine sources before or inside the shared representation model, as LightFM does by summing feature embeddings.
- **Late fusion:** train separate models or branches and combine their final scores or decisions.

## 4.10 Neural Collaborative Filtering

NCF replaces or augments the fixed dot-product interaction with a neural network.

- **Generalized Matrix Factorization (GMF):** elementwise product of user and item embeddings, followed by a learned output.
- **MLP branch:** concatenate embeddings and process them through a tower of nonlinear layers.
- **NeuMF:** combines GMF and MLP branches, often after pretraining them separately.

For implicit interactions, binary cross-entropy is used. Deep models increase interaction-function flexibility, but dot products remain common because they are efficient for large-scale retrieval.

## 4.11 SASRec: sequential recommendation

SASRec uses a causally masked Transformer to predict the next item.

Input:

- fixed-length interaction sequence;
- truncation or zero-padding;
- item embeddings plus trainable positional embeddings.

Architecture:

- masked self-attention;
- feed-forward network;
- residual connections;
- layer normalization;
- dropout;
- stacked blocks.

The task is shifted-sequence prediction:

$$
(S_1^u,\ldots,S_{n-1}^u)\rightarrow(S_2^u,\ldots,S_n^u).
$$

The model combines:

- long-range access similar to attention;
- focus on a small number of especially relevant past actions;
- dot-product item scoring;
- binary cross-entropy with sampled negatives.

## 4.12 LightGCN

Represent recommendation data as a bipartite graph:

- user and item nodes;
- an edge for each interaction;
- block adjacency $A=\begin{bmatrix}0&R\\R^\top&0\end{bmatrix}$.

LightGCN removes feature transformations and nonlinearities from a conventional GCN because ablation studies found them unnecessary for collaborative filtering.

> **Formula box — LightGCN**
>
> $$
> V^{(\ell)}=D^{-1/2}AD^{-1/2}V^{(\ell-1)}.
> $$
>
> Final embedding:
>
> $$
> V=\frac{1}{L+1}\sum_{\ell=0}^{L}V^{(\ell)}.
> $$

The initial user/item embeddings are the trainable parameters. Averaging layers combines self-information ($\ell=0$) with neighborhoods of different radii.

## 4.13 Contrastive graph recommendation and SimGCL

Graph augmentations such as node or edge dropout create two views. A contrastive loss encourages the same user/item across views to be similar.

**SGL** combines graph augmentations with InfoNCE. **SimGCL** emphasizes that the contrastive objective may matter more than complicated graph augmentations. Adding embedding noise can create useful views and promote a more uniform embedding distribution.

The lecture also pointed to diffusion-based recommendation/augmentation as a modern direction: a generative process can model or denoise interaction structure instead of relying only on manual node/edge deletion.

> **Formula box — InfoNCE**
>
> For normalized anchor $z$, positive $z^+$, negatives $z_j^-$, and temperature $\tau$:
>
> $$
> \ell=-\log
> \frac{\exp(\operatorname{sim}(z,z^+)/\tau)}
> {\exp(\operatorname{sim}(z,z^+)/\tau)+
> \sum_j\exp(\operatorname{sim}(z,z_j^-)/\tau)}.
> $$

In graph recommenders, a common combined objective is **BPR + InfoNCE**.

## 4.14 Current recommender challenges

- explainability;
- multimodal items;
- multiple behavior types;
- conversational recommendation;
- fairness;
- exposure and popularity bias;
- false negatives;
- sparsity and cold start;
- noisy implicit data;
- scalability.

---

# 5. Classical time-series methods

## 5.1 Definitions and tasks

A time series is an ordered sequence:

$$
x_1,x_2,\ldots,x_t,\ldots
$$

and is treated as one realization of a stochastic process:

$$
X_1,X_2,\ldots,X_t,\ldots
$$

Main tasks:

- forecasting future values;
- classifying complete windows;
- clustering series;
- anomaly detection;
- imputation;
- filtering a latent state from noisy measurements.

For forecasting at time $t$, a history window of length $L$ maps to a horizon of length $H$:

$$
x_{t-L+1:t}\mapsto \hat y_{t+1:t+H}.
$$

## 5.2 Stationarity and preprocessing

A weakly stationary process has:

- constant mean;
- autocovariance depending only on lag, not absolute time.

> **Formula box — weak stationarity**
>
> $$
> \mathbb E[X_t]=\mu,
> \qquad
> \operatorname{Cov}(X_t,X_{t+k})=\gamma_k.
> $$

Real series are often non-stationary due to:

- trend;
- seasonality;
- level shifts;
- anomalies;
- changing variance.

Common preprocessing:

- estimate and remove or divide by trend;
- estimate and remove seasonality;
- difference the series;
- transform or normalize changing variance;
- correct obvious anomalies when justified.

Additive and multiplicative decompositions express different assumptions. Dividing by trend is appropriate when variation scales with the level; subtracting assumes additive effects.

## 5.3 Forecasting baselines

Always compare complex methods with simple baselines:

- global mean;
- previous value / naive forecast;
- seasonal naive;
- moving average;
- linear regression on lagged values.

Simple baselines reveal whether a sophisticated model is learning more than persistence, trend, or seasonality.

## 5.4 AR, MA, ARMA, and ARIMA

> **Formula box — classical models**
>
> Autoregressive model:
>
> $$
> X_t=c+\sum_{i=1}^{p}\phi_iX_{t-i}+\varepsilon_t.
> $$
>
> Moving-average model:
>
> $$
> X_t=c+\varepsilon_t+\sum_{j=1}^{q}\theta_j\varepsilon_{t-j}.
> $$
>
> ARMA combines both:
>
> $$
> X_t=c+\sum_{i=1}^{p}\phi_iX_{t-i}
> +\varepsilon_t+\sum_{j=1}^{q}\theta_j\varepsilon_{t-j}.
> $$
>
> Sign conventions for the MA terms may differ.
>
> First difference:
>
> $$
> \Delta X_t=X_t-X_{t-1}.
> $$
>
> ARIMA$(p,d,q)$ applies ARMA$(p,q)$ after differencing $d$ times.

A random walk is AR(1) with $c=0$ and $\phi_1=1$. It is not stationary in level.

Interpretation:

- AR uses previous observations;
- MA uses previous shocks/errors;
- $d$ handles non-stationarity through differencing.

## 5.5 Model-order and parameter selection

- inspect autocorrelation and related diagnostics;
- compare candidate models;
- use information criteria;
- estimate coefficients with least squares, maximum likelihood, moments, or Yule–Walker for AR models.

> **Formula box — AIC and BIC**
>
> $$
> AIC=2k-2\log L,
> $$
>
> $$
> BIC=k\log N-2\log L.
> $$
>
> $k$ is the number of model parameters, $N$ the sample count, and $L$ the maximized likelihood. Lower is preferred. BIC penalizes complexity more strongly as $N$ grows.

Yule–Walker uses stationarity to relate autocorrelations to AR coefficients. For AR($p$):

$$
r=R\phi,\qquad \phi=R^{-1}r,
$$

where $R$ is the Toeplitz autocorrelation matrix. The important idea is that AR parameters can be recovered from lagged correlation structure.

## 5.6 Time-series validation and errors

Do not randomly mix past and future when deployment is chronological.

Rules:

- train on earlier data and validate/test on later data;
- fit scalers and preprocessors only on training data;
- ensure every feature was actually known at prediction time;
- evaluate by horizon because long-horizon behavior can differ from one-step behavior;
- avoid near-duplicate overlapping train/test windows.

> **Formula box — point forecast errors**
>
> $$
> MSE=\frac1T\sum_{t=1}^{T}(x_t-\hat x_t)^2,
> $$
>
> $$
> RMSE=\sqrt{MSE},
> \qquad
> MAE=\frac1T\sum_{t=1}^{T}|x_t-\hat x_t|,
> $$
>
> $$
> MAPE=\frac1T\sum_{t=1}^{T}
> \frac{|x_t-\hat x_t|}{|x_t|}.
> $$
>
> MSE/RMSE penalize large errors strongly. MAE is more robust. MAPE is problematic near zero.

For probabilistic forecasting, use metrics such as negative log-likelihood or CRPS and check calibration.

## 5.7 Time-series distances and DTW

Direct Euclidean distance assumes:

- equal lengths;
- point $t$ in one series corresponds to point $t$ in the other;
- shifts and speed differences should count as dissimilarity.

Alternatives:

- normalize and use Euclidean/Manhattan/cosine distance;
- extract features such as mean, standard deviation, or slope;
- use **Dynamic Time Warping (DTW)**.

DTW finds a monotonic alignment that may repeat points so similar shapes can match despite local time shifts.

> **Formula box — DTW recurrence**
>
> $$
> D(i,j)=d(s_i,t_j)+
> \min\{D(i-1,j),D(i,j-1),D(i-1,j-1)\}.
> $$
>
> Initialize $D(0,0)=0$ and other zero-row/zero-column entries to $\infty$. The result is $D(n,m)$.
>
> Complexity is $O(nm)$.

The path must:

- begin at the first elements;
- end at the last elements;
- be monotonic;
- cover both sequences.

DTW can over-warp, is computationally expensive, and requires an appropriate local distance.

## 5.8 Time-series clustering

Approaches:

1. group regular daily/weekly/monthly profiles;
2. treat equal-length series as vectors and apply ordinary clustering;
3. use a time-series distance such as DTW;
4. use DTW-aware centers, especially **DBA-k-means**.

Ordinary k-means alternates:

- assign each point to the nearest center;
- replace each center by the arithmetic mean of assigned points.

This works because the arithmetic mean minimizes squared Euclidean error. It does not directly define the correct center under DTW.

**DTW Barycenter Averaging (DBA)**:

1. start with a candidate center sequence;
2. align every cluster member to the center with DTW;
3. collect values matched to each center position;
4. replace each center value by the mean of its matches;
5. iterate.

DBA-k-means therefore replaces the Euclidean centroid update with a DTW-aware barycenter update.

## 5.9 Time-series classification

Main approaches:

- nearest pattern or prototype;
- vectorize and use kNN/SVM;
- use DTW in nearest-neighbor classification;
- feature-based representations;
- shapelets;
- learned neural representations.

**Time Series Forest** samples intervals and extracts features such as:

- mean;
- standard deviation;
- slope.

Trees classify the resulting feature vectors. This gives some interpretability: important intervals and statistics can be inspected.

## 5.10 Shapelets

A **shapelet** is a short discriminative pattern. A series is represented by its distance to each learned or selected shapelet.

> **Formula box — shapelet distance**
>
> For shapelet $S=(s_1,\ldots,s_L)$ and series $X_i$ of length $Q$:
>
> $$
> M_{ik}=
> \min_{j=1,\ldots,Q-L+1}
> \frac1L\sum_{\ell=1}^{L}
> (x^{(i)}_{j+\ell-1}-s^{(k)}_\ell)^2.
> $$
>
> The shapelet representation is:
>
> $$
> m_i=(M_{i1},\ldots,M_{iK}).
> $$

### Brute-force shapelet discovery

1. enumerate candidate subsequences;
2. compute candidate-to-series distances;
3. choose a distance threshold that splits training data;
4. evaluate information gain;
5. keep highly discriminative shapelets.

This is expensive. Early abandoning and entropy-based pruning reduce work.

### Learning shapelets

Shapelet values can be optimized jointly with a classifier. Because the hard minimum is non-differentiable, replace it with a differentiable soft minimum. Logistic regression then operates on shapelet distances, and gradient descent updates both classifier weights and shapelet values.

The conceptual advantage is task-specific shapelets; the risk is non-convex optimization and reduced direct correspondence to an observed subsequence.

---

# 6. Neural sequence models for time series

## 6.1 Turning a time series into supervised examples

For every valid prediction time $t$, construct:

- input window $x_{t-L+1:t}$;
- target horizon $y_{t+1:t+H}$.

Important design choices:

- history length $L$;
- forecast horizon $H$;
- which channels are targets;
- which future covariates are genuinely known;
- direct or recursive prediction;
- point or probabilistic output.

A multivariate series $X\in\mathbb R^{T\times C}$ contains:

- temporal dependencies within channels;
- cross-channel dependencies;
- possibly exogenous variables such as weather, prices, or calendar features.

Channel order may be arbitrary even though temporal order is meaningful.

## 6.2 Vanilla RNN

An RNN compresses the history into a hidden state:

$$
h_t\approx\text{useful summary of }x_{1:t}.
$$

The same update rule is reused at every time step.

> **Formula box — vanilla RNN**
>
> $$
> h_t=\tanh(W_xx_t+W_hh_{t-1}+b),
> $$
>
> $$
> \hat y_t=g_\theta(h_t).
> $$

Unrolling an RNN creates a deep network whose depth equals sequence length. **Backpropagation Through Time (BPTT)** differentiates through this unrolled graph and sums parameter gradients over all time steps.

Output patterns:

- many-to-one: classify a complete window from the final state;
- many-to-many: predict at every time step;
- encoder–decoder: encode history and decode a future sequence.

## 6.3 Vanishing and exploding gradients

Gradients through time contain products of temporal Jacobians:

$$
\frac{\partial h_T}{\partial h_t}
=\prod_{k=t+1}^{T}\frac{\partial h_k}{\partial h_{k-1}}.
$$

- typical singular values below $1$ cause exponential decay;
- values above $1$ cause explosion.

Vanishing gradients make long-range credit assignment difficult. Exploding gradients produce unstable updates, huge losses, and NaNs.

> **Formula box — global gradient clipping**
>
> $$
> g\leftarrow g\cdot
> \min\left(1,\frac{\tau}{\|g\|_2}\right).
> $$
>
> Clipping controls explosion but does not solve vanishing gradients.

## 6.4 Multi-step forecasting and teacher forcing

Strategies:

- **direct:** one model outputs the entire horizon;
- **recursive:** predict one step and feed the prediction back;
- **sequence-to-sequence:** encode history and use a decoder for the horizon.

With **teacher forcing**, a decoder receives the true previous target during training but its own prediction at inference. This creates **exposure bias**: deployment inputs differ from training inputs, and errors can accumulate.

**Scheduled sampling** sometimes replaces the true previous target with the model’s prediction during training.

Tradeoff:

- recursive methods share a one-step mechanism but accumulate errors;
- direct methods avoid feedback but may blur multiple plausible futures;
- performance should be inspected separately for each horizon.

## 6.5 LSTM

LSTM separates:

- a cell state $c_t$, intended as longer-term memory;
- a hidden state $h_t$, the exposed task-relevant state.

Gates decide what to forget, write, and reveal.

> **Formula box — LSTM**
>
> $$
> f_t=\sigma(W_f[x_t,h_{t-1}]+b_f),
> $$
>
> $$
> i_t=\sigma(W_i[x_t,h_{t-1}]+b_i),
> $$
>
> $$
> \widetilde c_t=\tanh(W_c[x_t,h_{t-1}]+b_c),
> $$
>
> $$
> c_t=f_t\odot c_{t-1}+i_t\odot\widetilde c_t,
> $$
>
> $$
> o_t=\sigma(W_o[x_t,h_{t-1}]+b_o),
> \qquad
> h_t=o_t\odot\tanh(c_t).
> $$

Interpretation:

- **forget gate $f_t$:** preserve seasonal state or discard obsolete regimes;
- **input gate $i_t$:** decide whether a new spike is noise or a lasting change;
- **output gate $o_t$:** expose only the currently relevant part of memory.

The additive cell update gives gradients a more direct path:

$$
\frac{\partial c_t}{\partial c_{t-1}}\approx f_t.
$$

When $f_t$ is near $1$, memory and gradients can persist.

Variants:

- peephole LSTM;
- projection LSTM;
- ConvLSTM for grid-valued sequences;
- bidirectional LSTM for non-causal tasks such as classification or imputation.

Bidirectional models must not be used for causal forecasting unless future context is actually available.

## 6.6 GRU

GRU has one state and two gates, usually fewer parameters than LSTM.

> **Formula box — GRU**
>
> $$
> z_t=\sigma(W_zx_t+U_zh_{t-1}+b_z),
> $$
>
> $$
> r_t=\sigma(W_rx_t+U_rh_{t-1}+b_r),
> $$
>
> $$
> \widetilde h_t=
> \tanh(W_hx_t+U_h(r_t\odot h_{t-1})+b_h),
> $$
>
> $$
> h_t=(1-z_t)\odot h_{t-1}+z_t\odot\widetilde h_t.
> $$

- reset gate $r_t$: controls how much old state contributes to the candidate;
- update gate $z_t$: interpolates between old and new state.

LSTM versus GRU is empirical:

- LSTM offers a separate memory cell;
- GRU is simpler and often faster;
- vanilla RNN is smallest but least reliable for long dependencies.

## 6.7 Practical recurrent modeling

- start with one or two recurrent layers;
- normalize channels using training-only statistics;
- use a linear or small MLP horizon head;
- apply dropout carefully, commonly between recurrent layers;
- clip gradients;
- use time-ordered early stopping;
- compare with linear, naive, and seasonal baselines.

Limitations:

- sequential computation is difficult to parallelize;
- the history is compressed into a fixed-size state;
- very long contexts remain difficult;
- high-dimensional cross-channel relations may be awkward.

## 6.8 Modern recurrent and state-space models

Modern sequence models try to combine:

- recurrent, constant-memory inference;
- parallel training;
- long-context efficiency.

A general linear state-space recurrence is:

> **Formula box — state-space model**
>
> $$
> s_t=A_ts_{t-1}+B_tx_t,
> \qquad
> y_t=C_ts_t.
> $$

If the update can be expressed associatively, parallel scan algorithms can train it efficiently.

### RWKV

RWKV combines Transformer-like training with an RNN-style state update. It uses a linear-attention idea and has context-independent inference memory.

### xLSTM

xLSTM modernizes LSTM-style memory:

- **sLSTM:** scalar memory with exponential gating;
- **mLSTM:** matrix memory with parallelizable updates;
- deep residual block structure.

### S4 and Mamba

Structured state-space models learn efficient long filters. **Mamba** makes the state-space parameters depend on the current input:

$$
A_t,B_t,C_t=\operatorname{functions}(x_t).
$$

This **selectivity** lets the model:

- ignore noise;
- retain regime information;
- react to shocks;
- scale linearly with sequence length.

A Mamba block typically includes:

- input projection;
- local convolution;
- selective scan;
- gating;
- output projection;
- residual connection and normalization.

Mamba-2 develops the relationship between structured state-space computations and attention-like matrices. Mamba4Cast applies Mamba-style modeling to zero-shot forecasting.

### Complexity intuition

| Backbone | Training | Inference memory/state | Main strength |
|---|---|---:|---|
| RNN/LSTM | sequential | $O(d)$ | simple stateful recurrence |
| Transformer | parallel | cache grows with context | direct content retrieval |
| SSM/Mamba | parallel scan | $O(d)$ | long dense streams |
| Hybrid | mixed | mixed | combines state and retrieval |

Do not assume a new model automatically wins. Time-series benchmarks are sensitive to preprocessing, and strong linear models often remain competitive.

## 6.9 Transformers for time series

Self-attention is applied across timestamps or temporal patches.

Why useful:

- direct access to old events;
- short path between any visible time points;
- parallel training;
- flexible multivariate interactions.

Time-series positional/covariate encodings can include:

- absolute or relative position;
- time of day;
- weekday/month;
- observation age;
- irregular time gaps;
- future-known calendar variables.

### Patching

Split the sequence into fixed-length patches and embed each patch as a token.

Benefits:

- shorter effective token sequence;
- cheaper attention;
- local shape encoded inside a token;
- convenient multi-step outputs.

### Architecture choice

- **encoder-only:** classification, imputation, representation learning; usually bidirectional;
- **decoder-only:** causal autoregressive forecasting;
- **encoder–decoder:** map an observed history to a future horizon.

### Probabilistic heads

A model may output $\mu_{t+h}$ and $\sigma_{t+h}$ for a Gaussian, quantiles, mixture parameters, or discrete tokens. The purpose is to represent uncertainty rather than only a mean.

### Long-horizon pitfalls

- recursive error accumulation;
- direct predictions averaging multiple futures;
- attention overfitting calendar artifacts;
- poor tail behavior hidden by average metrics;
- quadratic attention cost.

Patching, sparse/linear attention, and state-space models address the cost in different ways.

## 6.10 Time-series foundation models and generative forecasting

- **Chronos:** scales and quantizes numeric values into tokens, trains a language-model objective, and samples probabilistic forecasts.
- **TimesFM:** patched decoder-only zero-shot forecasting model.
- **Moirai:** universal forecasting Transformer for varied frequencies and numbers of variables.
- **MOMENT:** general time-series foundation models supporting multiple tasks.
- **Diffusion forecasting:** learns to denoise future trajectories, useful for multimodal futures but often slower.
- **Retrieval/test-time memory:** explicitly retrieve similar historical events or update memory during inference.

Model selection:

- small data and short sequences: classical baselines, GRU/LSTM;
- long dense streams and fast rolling inference: SSM/Mamba;
- specific old events matter: attention or hybrids;
- zero-shot use: foundation models;
- risk matters: calibrated probabilistic outputs.

---

# 7. Self-supervised learning for time series

## 7.1 What a time-series representation should preserve

For $X_i\in\mathbb R^{T\times C}$, an encoder returns timestamp vectors:

$$
f_\theta(X_i)=Z_i=[z_{i1},\ldots,z_{iT}]
\in\mathbb R^{T\times K}.
$$

Granularity:

- timestamp-level;
- subsequence-level by pooling an interval;
- instance-level by global pooling.

Different downstream tasks need different granularity:

- classification: pooled instance representation;
- forecasting: latest or window representation;
- anomaly detection: timestamp representation;
- imputation: local contextual representation.

Useful information includes:

- local shape and motifs;
- magnitude when semantically meaningful;
- order, phase, trend, and seasonality;
- context;
- instance identity;
- uncertainty.

## 7.2 Why augmentations are difficult

An augmentation defines an invariance. In time series:

- scaling may change meaning;
- cropping may change regime;
- nearby points can differ at anomalies;
- distant points may be similar due to periodicity;
- missingness may carry information.

Therefore “two augmented views should be equal” is not automatically safe.

SSL also does not remove leakage:

- do not pretrain on test-period data if it would be unavailable at deployment;
- fit scalers/imputers on training data;
- use chronological validation when required.

## 7.3 Contrastive and masked objectives

Contrastive learning chooses positives and negatives. The key question is not merely the formula but whether pair construction reflects true semantic similarity.

Masked modeling hides values or patches and reconstructs them:

$$
\widetilde X=\operatorname{mask}(X),
\qquad
\widehat X_M=g_\phi(f_\theta(\widetilde X)).
$$

Reconstruction is useful for imputation and local context learning, but may emphasize low-level accuracy over task-relevant semantics.

The encoder should be evaluated with simple downstream heads and across several tasks.

## 7.4 TS2Vec

TS2Vec learns timestamp-level representations with:

1. two overlapping crops of the same series;
2. independently masked projected timestamps;
3. a dilated CNN encoder;
4. temporal and instance contrast;
5. hierarchical pooling and repeated contrast at multiple scales.

### Architecture

1. project raw observation $x_{it}$ into latent space;
2. mask projected vectors, not raw values;
3. use a dilated CNN to create contextual timestamp vectors.

Why mask after projection:

- zero may be a valid raw value;
- raw ranges differ by dataset;
- the zero latent vector can be a clear mask.

### Contextual consistency

Take overlapping crops:

$$
[a_1,b_1],\qquad[a_2,b_2],
\qquad a_1\le a_2\le b_1\le b_2.
$$

Use only overlap $\Omega=[a_2,b_1]$. The positive pair is the representation of the **same original timestamp** under two different surrounding contexts.

This avoids forcing:

- nearby but anomalous points to be similar;
- different level regimes in the same long subseries to be similar.

### Dual contrast

- **temporal contrast:** for one instance, distinguish a timestamp from other timestamps;
- **instance contrast:** at one timestamp, distinguish one series from other series in the batch.

> **Formula box — TS2Vec dual objective**
>
> In conceptual form:
>
> $$
> \mathcal L_{\text{dual}}
> =\frac1{BT}\sum_{i,t}
> \left(\ell^{(i,t)}_{\text{temp}}+
> \ell^{(i,t)}_{\text{inst}}\right).
> $$
>
> Each component is an InfoNCE-like objective. The positive is the same timestamp and instance in the other view; negatives vary either timestamp or instance.

### Hierarchical contrast

After applying the dual objective at timestamp resolution:

1. max-pool representations along time by factor $2$;
2. apply the objective again;
3. repeat until one temporal element remains;
4. average the losses across scales.

This teaches both fine and coarse temporal semantics.

### Downstream use

- classification: global max pool, then SVM/linear probe;
- forecasting: use the last contextual vector and fit a linear model;
- anomaly detection: compare representations with the point visible versus masked;
- arbitrary subsequence: pool its timestamp vectors.

The central TS2Vec insight is the construction of context views and multiscale timestamp representations, not just “use a contrastive loss.”

## 7.5 T-Rep

T-Rep keeps the TS2Vec-style timestamp encoder but adds an explicit learned representation of time.

Components:

1. signal projection $x_{it}\mapsto u_{it}$;
2. time embedding $t\mapsto\tau_t$;
3. concatenate signal and time embeddings;
4. dilated CNN encoder.

$$
z_{it}=f_\theta([u_{it};\tau_t]).
$$

### Time2Vec intuition

> **Formula box — Time2Vec-style embedding**
>
> $$
> h_\psi(t)=
> [\omega_0t+\phi_0,
> \sin(\omega_1t+\phi_1),\ldots,
> \sin(\omega_Kt+\phi_K)].
> $$
>
> The linear component can model trend; periodic components can learn cycles and phase.

The embedding is normalized into a probability-like vector so temporal embeddings can be compared with Jensen–Shannon divergence.

> **Formula box — Jensen–Shannon divergence**
>
> $$
> JSD(p\|q)=
> \frac12KL(p\|m)+\frac12KL(q\|m),
> \qquad
> m=\frac{p+q}{2}.
> $$

### Four T-Rep pretext tasks

1. TS2Vec instance contrast;
2. TS2Vec temporal contrast;
3. **time-embedding divergence prediction:** from two representation vectors, predict the JSD between their time embeddings;
4. **time-embedding-conditioned forecasting:** from $z_{it}$ and target time embedding $\tau_{t+\Delta}$, predict nearby representation $z_{i,t+\Delta}$.

The fourth task can predict forward or backward locally. It encourages representations to encode their temporal neighborhood without turning pretraining into only long-horizon forecasting.

The total loss is a weighted combination of four tasks, applied hierarchically as in TS2Vec.

### TS2Vec versus T-Rep

| Property | TS2Vec | T-Rep |
|---|---|---|
| Output | timestamp vectors | timestamp vectors |
| Backbone | dilated CNN | dilated CNN |
| Main time signal | same/different timestamp contrast | contrast plus learned time geometry |
| Extra tasks | none | divergence prediction and local representation forecasting |
| Main strength | robust general context | smoother, explicit temporal structure |

T-Rep is especially motivated by forecasting, anomalies, and missing data.

## 7.6 Newer directions

Objective families:

- masked patch reconstruction;
- tokenized next-value forecasting;
- autoregressive patch decoding;
- universal masking across variables and frequencies;
- task-token approaches such as **UniTS**, which unify forecasting, classification, anomaly detection, and imputation;
- mixture-of-experts scaling, as in **Time-MoE**.

Open problems:

- contamination of evaluation datasets in large pretraining corpora;
- irregular sampling and heterogeneous frequencies;
- integration of metadata and future-known covariates;
- calibration for high-stakes use;
- deciding when specialized timestamp representations beat large foundation models.

---

# 8. Deep learning for vision and geospatial data

## 8.1 Remote sensing fundamentals

**Remote sensing** obtains information about the Earth or another object without direct contact, commonly using satellite or airborne sensors.

A **spectral signature** describes reflectance across wavelengths. Healthy vegetation:

- absorbs much blue and red light;
- reflects green;
- strongly reflects near-infrared.

> **Formula box — NDVI**
>
> $$
> NDVI=\frac{NIR-Red}{NIR+Red}.
> $$
>
> It becomes high when near-infrared reflectance is strong relative to red reflectance.

Three main resolutions:

- **spatial resolution:** ground area represented by one pixel;
- **temporal resolution:** revisit frequency;
- **spectral resolution:** number and width of wavelength bands.

Tradeoffs are common: higher spatial, temporal, and spectral resolution cannot always be maximized simultaneously.

Data types:

- RGB: three visible bands;
- multispectral: several discrete spectral bands;
- hyperspectral: many narrow, nearly continuous bands, forming a spectral hypercube.

## 8.2 Remote-sensing errors and preprocessing

Internal errors come from the sensor; external errors come from atmosphere or platform motion.

Examples:

- striping/banding from detector variation: destripe, e.g. histogram matching;
- line drop: replace from neighboring lines;
- salt-and-pepper bit errors: neighborhood-based outlier correction;
- Rayleigh scattering: stronger at short wavelengths;
- Mie scattering: aerosols, harder to predict;
- non-selective scattering: large particles.

Geometric correction:

- image-to-map registration with ground control points;
- image-to-image registration for aligned time series;
- resampling using nearest neighbor, bilinear interpolation, or cubic convolution.

Registration is crucial for change detection and satellite image time series: the same pixel should refer to the same ground location.

## 8.3 Vision tasks

- **classification:** one label for an image;
- **semantic segmentation:** one semantic class per pixel, without distinguishing instances;
- **object detection:** object class plus bounding box;
- **instance segmentation:** pixel mask for each object instance.

## 8.4 CNN concepts

A convolutional filter:

- spans all input channels;
- slides over spatial locations;
- computes a dot product plus bias;
- creates one activation map.

Multiple filters create multiple output channels.

Early filters often detect edges or color contrasts; deeper layers combine them into larger structures.

> **Formula box — convolution output and parameters**
>
> For one spatial dimension:
>
> $$
> W'=\left\lfloor\frac{W-K+2P}{S}\right\rfloor+1.
> $$
>
> For 2D input $C_{in}\times H\times W$ and
> $C_{out}$ filters of size $C_{in}\times K_H\times K_W$:
>
> $$
> \#\text{parameters}
> =C_{out}(C_{in}K_HK_W+1),
> $$
>
> assuming one bias per output channel.

Padding controls shrinking. For odd kernel $K$, $P=(K-1)/2$ with stride $1$ gives “same” spatial size.

The receptive field grows with depth. With stride-one kernels of size $K$, $L$ layers have receptive field $1+L(K-1)$. Downsampling grows the effective receptive field faster.

### Pooling and stride

- strided convolution: learned downsampling;
- max/average pooling: fixed downsampling with no learned parameters;
- max pooling gives some robustness to small shifts.

Convolution and pooling are **translation equivariant**: translating the input translates the feature map. Classification later often seeks translation invariance through pooling or aggregation.

1D, 2D, and 3D convolutions apply the same concept to sequences, images, and volumes.

## 8.5 Semantic segmentation and U-Net

A sliding-window classifier is inefficient because neighboring patches repeat most computation.

A **fully convolutional network (FCN)** processes the full image and outputs class scores for all pixels.

Problem:

- downsampling provides context and efficient computation;
- segmentation needs high-resolution output.

Upsampling choices:

- nearest-neighbor or “bed of nails” unpooling;
- max-unpooling using saved pooling indices;
- transposed convolution, a learnable upsampling operation.

**U-Net** contains:

- encoder/downsampling path: increases receptive field but loses spatial detail;
- decoder/upsampling path: restores resolution;
- skip connections: concatenate high-resolution encoder features with decoder features.

The skips combine semantic context from deep layers with precise localization from shallow layers.

## 8.6 Vision Transformer

ViT:

1. splits an image into patches;
2. flattens each patch;
3. linearly projects it to a token embedding;
4. adds learned positional embeddings;
5. prepends a learned classification token;
6. applies an ordinary Transformer encoder;
7. projects the classification-token output to class scores.

For a $224\times224$ image with $16\times16$ patches, there are $14\times14=196$ patch tokens before the class token.

Compared with CNNs, ViT has weaker built-in locality and translation bias but can model global patch relations directly.

## 8.7 Knowledge distillation

Train a teacher model on labeled images. Then train a smaller student to match:

- ground-truth labels with cross-entropy;
- teacher output distribution with KL divergence.

Benefits:

- transfers knowledge from a larger model;
- soft probabilities reveal similarities between classes;
- teacher predictions can supervise unlabeled data.

In data-efficient ViT training, a CNN teacher can supervise a ViT student, including through a distillation token.

## 8.8 Satellite image time series

Advantages of modeling several acquisitions over time:

- capture seasonal variability;
- monitor and predict change;
- fill data gaps;
- detect environmental change;
- reduce sensitivity to one noisy or cloudy acquisition.

## 8.9 MAE and SatMAE

A **Masked Autoencoder (MAE)**:

1. divides an image into patches;
2. masks a large subset;
3. sends only visible tokens through the encoder;
4. restores mask tokens for the decoder;
5. reconstructs the missing patches.

This produces self-supervised visual representations while keeping the expensive encoder focused on visible tokens.

**SatMAE** extends MAE to temporal and multispectral satellite data:

- patch embeddings include spatial position;
- positional encoding also represents temporal or spectral dimension;
- the encoder processes visible spatiotemporal/spectral tokens;
- the decoder reconstructs masked observations.

Masking strategies:

- **consistent masking:** the same spatial locations are masked across times or spectral groups;
- **independent masking:** different positions are masked independently.

Consistent masking prevents the model from copying the same location from another date/band and can force stronger semantic reasoning. Independent masking gives more cross-time or cross-band evidence for reconstruction. The correct choice depends on the representation desired.

## 8.10 Presto

Presto is a lightweight pretrained encoder for pixel-level remote-sensing time series using heterogeneous inputs.

Dynamic variables include:

- Sentinel-1 radar VV/VH;
- Sentinel-2 RGB, red-edge, near-infrared, and short-wave infrared groups;
- NDVI;
- precipitation and temperature;
- dynamic land-cover information.

Static variables include:

- location coordinates;
- elevation and slope.

Variables receive suitable encodings:

- linear projections for continuous values;
- embeddings for categorical channels;
- sinusoidal/time encodings for month;
- channel/group embeddings;
- coordinate and topographic encodings.

Pretraining masks mixed variables and reconstructs them with an autoencoder.

> **Formula box — Presto reconstruction objective**
>
> $$
> \mathcal L_{\text{total}}
> =\mathcal L_{\text{MSE}}
> +\lambda\frac{N_{\text{cat}}}{N_{\text{cont}}}
> \mathcal L_{\text{CE}}.
> $$
>
> MSE is used for continuous variables and cross-entropy for categorical variables. The count ratio balances the two types of masked targets.

The pretrained encoder can then be reused for downstream geospatial tasks.

GraphCast was named as a graph-based weather forecasting model, but the lecture deck did not develop its architecture beyond the title. Do not confuse it with the graph-filter material: its relevance here is that global weather fields can be modeled as structured spatial data.

The lecture explicitly marked the later vision-interpretability section as **not applicable to the exam**.

---

# 9. Trajectory clustering and segmentation

## 9.1 Clustering versus segmentation

- **Trajectory clustering:** compares objects or sub-trajectories by spatial movement pattern. If a location is visited twice, both visits may belong to the same spatial cluster.
- **Trajectory segmentation:** splits one time-ordered trajectory into stays and transitions. Returning to the same location later may produce a separate stay because temporal order matters.

TRACLUS is mainly trajectory clustering through sub-trajectories. SeqScan is trajectory segmentation.

## 9.2 DBSCAN

Parameters:

- $\varepsilon$: neighborhood radius;
- MinPts: minimum neighborhood size.

Point types:

- **core:** at least MinPts points, including itself, in its $\varepsilon$-neighborhood;
- **border:** not core but lies in a core point’s neighborhood;
- **noise:** neither core nor border.

Clusters are grown by connecting density-reachable core points and attaching border points.

Advantages:

- arbitrary cluster shapes;
- explicit noise handling;
- no need to specify the number of clusters.

Limitations:

- one global $\varepsilon$ struggles with varying density;
- distances become less informative in high dimensions;
- border assignment can be sensitive.

Compared with k-means:

| Property | k-means | DBSCAN |
|---|---|---|
| Shape | compact/globular | arbitrary density-connected |
| Number of clusters | required | inferred |
| Center | centroid | no centroid required |
| Noise | forced into a cluster | explicitly labeled |
| Density variation | not its main issue | difficult with one $\varepsilon$ |

## 9.3 OPTICS

OPTICS addresses the limitation of one global density scale.

- **core distance of $p$:** distance to its MinPts-th neighbor, if enough neighbors exist within the maximum radius; otherwise $\infty$.
- **reachability distance of $q$ from $p$:**

> **Formula box — OPTICS reachability**
>
> $$
> \operatorname{reach\_dist}(p,q)
> =\max(\operatorname{core\_dist}(p),d(p,q)).
> $$

OPTICS outputs:

- an ordering of points;
- their reachability distances.

In the reachability plot:

- low valleys indicate clusters;
- peaks indicate transitions or noise;
- deep narrow valleys are dense clusters;
- shallow wide valleys are sparser clusters.

OPTICS reveals clusters over multiple density levels, but extracting final clusters requires interpreting the ordering or choosing a threshold.

## 9.4 TRACLUS overview

Input: trajectories of varying length.

Output:

- clusters of similar line-segment partitions;
- representative trajectory for each cluster.

The **partition-and-group** framework:

1. partition every trajectory at characteristic points;
2. pool all resulting line segments;
3. cluster similar segments;
4. generate a representative path.

One original trajectory may contribute segments to several clusters.

## 9.5 TRACLUS line-segment distance

Compare segment $L_j$ with a reference, usually longer, segment $L_i$.

Three components:

- perpendicular separation;
- parallel displacement;
- angular difference.

> **Formula box — TRACLUS distance**
>
> If $l_{\perp1},l_{\perp2}$ are endpoint-to-projection distances:
>
> $$
> d_\perp(L_i,L_j)=
> \frac{l_{\perp1}^2+l_{\perp2}^2}
> {l_{\perp1}+l_{\perp2}}.
> $$
>
> If $l_{\parallel1},l_{\parallel2}$ measure longitudinal endpoint offsets:
>
> $$
> d_\parallel(L_i,L_j)=
> \min(l_{\parallel1},l_{\parallel2}).
> $$
>
> For smaller angle $\theta$:
>
> $$
> d_\theta(L_i,L_j)=
> \begin{cases}
> \|L_j\|\sin\theta,&0^\circ\le\theta<90^\circ,\\
> \|L_j\|,&90^\circ\le\theta\le180^\circ.
> \end{cases}
> $$
>
> Combined:
>
> $$
> \operatorname{dist}(L_i,L_j)=
> w_\perp d_\perp+
> w_\parallel d_\parallel+
> w_\theta d_\theta.
> $$

The weights depend on the application.

## 9.6 TRACLUS partitioning with MDL

The goal is to find characteristic points balancing:

- **preciseness:** preserve important movement changes;
- **conciseness:** use few segments.

Minimum Description Length expresses:

> **Formula box — MDL principle**
>
> $$
> MDL=L(H)+L(D\mid H).
> $$
>
> $L(H)$ is the cost of encoding the simplified trajectory.
> $L(D\mid H)$ is the cost of reconstruction error relative to it.

Approximate partitioning:

1. start from a characteristic point;
2. extend a candidate segment;
3. compare the cost with partitioning against the cost without partitioning;
4. when partitioning becomes preferable under the algorithm’s criterion, add the previous point as a characteristic point;
5. continue and always include the final point.

It is a greedy approximation and may miss the globally optimal partition.

## 9.7 TRACLUS segment clustering and representative paths

After partitioning:

- run a DBSCAN-like algorithm using the line-segment distance;
- define core segments through an $\varepsilon$-neighborhood;
- expand density-connected clusters;
- discard a cluster unless it contains segments from enough **distinct original trajectories**, not merely many segments from one trajectory.

Representative trajectory generation:

1. compute average cluster direction;
2. rotate axes so this direction is horizontal;
3. sweep through sorted segment endpoints;
4. where enough segments overlap, average their coordinates;
5. require a smoothing separation $\gamma$ between output points;
6. undo the rotation.

The output summarizes the common sub-trajectory rather than selecting one observed path.

## 9.8 SeqScan

SeqScan segments a single time-ordered trajectory:

$$
T=[(p_1,t_1),\ldots,(p_n,t_n)].
$$

Outputs:

- **stay regions:** spatially dense regions with enough residence time;
- **transitions:** points between stays;
- **noise:** temporary irregular points not assigned to either.

Important definitions:

- a sub-trajectory is a connected interval $[i,j]$;
- a segment can be a union of sub-trajectories and may contain temporal gaps;
- a cluster is a segment whose spatial projection forms a DBSCAN cluster;
- gaps inside the bounding interval are **local noise** $N(S)$;
- duration $D(S)$ is time from first to last bounded point;
- presence $P(S)$ excludes absence periods/local noise;
- a stay region satisfies $P(S)\ge\delta$;
- a **Minimal Stay Region (MSR)** is the earliest minimal-length part that already satisfies stay conditions.

Algorithm intuition:

1. process points chronologically;
2. maintain a **context $C$** for expanding the active stay;
3. maintain a **pool $P$** of points that may form the next stay;
4. use incremental DBSCAN;
5. if a new point fits the active stay, expand it and reset the pool as appropriate;
6. otherwise add it to the pool;
7. when the pool forms a new MSR with presence at least $\delta$, close the current stay and start the new one.

The method respects both spatial density and temporal separation, unlike ordinary spatial clustering.

---

# 10. Dynamic graphs and graph recurrent models

## 10.1 Two distinct temporal-graph settings

This distinction is essential:

1. **Dynamic graph sequence:** topology, node set, or edges may change over time. EvolveGCN addresses this.
2. **Graph process on a fixed graph:** topology stays fixed, but node signals change over time. GRNN/GGRNN addresses this.

Both combine graph and temporal structure, but their assumptions differ.

## 10.2 Why a static GCN is insufficient

A static GCN assumes one graph snapshot. Real networks evolve:

- transactions appear;
- friendships form and dissolve;
- nodes enter and leave;
- node roles change.

An obvious architecture is:

$$
\text{graph}_t\rightarrow\text{GCN}\rightarrow
\text{node embeddings}_t\rightarrow\text{RNN}.
$$

This requires node identity and an embedding history across time. It fails when:

- new nodes appear;
- nodes disappear irregularly;
- node sets at different times are partly or fully disjoint.

## 10.3 EvolveGCN: evolve weights, not node embeddings

EvolveGCN stores temporal information in GCN weight matrices:

> **Formula box — EvolveGCN principle**
>
> $$
> W_t=\operatorname{RNN}(W_{t-1}),
> $$
>
> $$
> H_t^{(\ell+1)}
> =\sigma\left(
> \widehat A_tH_t^{(\ell)}W_t^{(\ell)}
> \right).
> $$

Node embeddings are freshly computed from the current graph using the evolved model. Therefore a new node needs current features and connectivity, not a historical hidden state.

Consequences:

- temporal memory lives in model parameters;
- model size does not grow with the number of time steps;
- the GCN weights are outputs of recurrent units rather than independently trained parameters;
- there is a separate recurrent weight evolution per GCN layer.

## 10.4 EvolveGCN-H

In EvolveGCN-H:

- current GCN weight matrix is the GRU hidden state;
- current node embeddings provide the GRU input.

$$
W_t^{(\ell)}
=\operatorname{GRU}
\left(\operatorname{summarize}(H_t^{(\ell)}),
W_{t-1}^{(\ell)}\right).
$$

Why summarize:

- number of nodes varies;
- GRU input must have a fixed shape matching the weight matrix.

The learned summarizer:

1. scores node rows with a learned vector;
2. selects top-$k$ representative rows;
3. scales selected rows with a learned score;
4. transposes them to the required matrix shape.

Use EvolveGCN-H when node features are informative, because weight evolution is directly conditioned on current node states.

## 10.5 EvolveGCN-O

In EvolveGCN-O:

$$
W_t^{(\ell)}
=\operatorname{LSTM}(W_{t-1}^{(\ell)}).
$$

Node embeddings are not inputs to the recurrent unit. The LSTM cell state carries internal temporal memory, while weight matrices are recurrent input/output.

Use it when structural evolution is more informative than node features.

## 10.6 Evolving Graph Convolution Unit

Each EGCU performs:

1. recurrent update of $W_t^{(\ell)}$;
2. graph convolution with the updated weights.

Stacking:

- vertically: GCN layers at one time;
- horizontally: recurrent evolution through time.

EvolveGCN was evaluated on:

- link prediction;
- edge classification;
- node classification.

Important caveats:

- graph autoencoder baselines can remain competitive for link prediction;
- unseen structural regime changes degrade all methods;
- removing the node-history requirement does not eliminate distribution shift.

## 10.7 Graph Recurrent Neural Networks

For a fixed graph with evolving signal $x_t$, a standard RNN ignores topology:

$$
z_t=\sigma(Ax_t+Bz_{t-1}).
$$

A GRNN replaces dense maps with graph filters:

> **Formula box — GRNN**
>
> $$
> z_t=\sigma\left(A(S)x_t+B(S)z_{t-1}\right),
> $$
>
> $$
> A(S)=\sum_{k=0}^{K-1}a_kS^k,
> \qquad
> B(S)=\sum_{k=0}^{K-1}b_kS^k.
> $$

This models:

- spatial relations through graph filtering;
- temporal relations through recurrence.

Advantages:

- local distributed computation;
- $K$-hop spatial receptive field;
- parameter count independent of graph size for scalar graph filters;
- parameter sharing;
- permutation equivariance;
- stability to small graph perturbations under suitable assumptions.

The stability bound worsens with sequence length, reflecting repeated recurrence and filtering.

## 10.8 Why gate a GRNN

Problems:

- vanishing/exploding gradients in time;
- oversmoothing and oversquashing;
- spatial imbalance where highly connected regions dominate;
- irrelevant nodes or edges transmit noise.

A generic gated GRNN uses:

$$
Z_t=\sigma\left(
\widehat Q\{A_S(X_t)\}+
\check Q\{B_S(Z_{t-1})\}
\right),
$$

where $\widehat Q$ is an input-gate operator and $\check Q$ a forget-gate operator.

The gate parameters are themselves generated by auxiliary GRNN states.

## 10.9 Time, node, and edge gating

### Time gating

One scalar input gate and one scalar forget gate per time step:

$$
\widehat Q\{A_S(X_t)\}=\hat q_tA_S(X_t),
$$

$$
\check Q\{B_S(Z_{t-1})\}=\check q_tB_S(Z_{t-1}).
$$

Properties:

- same decision for all nodes;
- controls whole input/state flow at a time;
- analogous to LSTM-style temporal memory;
- useful for long temporal dependencies;
- scalar-generation parameter count can depend on graph size because all node states are reduced to a scalar.

### Node gating

One gate per node:

$$
\widehat Q\{A_S(X_t)\}
=\operatorname{diag}(\hat q_t)A_S(X_t),
$$

$$
\check Q\{B_S(Z_{t-1})\}
=\operatorname{diag}(\check q_t)B_S(Z_{t-1}).
$$

Properties:

- graph regions can retain or ignore information differently;
- controls spatially heterogeneous dynamics;
- suppresses noisy nodes after local exchange;
- can be interpreted as a node-varying graph filter;
- parameter count remains independent of graph size when gates are generated convolutionally.

### Edge gating

One gate per edge modifies information exchange itself:

- close noisy or irrelevant edges;
- adapt message flow;
- behave similarly to graph attention;
- can be interpreted as an edge-varying graph filter.

Time gating asks **when the entire graph should remember**.  
Node gating asks **which nodes should remember**.  
Edge gating asks **which connections should transmit information**.

## 10.10 EvolveGCN versus GRNN/GGRNN

| Property | EvolveGCN | GRNN/GGRNN |
|---|---|---|
| Topology | may change with time | fixed |
| Node set | may change | fixed support assumed |
| Temporal state | GCN weights | graph-valued hidden state |
| Main goal | dynamic graph snapshots | temporal graph signals |
| Spatial operation | GCN per snapshot | graph filters inside recurrence |
| Gating | GRU/LSTM evolves weights | gates input/state by time, node, or edge |

---

# 11. Final comparison review

## 11.1 High-value conceptual comparisons

### Supervised, self-supervised, and transfer learning

- supervised: real target labels train the model;
- self-supervised: targets/relationships are generated from the data;
- transfer: reuse a model learned elsewhere;
- fine-tuning: continue training reused layers.

### Contrastive versus masked learning

- contrastive: decide what should be similar and different;
- masked: infer hidden content from context;
- contrastive methods depend critically on valid invariances;
- reconstruction can preserve low-level details that are not useful downstream;
- combinations are common.

### RNN versus Transformer versus SSM

- RNN: fixed-size recurrent memory, sequential training;
- Transformer: direct retrieval from visible context, quadratic full attention;
- SSM/Mamba: compact recurrent state with efficient parallel scan;
- hybrid: state for efficient long memory, attention for direct event lookup.

### CNN versus ViT

- CNN: strong locality, shared spatial filters, translation equivariance;
- ViT: patch tokens and global self-attention, weaker built-in visual bias;
- CNNs often work well with less data;
- ViTs benefit strongly from pretraining and distillation.

### Matrix factorization versus LightGCN versus SASRec

- MF: static user/item latent compatibility;
- LightGCN: propagates collaborative information through the bipartite graph;
- SASRec: models ordered interaction history and recency/context;
- the correct representation depends on whether matrix, graph, or order structure matters.

### Euclidean distance versus DTW

- Euclidean: same index must correspond;
- DTW: learns monotonic local alignment;
- DTW handles shifts/speed variation but is slower and may over-warp;
- k-means’ arithmetic center is tied to squared Euclidean geometry, motivating DBA under DTW.

### DBSCAN versus OPTICS

- DBSCAN gives clusters directly for one density scale;
- OPTICS gives an ordering and reachability structure over multiple scales;
- OPTICS handles varying density better but needs interpretation.

### TRACLUS versus SeqScan

- TRACLUS: find common sub-trajectories across objects;
- SeqScan: segment one ordered trajectory into stays and transitions;
- TRACLUS partitions and clusters line segments;
- SeqScan combines spatial density with residence time and temporal separation.

### Static GCN versus LightGCN versus EvolveGCN versus GRNN

- static GCN: one graph and node-feature transformation;
- LightGCN: simplified propagation on a user–item graph;
- EvolveGCN: graph snapshots may change; evolve GCN weights;
- GRNN: fixed topology, signals change; put graph filters inside recurrence.

## 11.2 Questions you should be able to answer

1. Why can a low pretext-task loss still produce poor representations?
2. Why does RotNet learn more than a four-class label in favorable cases?
3. What do query, key, and value mean?
4. Why is attention divided by $\sqrt{d_k}$?
5. Why does a Transformer need positional information?
6. What is the difference between self-attention and cross-attention?
7. Why are self-loops and degree normalization used in GCN?
8. What do powers of a graph shift operator represent?
9. What are oversmoothing and oversquashing?
10. Why is ranking loss often preferable to rating MSE for implicit recommendation?
11. How does BPR construct a training example?
12. How does LightFM help cold start?
13. Why does LightGCN remove nonlinearities and feature transforms?
14. How does SASRec prevent future leakage?
15. What assumptions make a process stationary?
16. Why is a random split dangerous for time series?
17. What roles do $p,d,q$ play in ARIMA?
18. Why does ordinary k-means not naturally work with DTW?
19. How does a shapelet represent a complete series?
20. Why does teacher forcing create an inference mismatch?
21. How do LSTM’s additive cell update and forget gate help gradient flow?
22. What is the conceptual difference between LSTM and GRU?
23. Why can Mamba scale better than full attention on long dense streams?
24. Why is time-series augmentation more delicate than image augmentation?
25. What exactly is a positive pair in TS2Vec?
26. Why is TS2Vec hierarchical?
27. What does T-Rep add to TS2Vec?
28. What are spatial, temporal, and spectral resolution?
29. Why are image registration and atmospheric correction important?
30. How do U-Net skip connections solve a segmentation problem?
31. What is the difference between consistent and independent SatMAE masking?
32. How does Presto combine continuous and categorical remote-sensing variables?
33. Why does DBSCAN fail with strongly varying densities?
34. What information do the three TRACLUS distance components measure?
35. What tradeoff does MDL express in trajectory partitioning?
36. Why does EvolveGCN handle new nodes better than an RNN over node embeddings?
37. When should EvolveGCN-H be preferred over EvolveGCN-O?
38. What assumption distinguishes GRNN from EvolveGCN?
39. What is controlled by time, node, and edge gates?
40. Across the entire course, where do masking, parameter sharing, and normalization appear, and why?

## 11.3 Recurring principles across the course

### Structure should match the data

- CNNs use image grids;
- GNNs use graph neighborhoods;
- RNNs use temporal recurrence;
- Transformers use content-based retrieval;
- DTW uses flexible temporal alignment;
- trajectory models combine space and order.

### Parameter sharing creates scalability

- convolution shares filters across locations;
- RNNs share updates across time;
- graph filters share coefficients across nodes;
- matrix factorization shares latent dimensions across interactions.

### Normalization controls scale

- attention uses $1/\sqrt{d_k}$;
- GCN uses degree normalization;
- embedding normalization supports contrastive losses;
- time-series scaling must be learned from training data only.

### Masking controls information availability

- causal masks prevent future leakage;
- SSL masks create reconstruction/context tasks;
- SatMAE masks space, time, or spectrum;
- masking assumptions determine what the model must infer.

### Evaluation must match deployment

- chronological splits for forecasting;
- leave-last-out for sequential recommendation;
- ranking metrics for top-$K$ systems;
- probabilistic metrics when uncertainty matters;
- simple downstream heads when evaluating representations.

### More flexible models introduce new failure modes

- attention is expensive;
- deep GNNs oversmooth/oversquash;
- recursive forecasts accumulate errors;
- contrastive learning can encode false invariances;
- large pretrained models can leak benchmark data;
- trajectory approximations can miss global optima.

---

# Coverage note

These notes cover the theoretical content of Lectures 1–14, including the Lecture 08 time-series presentations and notebooks. Lecture 02 and Lecture 03 contained the same Transformer presentation and were merged. The project-information PDF was excluded because it contains project logistics rather than exam theory. The vision lecture explicitly marked its model-interpretation section as outside the exam, so that section is not included.
