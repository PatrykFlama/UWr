## Slajdy Justin Johnson
- [598_WI2022_lecture15.pdf](https://skos.ii.uni.wroc.pl/pluginfile.php/77148/mod_resource/content/2/598_WI2022_lecture15.pdf)

## Slajdy cs224n
- [https://web.stanford.edu/class/cs224n/slides/](https://web.stanford.edu/class/cs224n/slides/)

## FCN
- [Fully Convolutional Networks for Semantic Segmentation](https://arxiv.org/abs/1411.4038)
## UNet
- [U-Net: Convolutional Networks for Biomedical Image Segmentation](https://arxiv.org/abs/1505.04597)


## Extra resources
- [U-Nets for dummies in Pytorch & Tensorflow](https://medium.com/@chewryan0/u-nets-for-dummies-pytorch-tensorflow-dddcdb8a2759)
- [The Importance of Skip Connections in Biomedical Image Segmentation](https://arxiv.org/pdf/1608.04117)


## Loss Function in Negative Sampling
Given:

- A **center word** w_c
A **true context word** w_o
k **negative samples**: words not in the context (denoted as w_1, ..., w_k)
Word vectors:
- v_c = embedding of the center word (from input matrix)
- u_o = embedding of the context word (from output matrix)
- u_i = embeddings of negative samples

The Negative Sampling Loss:  
![img](./images/negative_sampling_loss.jpg)

