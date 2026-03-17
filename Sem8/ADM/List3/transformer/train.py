import argparse
import random

import torch
import torch.nn as nn
from torch.utils.data import DataLoader, Dataset

from model import Transformer, build_transformer


PAD_ID = 0
SOS_ID = 1
EOS_ID = 2


def causal_mask(size: int, device: torch.device | None = None) -> torch.Tensor:
    mask = torch.triu(torch.ones(1, 1, size, size, device=device), diagonal=1).type(torch.int)
    return mask == 0


class CopyDataset(Dataset):
    def __init__(self, sample_count: int, seq_len: int, vocab_size: int, seed: int = 42):
        self.sample_count = sample_count
        self.seq_len = seq_len
        self.vocab_size = vocab_size
        self.random = random.Random(seed)

    def __len__(self) -> int:
        return self.sample_count

    def __getitem__(self, index: int) -> dict[str, torch.Tensor]:
        del index
        content_len = self.random.randint(3, self.seq_len - 1)
        src_tokens = [self.random.randint(3, self.vocab_size - 1) for _ in range(content_len - 1)] + [EOS_ID]
        src_tokens = src_tokens + [PAD_ID] * (self.seq_len - len(src_tokens))

        tgt_tokens = src_tokens.copy()
        decoder_input = [SOS_ID] + tgt_tokens[:-1]

        return {
            "src": torch.tensor(src_tokens, dtype=torch.long),
            "tgt_in": torch.tensor(decoder_input, dtype=torch.long),
            "label": torch.tensor(tgt_tokens, dtype=torch.long),
        }


def make_src_mask(src: torch.Tensor) -> torch.Tensor:
    return (src != PAD_ID).unsqueeze(1).unsqueeze(2)


def make_tgt_mask(tgt_in: torch.Tensor) -> torch.Tensor:
    padding_mask = (tgt_in != PAD_ID).unsqueeze(1).unsqueeze(2)
    seq_mask = causal_mask(tgt_in.size(1), device=tgt_in.device)
    return padding_mask & seq_mask


def train_one_epoch(
    model: Transformer,
    loader: DataLoader,
    optimizer: torch.optim.Optimizer,
    criterion: nn.Module,
    device: torch.device,
) -> float:
    model.train()
    total_loss = 0.0

    for batch in loader:
        src = batch["src"].to(device)
        tgt_in = batch["tgt_in"].to(device)
        label = batch["label"].to(device)

        src_mask = make_src_mask(src)
        tgt_mask = make_tgt_mask(tgt_in)

        encoder_output = model.encode(src, src_mask)
        decoder_output = model.decode(encoder_output, src_mask, tgt_in, tgt_mask)
        logits = model.project(decoder_output)

        loss = criterion(logits.view(-1, logits.size(-1)), label.view(-1))

        optimizer.zero_grad(set_to_none=True)
        loss.backward()
        optimizer.step()

        total_loss += loss.item()

    return total_loss / max(1, len(loader))


@torch.no_grad()
def evaluate_loss(model: Transformer, loader: DataLoader, criterion: nn.Module, device: torch.device) -> float:
    model.eval()
    total_loss = 0.0

    for batch in loader:
        src = batch["src"].to(device)
        tgt_in = batch["tgt_in"].to(device)
        label = batch["label"].to(device)

        src_mask = make_src_mask(src)
        tgt_mask = make_tgt_mask(tgt_in)

        encoder_output = model.encode(src, src_mask)
        decoder_output = model.decode(encoder_output, src_mask, tgt_in, tgt_mask)
        logits = model.project(decoder_output)
        loss = criterion(logits.view(-1, logits.size(-1)), label.view(-1))
        total_loss += loss.item()

    return total_loss / max(1, len(loader))


@torch.no_grad()
def greedy_decode(model: Transformer, src: torch.Tensor, max_len: int, device: torch.device) -> torch.Tensor:
    model.eval()
    src = src.unsqueeze(0).to(device)
    src_mask = make_src_mask(src)
    encoder_output = model.encode(src, src_mask)

    decoded = torch.tensor([[SOS_ID]], dtype=torch.long, device=device)
    for _ in range(max_len - 1):
        tgt_mask = make_tgt_mask(decoded)
        out = model.decode(encoder_output, src_mask, decoded, tgt_mask)
        next_token = model.project(out[:, -1]).argmax(dim=-1, keepdim=True)
        decoded = torch.cat([decoded, next_token], dim=1)
        if next_token.item() == EOS_ID:
            break

    return decoded.squeeze(0)


def run_training(epochs: int = 5, batch_size: int = 64, learning_rate: float = 1e-4) -> None:
    torch.manual_seed(42)
    random.seed(42)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    vocab_size = 64
    seq_len = 16

    train_dataset = CopyDataset(sample_count=2048, seq_len=seq_len, vocab_size=vocab_size, seed=42)
    valid_dataset = CopyDataset(sample_count=256, seq_len=seq_len, vocab_size=vocab_size, seed=7)

    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    valid_loader = DataLoader(valid_dataset, batch_size=batch_size, shuffle=False)

    model = build_transformer(
        src_vocab_size=vocab_size,
        tgt_vocab_size=vocab_size,
        src_seq=seq_len,
        tgt_seq=seq_len,
        d_model=128,
        N=2,
        h=4,
        dropout=0.1,
        d_ff=256,
    ).to(device)

    optimizer = torch.optim.Adam(model.parameters(), lr=learning_rate)
    criterion = nn.CrossEntropyLoss(ignore_index=PAD_ID)

    print(f"Device: {device}")
    for epoch in range(1, epochs + 1):
        train_loss = train_one_epoch(model, train_loader, optimizer, criterion, device)
        valid_loss = evaluate_loss(model, valid_loader, criterion, device)
        print(f"Epoch {epoch:02d} | train_loss={train_loss:.4f} | valid_loss={valid_loss:.4f}")

    sample = valid_dataset[0]["src"]
    pred = greedy_decode(model, sample, max_len=seq_len, device=device)
    print("Sample source:   ", sample.tolist())
    print("Sample decoded:  ", pred.tolist())


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Train scratch Transformer on a copy task")
    parser.add_argument("--epochs", type=int, default=5)
    parser.add_argument("--batch-size", type=int, default=64)
    parser.add_argument("--lr", type=float, default=1e-4)
    return parser.parse_args()


if __name__ == "__main__":
    args = parse_args()
    run_training(epochs=args.epochs, batch_size=args.batch_size, learning_rate=args.lr)
