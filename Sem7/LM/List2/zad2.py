import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from torch.nn import functional as F
import re
import random
from tqdm.auto import tqdm
import equation

# === MAIN CLASS ===
class ModelGeneratorSetOfWords:
    def __init__(self, model_name='flax-community/papuGaPT2'):
        # === Model ===
        self.device = torch.device('cuda' if torch.cuda.is_available() else 'cpu')
        self.tokenizer = AutoTokenizer.from_pretrained(model_name)
        # ensure a pad token is set (avoid the warnings) and propagate to model config
        if self.tokenizer.pad_token is None:
            self.tokenizer.pad_token = self.tokenizer.eos_token
        self.model = AutoModelForCausalLM.from_pretrained(model_name)
        self.model.config.pad_token_id = self.tokenizer.pad_token_id
        self.model.to(self.device)  # type: ignore
        print("Model loaded on", self.device)

    # === helpers ===
    def log_probs_from_logits(self, logits, labels):
        logp = F.log_softmax(logits, dim=-1)
        logp_label = torch.gather(logp, 2, labels.unsqueeze(2)).squeeze(-1)
        return logp_label

    def sentence_prob(self, sentence_txt):
        inputs = self.tokenizer(sentence_txt, return_tensors='pt', return_attention_mask=True)
        input_ids = inputs['input_ids'].to(self.device)
        attention_mask = inputs['attention_mask'].to(self.device)
        with torch.no_grad():
            output = self.model(input_ids=input_ids, attention_mask=attention_mask)
            log_probs = self.log_probs_from_logits(output.logits[:, :-1, :], input_ids[:, 1:])
            seq_log_probs = torch.sum(log_probs)
        return seq_log_probs.cpu().numpy()  

    def ask_model(self, prompt, max_new_tokens=50, temperature=0.7):
        inputs = self.tokenizer(prompt, return_tensors='pt', return_attention_mask=True)
        input_ids = inputs['input_ids'].to(self.device)
        attention_mask = inputs['attention_mask'].to(self.device)
        with torch.no_grad():
            output_ids = self.model.generate(
                input_ids,
                attention_mask=attention_mask,
                max_new_tokens=max_new_tokens,
                temperature=temperature,
                do_sample=True,
                pad_token_id=self.tokenizer.pad_token_id
            )
        output_text = self.tokenizer.decode(output_ids[0], skip_special_tokens=True)
        return output_text


    # ===== MAIN =====
    def _tokenize_variants(self, word):
        ids1 = self.tokenizer(word, add_special_tokens=False)['input_ids']
        ids2 = self.tokenizer(' ' + word, add_special_tokens=False)['input_ids']
        return {tuple(ids1), tuple(ids2)}

    def generate_one_word_from_set(self, prompt, allowed_words, max_new_tokens=30, temperature=1.0):
        """
        Generates next tokens until finds one matching with allowed_words
        """
        device_local = self.device

        tok_cands = set()
        for w in allowed_words:
            tok_cands |= self._tokenize_variants(w)
        tok_cands = [list(t) for t in tok_cands]

        # prep prompt
        inputs = self.tokenizer(prompt, return_tensors='pt')
        input_ids = inputs['input_ids'].to(device_local)
        attention_mask = inputs.get('attention_mask', None)
        if attention_mask is not None:
            attention_mask = attention_mask.to(device_local)

        gen = []    # generated tokens
        with torch.no_grad():
            out = self.model(input_ids=input_ids, attention_mask=attention_mask, use_cache=True)
            past = out.past_key_values
        last_token = input_ids[:, -1:].to(device_local)

        for _ in range(max_new_tokens):
            allowed_next = set()
            completions = []

            # iterate over tokenized words
            for tok in tok_cands:
                # if we can use next token value from this word
                if len(gen) <= len(tok) - 1 and tuple(gen) == tuple(tok[:len(gen)]):
                    allowed_next.add(tok[len(gen)])

                # if we generated this tokenized word
                if len(gen) == len(tok) and tuple(gen) == tuple(tok):
                    completions.append(tok)

            # if any of completions reached, return it
            if completions:
                return self.tokenizer.decode(completions[0], skip_special_tokens=True).strip()

            # if there are no fitting tokens to generate next (shouldn't happen)
            if not allowed_next:
                return None

            # calculate the tokens probability
            with torch.no_grad():
                out = self.model(input_ids=last_token, past_key_values=past, use_cache=True)
                logits = out.logits[:, -1, :]  # (1, vocab)
                past = out.past_key_values

            mask = torch.full_like(logits, -1e9)
            allowed_idxs = sorted(list(allowed_next))
            temp = (temperature if temperature > 0 else 1.0)
            # assign per-index to avoid advanced-indexing edge cases
            for idx in allowed_idxs:
                mask[0, idx] = logits[0, idx] / temp
            probs = torch.softmax(mask, dim=-1)

            next_token = torch.multinomial(probs, num_samples=1)  # shape (1,1)
            next_id = next_token.item()
            gen.append(next_id)
            last_token = next_token  # already shape (1,1)

        return None

    def generate_n_from_set(self, prompt, allowed_words, n=10, max_new_tokens=30, temperature=1.0):
        results = []
        for _ in range(n):
            w = self.generate_one_word_from_set(prompt, allowed_words, max_new_tokens=max_new_tokens, temperature=temperature)
            results.append(w)
        return results



def evaluate_riddles(riddles, allowed_answers, model, model_name_desc="current model",
                    trials=5, max_new_tokens=30, temperature=1.0):
    """
    Simple evaluator:
    - riddles: iterable of tuples (riddle_text, correct_answer)
    - allowed_answers: set/list wszystkich możliwych odpowiedzi (słowa)
    Returns accuracy: for each riddle we execute `trials` attempts and consider it a success if any attempt
    generates the exact correct answer.
    """
    correct = 0
    total = 0
    for riddle, gold in tqdm(riddles, desc="Evaluating riddles"):
        prompt = riddle.strip() + "\nAnswer:"
        samples = model.generate_n_from_set(prompt, allowed_answers, n=trials,
                                    max_new_tokens=max_new_tokens, temperature=temperature)
        ok = any((s is not None and s.strip().lower() == gold.strip().lower()) for s in samples)
        correct += 1 if ok else 0
        total += 1
    acc = correct / total if total > 0 else 0.0
    print(f"{model_name_desc}: {correct}/{total} = {acc:.3f}")
    return acc

if __name__ == "__main__":
    model_name = 'flax-community/papuGaPT2'
    # model_name = 'eryk-mazus/polka-1.1b-chat'
    generator = ModelGeneratorSetOfWords(model_name=model_name)

    sample_allowed = {"cat", "dog", "fish", "tiger", "lion", "elephant", "wolf", "rabbit", "hamster", "parrot"}
    sample_riddles = [
        ("Small domesticated feline animal.", "cat"),
        ("Man's best friend.", "dog"),
        ("A large wild cat known as the king of the jungle.", "lion"),
        ("A common household pet that swims in a tank.", "fish"),
        ("A large mammal with a trunk.", "elephant"),
        ("A carnivorous mammal known for its strength and courage.", "tiger"),
        ("A small, burrowing rodent often kept as a pet.", "hamster"),
        ("A colorful bird known for its ability to mimic sounds.", "parrot"),
    ]
    acc = evaluate_riddles(sample_riddles, sample_allowed, model=generator, model_name_desc=model_name, trials=10,
                         max_new_tokens=20, temperature=0.8)
    
    print("Final accuracy: ", acc)

