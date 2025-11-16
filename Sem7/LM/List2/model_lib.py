import torch
from transformers import AutoTokenizer, AutoModelForCausalLM
from torch.nn import functional as F

class ModelUtils:
    def __init__(self, model_name='flax-community/papuGaPT2'):
        # model_name = 'eryk-mazus/polka-1.1b-chat'
        self.model_name = model_name
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
    
