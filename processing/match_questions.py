import os
import pandas as pd
import numpy as np
import pyreadr
from dotenv import load_dotenv
from tqdm import tqdm
from openai import AzureOpenAI
from pathlib import Path
import requests

import time


# Establish project root (like here::here() in R)
PROJECT_ROOT = Path("./processing").resolve().parent  # adjust as needed
embeddings_dir = PROJECT_ROOT / "data" / "generated" / "embeddings"


# Load environment variables
load_dotenv()
AZURE_OPENAI_ENDPOINT = os.getenv('AZURE_ENDPOINT_GPT')
AZURE_OPENAI_API_KEY = os.getenv('AZURE_API_KEY_GPT')

# Initialize AzureOpenAI client (do this once, not per call)
client = AzureOpenAI(
    api_key=os.getenv("AZURE_API_KEY_GPT"),
    api_version="2024-10-21",
    azure_endpoint=os.getenv("AZURE_ENDPOINT_GPT")
)

def get_embedding(text):
    response = client.embeddings.create(
        input=text,
        model="text-embedding-3-large"
    )
    # The embedding is in response.data[0].embedding
    return response.data[0].embedding

def get_embedding_with_retry(text, retries=5, delay=2):
    for attempt in range(retries):
        try:
            return get_embedding(text)
        except requests.exceptions.HTTPError as e:
            if e.response.status_code == 429:
                print("Rate limited. Waiting before retrying...")
                time.sleep(delay)
            else:
                raise
    raise Exception("Failed after retries")

# Helper: Cosine similarity

def cosine_similarity(a, b):
    a = np.array(a)
    b = np.array(b)
    return np.dot(a, b) / (np.linalg.norm(a) * np.linalg.norm(b))


rds_path = PROJECT_ROOT / "data" / "generated" / "posttest.rds"
csv_path = PROJECT_ROOT / "data" / "generated" / "post_tests_dataset.csv"

rds_result = pyreadr.read_r(rds_path)
rds_df = list(rds_result.values())[0]
csv_df = pd.read_csv(csv_path)

# 2. Reshape RDS to questions_universe
q_cols = [f"q{i}_text" for i in range(1, 11)]

# All questions we need to find answers for
questions_universe = rds_df.melt(
    id_vars=["airtable_class_id"],
    value_vars=q_cols,
    var_name="question_number",
    value_name="question_text"
)
questions_universe["question_number"] = questions_universe["question_number"].str.extract(r"q(\d+)_text").astype(int)
questions_universe = questions_universe.drop_duplicates()
questions_universe = questions_universe.dropna(subset=["question_text"]).reset_index(drop=True)

# Deduplicate csv_df question_texts within each class_id
possible_questions = (
    csv_df
    .dropna(subset=["question_text"])
    .drop_duplicates(subset=["class_id", "question_text"])
    .reset_index(drop=True)
)

# Check which airtable_class_ids are missing from csv_df["class_id"]
class_with_post_test = set(questions_universe["airtable_class_id"].unique())
all_classes = set(csv_df["class_id"].unique())

assert class_with_post_test.issubset(all_classes), \
    "Some airtable_class_ids in questions_universe are not present in csv_df['class_id']"

# 3. Get embeddings for both dataframes, and save to disk so we don't have to regenerate

questions_universe_emb_path = embeddings_dir / "questions_universe_with_embeddings.parquet"
possible_questions_emb_path = embeddings_dir / "possible_questions_with_embeddings.parquet"

# Try to load embeddings if they exist, otherwise compute and save
if os.path.exists(questions_universe_emb_path):
    print("Loading questions_universe with embeddings from disk...")
    questions_universe = pd.read_parquet(questions_universe_emb_path)
else:
    print("Getting embeddings for questions_universe...")
    questions_universe["embedding"] = [get_embedding_with_retry(q) for q in tqdm(questions_universe["question_text"])]
    questions_universe.to_parquet(questions_universe_emb_path, index=False)
    print(f"Saved questions_universe with embeddings to {questions_universe_emb_path}")

if os.path.exists(possible_questions_emb_path):
    print("Loading possible_questions with embeddings from disk...")
    possible_questions = pd.read_parquet(possible_questions_emb_path)
else:
    print("Getting embeddings for possible_questions...")
    possible_questions["embedding"] = [get_embedding_with_retry(q) for q in tqdm(possible_questions["question_text"])]
    possible_questions.to_parquet(possible_questions_emb_path, index=False)
    print(f"Saved possible_questions with embeddings to {possible_questions_emb_path}")

# 4. For each row in questions_universe, find best match in csv_df with same class_id
results = []
for idx, row in tqdm(questions_universe.iterrows(), total=len(questions_universe)):
    class_id = row["airtable_class_id"]
    q_emb = row["embedding"]
    candidates = csv_df[csv_df["class_id"] == class_id]
    if candidates.empty:
        continue
    sims = candidates["embedding"].apply(lambda emb: cosine_similarity(q_emb, emb))
    best_idx = sims.idxmax()
    best_row = candidates.loc[best_idx]
    results.append({
        "class_id": class_id,
        "question_text": row["question_text"],
        "options": best_row.get("options", None),
        "answer": best_row.get("answer", None)
    })

# 5. Save results
out_df = pd.DataFrame(results)
out_path = os.path.expanduser("~/Github/pingpong_analysis/data/generated/matched_questions.csv")
out_df.to_csv(out_path, index=False)
print(f"Saved to {out_path}") 