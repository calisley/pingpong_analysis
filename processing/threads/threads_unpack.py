import os
import json
import pandas as pd
import ast
from pathlib import Path
import re

def process_json_file(file_path, expected_keys=None):
    with open(file_path, 'r', encoding='utf-8') as f:
        file_content = f.read()

    # Split on closing brace followed by opening brace (no commas, no line breaks)
    json_blocks = split_json_blocks(file_content)

    rows = []
    for block in json_blocks:
        try:
            data = json.loads(block)
            custom_id = data.get("custom_id", None)
            content_str = data["response"]["body"]["choices"][0]["message"]["content"]
        except (KeyError, IndexError, TypeError, json.JSONDecodeError) as e:
            print(f"Skipping entry due to parse error: {e}")
            continue

        try:
            # Try strict JSON first
            parsed_content = json.loads(content_str)
            if expected_keys:
                for key in expected_keys:
                    parsed_content.setdefault(key, None)
        except json.JSONDecodeError:
            try:
                # Fallback to Python-style dict parsing
                loose_dict = ast.literal_eval(content_str)
                if isinstance(loose_dict, dict):
                    parsed_content = {key: loose_dict.get(key, None) for key in expected_keys}
                else:
                    raise ValueError("Parsed content is not a dict")
            except Exception as e:
                print(f"Could not parse content even with ast.literal_eval: {e}")
                parsed_content = {key: None for key in expected_keys}
                parsed_content["raw_content"] = content_str

        parsed_content["custom_id"] = custom_id
        rows.append(parsed_content)

    return pd.DataFrame(rows)


def split_json_blocks(raw_str):
    """
    Splits a raw string of multiple JSON objects stuck together.
    Assumes no commas, no brackets—just multiple `{...}{...}` blobs.
    """
    parts = re.split(r'(?<=\})\s*(?=\{)', raw_str.strip())
    return parts


def process_batch_outputs(directory_path):
    json_files = [
        str(p) for p in Path(directory_path).glob("*.jsonl")
        if p.is_file()
    ]

    print(f"Found {len(json_files)} JSON files")

    expected_keys = [
        "category", "correctness", "appropriateness", "avoid",
        "beyond_course", "gave_answer", "disobeyed_prompt", "tone",
        "complexity", "multiple_questions", "potentially_harmful",
        "new_conversation", "user_category", "misuse_homework"
    ]

    all_dfs = []
    for i, file_path in enumerate(json_files, 1):
        print(f"Processing file {i} of {len(json_files)}: {os.path.basename(file_path)}")
        df = process_json_file(file_path, expected_keys)
        if not df.empty:
            all_dfs.append(df)

    if all_dfs:
        final_df = pd.concat(all_dfs, ignore_index=True)
        print(f"Final dataframe has {final_df.shape[0]} rows and {final_df.shape[1]} columns")
        return final_df
    else:
        print("No valid data found")
        return pd.DataFrame()


# Main execution
if __name__ == "__main__":
    batch_outputs_dir = "/Users/cai529/Github/pingpong_analysis/data/generated/batch_outputs/qualitative"
    output_path = "/Users/cai529/Github/pingpong_analysis/data/generated/threads_analysis.csv"

    if os.path.exists(batch_outputs_dir):
        result_df = process_batch_outputs(batch_outputs_dir)
        result_df.to_csv(output_path, index=False)
        print(f"Saved results to: {output_path}\n")
        print("Dataframe summary:")
        print(result_df.info())
        print("\nFirst few rows:")
        print(result_df.head())
    else:
        print(f"Directory not found: {batch_outputs_dir}")
