import os
import re
import json
import pandas as pd
from pathlib import Path
from typing import Optional, Literal
from pydantic import BaseModel, Field, ValidationError
from dotenv import load_dotenv
import tiktoken
from openai.lib._pydantic import to_strict_json_schema

load_dotenv()

# =====================
# Acceptable User Message Categories
UserMessageCategory = Literal[
    "Conceptual",  # asking for explanation
    "Procedural",  # e.g. how to solve ...
    "Clarification",  # seeking further explanation, perhaps from previous chat context
    "Application",  # applying course material to new context
    "Out of Scope",  # unrelated to the course content
    "Course Policies",  # about the administrative aspects of the course
    "Appears to be homework question",
    "Response to Chatbot Prompt, no question",
    "Other prompt"
]

UserMessageTone = Literal[
    "Neutral tone",
    "Polite",
    "Frustrated",
    "Urgent",
    "Confused",
    "Demanding answers",
]

UserQuestionComplexity = Literal[
    "Basic",
    "Intermediate",
    "Advanced"
]
# =====================
# Pydantic Models
# =====================

class UserClassification(BaseModel):
    new_conversation: bool = Field(description="TRUE if this message appears to be the first message about a new topic, FALSE otherwise. Some users may ask about multiple topics in a single thread, which should be considered a new conversation.")
    user_category: UserMessageCategory = Field(description="The type of user message. Conceptual questions ask for explanations, procedural questions ask how to do something, clarification questions seek further explanation, application questions apply course material to new contexts, out of scope questions are unrelated to the course content, course policies questions are about administrative aspects of the course, homework questions appear to be homework-related, response to chatbot prompt messages are responses to a chatbot prompt without a question, and other prompt messages are any other type of prompt.")
    misuse_homework: bool = Field(description="TRUE if the user message appears to be misusing the chatbot to get answers on homework or exams, FALSE otherwise. This does NOT includes messages that are not questions or that do not seek help with course material.")
    tone: UserMessageTone = Field(description="The tone of the user's message")
    complexity: UserQuestionComplexity = Field(description="The complexity of the user's question.")
    multiple_questions: int = Field(description="Number of questions the user asks at once")
    potentially_harmful: bool = Field(description="TRUE if the user message appears to be potentially harmful, FALSE otherwise.")

class AssistantClassification(BaseModel):
    category: UserMessageCategory = Field(description="The category of the assistant's response.")
    correctness: bool = Field(description="Whether the assistant's response is correct. TRUE if correct, FALSE if not.")
    appropriateness: bool = Field(description="Whether the assistant's response is appropriate for the user's question. TRUE if appropriate, FALSE if not.")
    avoid: bool = Field(decscription="Whether the assistant avoided answering a question it should have avoided. TRUE if it avoided answering, FALSE if it answered a question it should have avoided. This usually looks like the assistant deflecting saying 'It seems like you're asking about a homework question, I can't help with that.'")
    beyond_course: bool = Field(description="If the bot provided information beyond the scope of the course content and its specified system prompt. TRUE if it did, FALSE if it did not. This includes providing information that is not relevant to the course material or going into detail that is not necessary for the user's question.")
    gave_answer: bool = Field(description="TRUE if the bot provided an answer to a homework question. FALSE for any other response.")
    disobeyed_prompt: bool = Field(description="TRUE if the bot disobeyed or its prompt or behaved in a way opposite to the desired behavior. FALSE otherwise.") # whether the assistant disobeyed the prompt

# =====================
# Prompts
# =====================

def get_user_prompt(course_name: str) -> str:
    return f"""
You are an expert at classifying user messages with a chatbot for the course {course_name}.
Given the conversation context, classify the user's message according to the following criteria:
- Is this a new conversation?
- Message category (e.g., conceptual, procedural, clarification, etc.)
- Whether the user is misusing the chatbot for homework/exam answers
- Tone of the message
- Complexity of the question
- Number of questions asked
- Whether the message is potentially harmful

Your response MUST strictly match the following schema:
{{schema}}
""".strip().format(schema=to_strict_json_schema(UserClassification))

def get_assistant_prompt(course_name: str) -> str:
    return f"""
You are an expert at evaluating tutor chatbot responses for the course {course_name}.
Given the conversation context, classify the assistant's message according to the following criteria:
- Category of the response
- Correctness
- Appropriateness
- Whether the assistant avoided answering a question it should have avoided
- Whether the response goes beyond course content
- Whether the assistant gave a direct answer to a homework question
- Whether the assistant disobeyed its prompt

Your response MUST strictly match the following schema:
{{schema}}
""".strip().format(schema=to_strict_json_schema(AssistantClassification))

# =====================
# Token Counting
# =====================

MODEL_NAME = "o3-mini"
encoder = tiktoken.encoding_for_model(MODEL_NAME)

# =====================
# Batch Preparation Function
# =====================

def build_batch_jsonl(
    df: pd.DataFrame,
    model_name: str,
    output_jsonl_path: Path,
    output_csv_path: Path,
    batch_size: int = 1000
):
    """
    Build batches of chat completion requests as JSONL for Azure OpenAI batch API.
    Each JSONL file contains up to batch_size lines.
    Also adds a 'batch_request_id' column to the DataFrame and saves it as CSV.
    """
    df = df.reset_index(drop=True)
    thread_contexts = {}
    thread_system_prompt = {}
    batch_lines = []
    row_indices = []
    batch_request_ids = []

    for index, row in df.iterrows():
        thread_id = row["Thread ID"]
        thread_contexts.setdefault(thread_id, [])
        thread_messages = thread_contexts[thread_id]
        # Track the most recent system_prompt for this thread
        if row["Role"] == "system_prompt":
            thread_system_prompt[thread_id] = row["Content"]
            thread_messages.append({"role": row["Role"], "content": row["Content"]})
            thread_contexts[thread_id] = thread_messages[-6:]
            continue

        # Build context for assistant messages (prepend system_prompt if available)
        if row["Role"] == "assistant" and thread_id in thread_system_prompt:
            context_with_system = (
                [{"role": "system_prompt", "content": thread_system_prompt[thread_id]}] + thread_messages[-6:]
            )
            context_for_prompt = context_with_system
        else:
            context_for_prompt = thread_messages[-6:]

        # Compose prompt and messages
        if row["Role"] == "user":
            prompt = get_user_prompt(str(row["Class Name"]))
        else:
            prompt = get_assistant_prompt(str(row["Class Name"]))

        context_text = "\n".join([
            f'{m["role"].capitalize()}: {m["content"]}' for m in context_for_prompt
        ])
        prompt_full = prompt + f"\n\nPrevious conversation context:\n{context_text}\n\nNow classify the following message."

        messages = [
            {"role": "system", "content": prompt_full},
            {"role": row["Role"], "content": str(row["Content"])}
        ]

        # Compose batch line
        custom_id = f"{index:09d}"
        batch_line = {
            "custom_id": custom_id,
            "method": "POST",
            "url": "/chat/completions",
            "body": {
                "model": model_name,
                "messages": messages
            }
        }
        batch_lines.append(batch_line)
        row_indices.append(index)
        batch_request_ids.append(custom_id)

        # Always update the context for the thread
        thread_messages.append({"role": row["Role"], "content": row["Content"]})
        thread_contexts[thread_id] = thread_messages[-6:]

    # Add batch_request_id column to DataFrame
    # Only for rows that are not system_prompt (since those are skipped in batch)
    mask = df["Role"] != "system_prompt"
    df_with_id = df.copy()
    df_with_id.loc[mask, "batch_request_id"] = [bid for bid in batch_request_ids]
    # Save CSV
    output_csv_path.parent.mkdir(parents=True, exist_ok=True)
    df_with_id.to_csv(output_csv_path, index=False)
    print(f"Wrote CSV with batch_request_id to {output_csv_path}")

    # Write to JSONL in batches of batch_size
    output_jsonl_path.parent.mkdir(parents=True, exist_ok=True)
    total = len(batch_lines)
    num_batches = (total + batch_size - 1) // batch_size
    base_name = output_jsonl_path.stem
    ext = output_jsonl_path.suffix
    parent = output_jsonl_path.parent

    for i in range(num_batches):
        start = i * batch_size
        end = min((i + 1) * batch_size, total)
        batch = batch_lines[start:end]
        batch_file = parent / f"{base_name}_{i+1}{ext}"
        with open(batch_file, "w", encoding="utf-8") as f:
            for line in batch:
                f.write(json.dumps(line, ensure_ascii=False) + "\n")
        print(f"Wrote {len(batch)} batch requests to {batch_file}")

# =====================
# Main Entrypoint
# =====================

def main():
    input_csv = Path("data", "raw", "pingpong", "PingPong Threads, DeIdentified.csv")
    # Save to data/generated/batch/qualitative/threads_qualitative_batch.jsonl
    output_jsonl = Path("data", "generated", "batch", "qualitative", "threads_qualitative_batch.jsonl")
    output_csv = Path("data", "generated", "batch", "qualitative", "threads_batch.csv")
    df = pd.read_csv(input_csv)
    model_name = "o3-mini-2"
    build_batch_jsonl(df, model_name, output_jsonl, output_csv, batch_size=1000)

def print_env_vars():
    print("AZURE_API_KEY:", os.getenv("AZURE_API_KEY"))
    print("AZURE_ENDPOINT:", os.getenv("AZURE_ENDPOINT"))
    print("AZURE_REGION:", os.getenv("AZURE_REGION"))
    print("AZURE_API_VERSION:", os.getenv("AZURE_API_VERSION"))
    print("AZURE_DEPLOYMENT:", os.getenv("AZURE_DEPLOYMENT"))

def usage():
    print("Usage: Set environment variables for Azure OpenAI and MODEL_DEPLOYMENT_NAME.")

if __name__ == "__main__":
    main()
