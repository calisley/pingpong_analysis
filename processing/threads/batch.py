import os
import glob
import time
import json
import datetime
from pathlib import Path
from tqdm import tqdm
from openai import AzureOpenAI
from dotenv import load_dotenv
# Load environment variables
load_dotenv()

def upload_and_batch_all_jsonl(
    directory: str,
    completion_window: str = "24h",
    file_expiry_seconds: int = 604800,  # 7 days
    output_expiry_seconds: int = 604800  # 7 days
):
    """
    Uploads all .jsonl files in the given directory to Azure OpenAI as batch files,
    submits them as batch jobs, tracks their status, and prints/saves the results.
    All batch jobs are submitted at once, and then monitored in parallel.
    """
    client = AzureOpenAI(
        api_key=os.getenv("AZURE_API_KEY_GPT"),
        api_version="2024-12-01-preview",
        azure_endpoint=os.getenv("AZURE_ENDPOINT_GPT")
    )

    jsonl_files = sorted(glob.glob(os.path.join(directory, "*.jsonl")))
    print(f"Found {len(jsonl_files)} .jsonl files in {directory}")

    # Step 1: Upload all files
    file_uploads = []
    for jsonl_path in tqdm(jsonl_files, desc="Uploading files"):
        with open(jsonl_path, "rb") as f:
            file = client.files.create(
                file=f,
                purpose="batch",
                extra_body={
                    "expires_after": {
                        "seconds": file_expiry_seconds,
                        "anchor": "created_at"
                    }
                }
            )
        file_uploads.append({
            "file": file,
            "jsonl_path": jsonl_path
        })

    # Step 2: Submit all batch jobs
    batch_info = []
    for upload in tqdm(file_uploads, desc="Submitting batch jobs"):
        file = upload["file"]
        jsonl_path = upload["jsonl_path"]
        batch_response = client.batches.create(
            input_file_id=file.id,
            endpoint="/chat/completions",
            completion_window=completion_window,
            extra_body={
                "output_expires_after": {
                    "seconds": output_expiry_seconds,
                    "anchor": "created_at"
                }
            }
        )
        batch_info.append({
            "batch": batch_response,
            "file": file,
            "jsonl_path": jsonl_path
        })

    # Step 3: Monitor all batch jobs and retrieve results as they complete
    # We'll poll all batches in a loop until all are done
    batch_status = {}
    for info in batch_info:
        batch_id = info["batch"].id
        batch_status[batch_id] = {
            "status": info["batch"].status,
            "info": info,
            "retrieved": False
        }

    print(f"Monitoring {len(batch_status)} batch jobs...")
    output_dir = Path(directory) / "batch_outputs"
    output_dir.mkdir(exist_ok=True)

    while True:
        all_done = True
        for batch_id, status_dict in batch_status.items():
            if status_dict["status"] in ("completed", "failed", "canceled") and status_dict["retrieved"]:
                continue  # Already handled
            all_done = False
            # Poll status
            batch_response = client.batches.retrieve(batch_id)
            status_dict["status"] = batch_response.status
            if batch_response.status in ("completed", "failed", "canceled") and not status_dict["retrieved"]:
                info = status_dict["info"]
                jsonl_path = info["jsonl_path"]
                print(f"{datetime.datetime.now()} Batch Id: {batch_id}, Status: {batch_response.status}")
                # Handle errors if failed
                if batch_response.status == "failed":
                    print("Batch job failed. Errors:")
                    for error in getattr(batch_response.errors, "data", []):
                        print(f"Error code {error.code} Message {error.message}")
                # Save or print results
                output_file_id = getattr(batch_response, "output_file_id", None)
                if not output_file_id:
                    output_file_id = getattr(batch_response, "error_file_id", None)
                if output_file_id:
                    print(f"Retrieving results from file_id: {output_file_id}")
                    file_response = client.files.content(output_file_id)
                    # Try to decode as text, fallback to bytes if needed
                    try:
                        text = file_response.text
                    except Exception:
                        text = file_response.read().decode("utf-8")
                    raw_responses = text.strip().split('\n')
                    output_file = output_dir / f"{Path(jsonl_path).stem}_batch_output.jsonl"
                    with open(output_file, "w", encoding="utf-8") as out_f:
                        for raw_response in raw_responses:
                            try:
                                json_response = json.loads(raw_response)
                                formatted_json = json.dumps(json_response, indent=2)
                                out_f.write(formatted_json + "\n")
                            except Exception as e:
                                print(f"Error parsing response: {e}")
                    print(f"Saved batch output to {output_file}")
                else:
                    print("No output file id found for this batch.")
                status_dict["retrieved"] = True
        if all_done:
            print("All batch jobs completed and results retrieved.")
            break
        time.sleep(60)  # Wait before polling again

if __name__ == "__main__":
    # Example usage:
    upload_and_batch_all_jsonl(
        directory="/Users/cai529/Github/pingpong_analysis/data/generated/batch/qualitative"
    )
