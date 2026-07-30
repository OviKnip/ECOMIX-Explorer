import ollama
import re
import os
from tqdm import tqdm

# Configuration
INPUT_FILE = 'data/modelled/waterbodyInfo.txt'
MODEL_NAME = 'gemma4:31b-mlx'
PLACEHOLDER_TEXT = 'DUMMY PLACEHOLDER] No verified background information is available yet for this water body.'
MAX_WATERBODIES = 9999

def get_catchment_info(wb_id, wb_name):
    """Calls the Ollama model to generate a catchment overview for a water body."""
    prompt = (
        f"Provide a concise, approximately 40-word overview of the catchment characteristics "
        f"(including typical land use, geology, and rainfall patterns) for the UK WFD waterbody "
        f"\"{wb_name}\" (waterbody ID: {wb_id}). "
        f"Focus on factual geographical and environmental descriptors."
    )

    try:
        response = ollama.generate(model=MODEL_NAME, prompt=prompt)
        return response['response'].strip()
    except Exception as e:
        print(f"Error calling Ollama for {wb_id}: {e}")
        return None

def main():
    if not os.path.exists(INPUT_FILE):
        print(f"Error: File {INPUT_FILE} not found.")
        return

    with open(INPUT_FILE, 'r', encoding='utf-8') as f:
        content = f.read()

    # Split the file into sections starting with ###
    # This regex finds everything from one ### to the next
    sections = re.split(r'(?=###\s+)', content)

    updated_sections = []
    processed_count = 0

    with tqdm(total=MAX_WATERBODIES, desc="Processing waterbodies", unit="wb") as pbar:
        for section in sections:
            if not section.strip():
                continue

            # Extract the Waterbody ID (the first word after ###)
            match = re.search(r'###\s+(\S+)', section)
            if match:
                wb_id = match.group(1)

                # Extract the Water body name for this section
                name_match = re.search(r'Water body:\s*(.+)', section)
                wb_name = name_match.group(1).strip() if name_match else wb_id

                # Check if this specific section contains the placeholder,
                # and stop once we've processed the configured limit
                if PLACEHOLDER_TEXT in section and processed_count < MAX_WATERBODIES:
                    pbar.set_postfix_str(f"{wb_id} ({wb_name})")

                    # Fetch info from Ollama
                    new_info = get_catchment_info(wb_id, wb_name)

                    if new_info:
                        # Replace the placeholder with the generated text
                        section = section.replace(PLACEHOLDER_TEXT, new_info)
                        processed_count += 1
                        pbar.update(1)
                    else:
                        tqdm.write(f"Skipping {wb_id} due to API error.")

            updated_sections.append(section)

    # Join sections back together and write to file
    final_content = "".join(updated_sections)

    with open(INPUT_FILE, 'w', encoding='utf-8') as f:
        f.write(final_content)

    print("Processing complete.")

if __name__ == "__main__":
    main()
