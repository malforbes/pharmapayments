from scraper_api import ScraperAPIClient
import csv
import scrapy
import json
import requests
import os
import concurrent.futures

keywords = [
    "ADDICTION", "ANAESTHES", "CARDIOL", "THORACIC SURGERY", "CLINICAL GENETIC",
    "CLINICAL PHARM", "COLORECTAL SURG", "DERMATOLOG", "RADIOLOGY",
    "DOCTOR-IN-TRAINING", "EMERGENCY", "ENDOCRIN", "GASTRO", "HEPATO",
    "GENERAL MEDICINE", "GP", "GENERAL SURG", "GERIATRIC", "HAEMATOLOG",
    "IMMUNOLOG", "INFECTIOUS", "INTENSIVE CARE", "MEDICAL ADMINISTRATION",
    "MICROBIOLOG", "NEPHROLO", "NEUROLOGY", "NEUROSURG", "NUCLEAR",
    "OCCUPATIONAL", "OBSTETRIC", "OPHTHALM", "ORTHOPAEDIC", "MAXILLOFACIAL",
    "OTOLARYNGOL", "PAEDIATRIC ENDOCRINOL", "PAEDIATRIC GASTRO", "PAEDIATRIC HAEMAT",
    "PAEDIATRIC IMMUNOLO", "PAEDIATRIC INFECTIOUS", "PAEDIATRIC MEDICAL ONCOLOGY",
    "PAEDIATRIC NEUROL", "PAEDIATRIC RESPIRATO", "PAEDIATRIC RHEUMAT", "PAEDIATRIC SURGERY",
    "PAEDIATRICS AND CHILD HEALTH", "PAEDRIATICS", "PAIN MEDICINE", "PALLIATIVE MEDICINE",
    "PATHOLOGY", "PHYSICIAN", "PLASTIC", "PSYCHIAT", "PUBLIC HEALTH", "RADIATION ONCOL",
    "REHABILITATION", "RESPIRATORY", "RHEUMATOLO", "SEXUAL HEALTH", "SPORT AND EXERCISE",
    "SURGE", "UROLOG", "VASCULAR", "GENERAL PRACT", "ONCOLOG", "ALLERG", "RENAL",
    "GYNAECOL", "EYE"
]

if 'google_output.csv' not in os.listdir(os.getcwd()):
    with open("google_output.csv","a") as f:
        writer = csv.writer(f)
        writer.writerow(['Name','Address','Title','Matched Keyword'])

alreadyscrapped = []
with open("google_output.csv","r") as r:
    reader = csv.reader(r)
    for line in reader:
        alreadyscrapped.append(f"{line[0]}{line[1]}")


def getdata(line):
    if f"{line[0]}{line[1]}" not in alreadyscrapped:
        try:
            url = "https://google.serper.dev/search"

            payload = json.dumps({
                "q": f"{line[0]} {line[1]}"
            })
            headers = {
                'X-API-KEY': '61a601e970a8688773c8a9be5b7e4a1cd0dc1ee6',
                'Content-Type': 'application/json'
            }

            response = requests.request("POST", url, headers=headers, data=payload)

            print(response.text)

            if response.json()['organic']:
                title = response.json()['organic'][0]['title']

                matched_keywords = ', '.join([keyword for keyword in keywords if keyword.lower() in title.lower()])

                with open("google_output.csv","a") as f:
                    writer = csv.writer(f)
                    writer.writerow([line[0],line[1],title,matched_keywords])
                    print([line[0],line[1],title,matched_keywords])

            else:
                with open("google_output.csv","a") as f:
                    writer = csv.writer(f)
                    writer.writerow(line)
                    print(line)

        except:
            print("Error...")
            pass

    else:
        print("Exists...")


with open("sample.csv","r") as r:
    reader = csv.reader(r)
    next(reader)

    with concurrent.futures.ThreadPoolExecutor(max_workers=50) as f:
        a = {
            f.submit(getdata,line)
            for line in reader
        }

    # for line in reader:

        # req = client.get(f"https://www.google.com/search?q={line[0]} {line[1]}")
        # print(req)
        # title = scrapy.Selector(text=req.text).xpath('.//*[@class="LC20lb MBeuO DKV0Md"]/text()').extract_first()

