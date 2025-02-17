import os
import json
import re
from tqdm import tqdm
import pandas as pd

# Define CS-related job keywords
cs_job_categories = {
    "Software Engineer": {
        "Software Engineer", "Software Developer", "Fullstack Developer", "Full-Stack Developer", "Fullstack Entwickler",
        "Backend Engineer", "Frontend Engineer", "Web Developer", "Mobile Developer",
        "Game Developer", "Embedded Systems Engineer", "Software Architect",
        "Fullstack Engineer", "Full Stack Engineer", "Software Entwickler", "Software-Entwickler", "Softwarentwickler",
        "Software Entwickler/in", "Applikationsentwickler", "Applikationsentwickler/in",
        "Softwarearchitekt", "Softwarearchitekt/in", "Anwendungsentwickler",
        "Anwendungsentwickler/in", "IT-Entwickler", "IT-Entwickler/in", "Software-Engineer", "Softwareengineer",
        "Application Developer", "Automation Engineer", "Frontend Developer", "Backend Developer",
        "Application Engineer", "Software-Entwicklungsingenieur", "Front-End Engineer", "Research Engineer", 
        "Softwareentwickler", "Software Ingenieur", "Full Stack Developer", "Mobile App Ingenieur", "Mobile App Entwickler", "Java Entwickler",
        "Java Fullstack","Entwickler:in", "Senior Developer", "Python Developer", "Java Developer", "Angular Developer", 
        "Applikations-Entwickler", "Software Development","Software-Developer", "Full Stack Entwickler", "Android Developer", "iOS Developer",
        "Java Engineer",
    },
    
    "Data Engineer": {
        "Data Scientist", "Machine Learning Engineer", "AI Engineer",
        "Big Data Engineer", "Data Engineer", "Data Analyst",
        "Künstliche Intelligenz Ingenieur", "KI-Ingenieur", "Datenwissenschaftler", "Computer Vision",
        "Datenanalyst", "Dateningenieur", "Maschinelles Lernen Ingenieur", "Database Engineer", "Teamlead Data", "Data Architect", 
        "Data Warehouse Architect", "Data Warehouse Engineer","Data Integration Engineer","Datenbankarchitekt", "SQL-Entwickler", 
        "Datenbank Engineer", "Datenbank Spezialist", "Generative AI", "Computer Vision Engineer", "NLP Engineer", "Computer Vision Spezialist",
        "Natural Language Processing Engineer", "Robotik Ingenieur", "Data Science", "Data & AI", "R&D", "wissenschaftliche Hilfskraft", "Large Language Models Engineer",
        "3D Developer", "GIS-Spezialist", "Data Specialist", "Data management", "Datenmanagement", 
    },
    
    "Cloud/System Engineer": {
        "Cloud Engineer", "Cloud Architect", "Site Reliability Engineer",
        "System Engineer", "Systemadministrator", "Cloud-Architekt", "SRE",
        "Systemingenieur", "Netzwerkadministrator", "Network Engineer", 
         "Cloud Systems Engineer"
    },
    
    "Security Engineer": {
        "Cybersecurity Engineer", "Security Engineer", "IT-Security Engineer",
        "Netzwerksicherheit Ingenieur", "IT-Sicherheitsspezialist",
        "Penetration Tester", "Ethical Hacker", "Netzwerktechniker", "Network Engineer", "Security Researcher"
    },
    
    "Infrastructure": {
        "Database Administrator", "Datenbankadministrator", "Datenbankentwickler",
         "IT-Architekt", "Plattformingenieur",
        "Platform Engineer", "Storage Engineer", "Systems Engineer", "Test Engineer",
        "System-Engineer",
        "Datenbank-Entwickler", "Datenbank-Administrator",
        "DevOps Engineer", "Test Engineer", "IT-Administrator", "ICT Architekt", "ICT-Architekt",
        "IT-Supporter", "Verification Engineer", "Automatisierungsingenieur"
    },
    
    
    "Design": {
        "Design Engineer", "UX Designer", "UI Designer", "Product Designer", "UI/UX"
    },
    
    "Consulting & Management": {
        "IT-Consultant", "IT-Berater", "Technischer Berater", "Projektmanager IT",
        "IT-Projektleiter", "Technischer Produktmanager", "Technical Lead", 
        "Requirements Engineer", "Middleware Engineer", "Application-Manager", "IT-Security Manager", "Lead Architect", 
        "Solution Engineer", "Requirement Engineer", "Projektleiter:in", "IT Management", "Salesforce Engineer", "Cyber Security Consultant", "Cloud Consultant",
        "Wirtschaftsinformatiker", "IT Application Manager", 
    }
}

# Define a set of CS-related keywords (can be extended)
programming_languages = {
    "Python", "Java", "C", "C#", "C++", "JavaScript", "TypeScript", "HTML", "CSS",
    "Go", "Rust", "Swift", "Kotlin", "Ruby", "PHP", "Scala", "Perl", "Lua", "Dart",
    " R ", "MATLAB", "Shell", "Bash", "PowerShell", "Objective-C", "SQL"
}

frameworks = {
    "React", "Angular", "Vue", "Svelte", "Next.js", "Nuxt.js", "SolidJS",  # Web & Frontend Frameworks
    "Node.js", "Django", "Flask", "Spring", "FastAPI", "Express.js", "NestJS",  # Backend Frameworks
    "ASP.NET", "Ruby on Rails", "Laravel", "Symfony", "Ktor",".NET"  # Backend Frameworks
}

tools = {
    "Docker", "Kubernetes", "Git", "Terraform", "Ansible", "Jenkins", "GitLab", "CI/CD", "Grafana"
}
operating_systems = {
    "Linux", "Windows", "macOS"
}

# Education levels and their corresponding groups
education_levels = {
    "Bachelor": [r"\bBachelor\b", r"\bB\.Sc\.\b", r"\bBSc\b", r"\bB\.A\.\b", r"\bBA\b", r"\bB\.Eng\.\b", r"\bBEng\b", r"\bB\.Ed\.\b", r"\bBBA\b", r"\bBachelor of Science\b", r"\bBachelor of Arts\b", r"\bBachelor of Engineering\b", r"\bBachelor of Education\b", r"\bBachelor of Business Administration\b", r"\bBachelordiplom\b", r"\bFH-Diplom\b", r"\bDiplom \(FH\)\b", r"\bDipl\.-Ing\.\(FH\)\b", r"Hochschulstudium", r"Hochschulabschluss"],
    
    "Master": [r"\bMaster\b", r"\bM\.Sc\.\b", r"\bMSc\b", r"\bM\.A\.\b", r"\bMA\b", r"\bM\.Eng\.\b", r"\bMEng\b", r"\bMBA\b", r"\bMaster of Science\b", r"\bMaster of Arts\b", r"\bMaster of Engineering\b", r"\bMaster of Business Administration\b", r"\bMagister\b", r"\bDiplom\b", r"\bDipl\.-Ing\.\b"],
    
    "PhD": [r"\bPhD\b", r"\bDoctorate\b", r"\bDr\.\b", r"\bDr\.-Ing\.\b", r"\bDr\. rer\. nat\.\b", r"\bDr\. phil\.\b", r"\bDoktor\b", r"\bDoktortitel\b", r"\bHabilitation\b", r"\bPostdoc\b", r"\bPost Doc\b"],
    
    "Vocational": [r"\bAusbildung\b", r"\bBerufsausbildung\b", r"\bTechniker\b", r"\bMeister\b", r"\bAssociate Degree\b", r"\bAdvanced Diploma\b", r"\bInformatikausbildung\b", r"Höhere Berufsbildung"  ]
}

number_map = {
    "zero": 0, "one": 1, "two": 2, "three": 3, "four": 4, "five": 5, 
    "six": 6, "seven": 7, "eight": 8, "nine": 9, "ten": 10,
    "null": 0, "eins": 1, "zwei": 2, "drei": 3, "vier": 4, "fünf": 5, 
    "sechs": 6, "sieben": 7, "acht": 8, "neun": 9, "zehn": 10
}
# List of canton names and abbreviations
cantons = [
    "Aargau", "AG", "Appenzell Ausserrhoden", "AI", "Appenzell Innerrhoden", 
    "AR", "Basel-Landschaft", "BL", "Basel-Stadt", "BS", "Bern", "BE", "Fribourg", 
    "FR", "Geneva", "GE", "Glarus", "GL", "Graubünden", "GR", "Jura", "JU", "Luzern", 
    "LU", "Neuchatel", "NE", "Nidwalden", "NW", "Obwalden", "OW", "Schaffhausen", 
    "SH", "Schwyz", "SZ", "Solothurn", "SO", "St. Gallen", "SG", "Ticino", "TI", 
    "Thurgau", "TG", "Uri", "UR", "Valais", "VS", "Vaud", "VD", "Zug", "ZG", 
    "Zürich", "ZH", "Sitten"
]

# Create a dictionary of abbreviations mapping to full canton names
canton_dict = {
    "AG": "Aargau", "AI": "Appenzell Ausserrhoden", "AR": "Appenzell Innerrhoden", 
    "BL": "Basel-Landschaft", "BS": "Basel-Stadt", "BE": "Bern", "FR": "Fribourg", 
    "GE": "Geneva", "GL": "Glarus", "GR": "Graubünden", "JU": "Jura", "LU": "Luzern", 
    "NE": "Neuchatel", "NW": "Nidwalden", "OW": "Obwalden", "SH": "Schaffhausen", 
    "SZ": "Schwyz", "SO": "Solothurn", "SG": "St. Gallen", "TI": "Ticino", 
    "TG": "Thurgau", "UR": "Uri", "VS": "Valais", "VD": "Vaud", "ZG": "Zug", 
    "ZH": "Zürich", "Sitten": "Valais"
}

def extract_keywords(job, description_text):
    
    # Find all matching keywords
    extracted_languages = set()
    
    for kw in programming_languages:
        if kw == "C":
            # Ensure "C" is not part of "C++" or "C#"
            if re.search(r"\bC\b(?![#\+])", description_text):
                extracted_languages.add("C")
        elif kw in {"C#", "C++"}:
            # Directly match "C#" and "C++" since \b doesn't work well
            if re.search(rf"(?<!\w){re.escape(kw)}(?!\w)", description_text):
                extracted_languages.add(kw)
        else:
            # General case for other languages
            if re.search(rf"\b{re.escape(kw)}\b", description_text, re.IGNORECASE):
                extracted_languages.add(kw)
    
    job["programming_languages"] = sorted(extracted_languages)
    job["frameworks"] = sorted({fw for fw in frameworks if re.search(rf"\b{fw}\b", description_text, re.IGNORECASE)})
    job["tools"] = sorted({tool for tool in tools if re.search(rf"\b{tool}\b", description_text, re.IGNORECASE)})
    job["operating_systems"] = sorted({os for os in operating_systems if re.search(rf"\b{os}\b", description_text, re.IGNORECASE)})
    
    
def extract_experience(job, description_text):
    years = set()  

    experience_patterns = [
        r"(\d+)\+?\s*(?:years?|Jahre)",          
        r"at least (\d+)\s*years?",              
        r"mindestens (\d+)\s*Jahre?",            
        r"mehr als (\d+)\s*Jahre?",              
        r"more than (\d+)\s*years?",             
        r"(\d+)-(\d+)\s*(?:years?|Jahre)",       
        r"(zero|one|two|three|four|five|six|seven|eight|nine|ten)\s*(?:years?|Jahre)",  # English words
        r"(null|eins|zwei|drei|vier|fünf|sechs|sieben|acht|neun|zehn)\s*(?:years?|Jahre)"  # German words
    ]

    # Handle general terms meaning "several years"
    if any(term in description_text.lower() for term in ["mehrjährig", "langjährig", "viele jahre", "many years", "mehrere jahre", "einige jahre"]):
        years.add(2)  
    if any(term in description_text.lower() for term in ["solide Kenntnisse"]):
        years.add(1)  
    for pattern in experience_patterns:
        matches = re.findall(pattern, description_text, re.IGNORECASE)
        for match in matches:
            if isinstance(match, tuple):  
                for num in match:
                    if num.isdigit():
                        years.add(int(num))
                    elif num.lower() in number_map:  
                        years.add(number_map[num.lower()])
            elif match.isdigit():  
                years.add(int(match))
            elif match.lower() in number_map:  
                years.add(number_map[match.lower()])
    
    job["years"] = sorted(years)

def extract_job_name(job):

    for job_category, job_titles in cs_job_categories.items():
        for job_title in job_titles:
            if job_title.lower() in job["job_title"].lower():
                job["job_title_cleaned"] = job_title
                job["job_category"] = job_category
            
    
    

def extract_career_stage(job):
    career_stages = {"junior", "senior"}
    job_title = job["job_title"].lower()
    career_stage_cleaned = None
    for career_stage in career_stages:
        if career_stage in job_title:
            career_stage_cleaned = career_stage
    
    job["career_stage_cleaned"] = career_stage_cleaned
    
def extract_education_stage(job, description_text):
    education = []

    for group, patterns in education_levels.items():
        for pattern in patterns:
            if re.search(pattern, description_text, re.IGNORECASE):
                if group not in education:
                    education.append(group)
                break  # If one pattern matches, no need to check the rest for that group
    

    job["education"] = education


def extract_canton(job):
    approx_canton = None
    if 'place_of_work' in job.keys() and isinstance(job['place_of_work'], str):
        address_lower = job['place_of_work'].lower()  # Make address lowercase for easier matching
        for canton in cantons:
            # Match canton names or abbreviations (case-insensitive)
            if re.search(r"\b" + re.escape(canton.lower()) + r"\b", address_lower):
                approx_canton = canton_dict.get(canton.upper(), canton)  # Ensure abbreviation maps to full name


    job["canton"] = approx_canton
    
    
if __name__ == "__main__":
    parent_dir = os.path.dirname(os.path.dirname(__file__))


    file_path = os.path.join(parent_dir, 'data', 'jobs.json')

    # count duplicates in jobs.json
    df = pd.read_json(file_path)

    initial_count = len(df)

    removed_url_duplicates = df.duplicated(subset=['url']).sum()
    removed_company_job_title_duplicates = df.duplicated(subset=['company', 'job_title']).sum()

    total_duplicates = removed_url_duplicates + removed_company_job_title_duplicates

    print(f"Total duplicate entries: {total_duplicates}")
    print(f" - Duplicate entries based on URL (should be Zero, since we only download non duplicate urls): {removed_url_duplicates}")
    print(f" - Duplicate entries based on Company & Job Title: {removed_company_job_title_duplicates}")

    with open(parent_dir+'/data/jobs.json', 'r') as file:
        jobs = json.load(file)

    with open(parent_dir+'/data/jobs_processed.json', 'r', encoding="utf-8") as file:
        jobs_processed = json.load(file)
    
    processed_urls = [item['url'] for item in jobs_processed if "url" in item]
    
    for i, job in tqdm(enumerate(jobs), total=len(jobs), desc="Preprocessing Jobs", ncols=100):
        if job['url'] in processed_urls:
            continue
        descriptions_text = " ".join(
            " ".join(desc_list)
            for desc_dict in job["descriptions"]
            for desc_list in desc_dict.values()
        )
        
        if "job_title" in job and job["job_title"]:
            extract_job_name(job)
            extract_career_stage(job)
            extract_canton(job)
            extract_keywords(job, descriptions_text)
            extract_experience(job, descriptions_text)
            extract_education_stage(job, descriptions_text)

            jobs_processed.append(job)


    with open(parent_dir+'/data/jobs_processed.json', "w", encoding="utf-8") as file:
        json.dump(jobs_processed, file, indent=4, ensure_ascii=False)
        
    