import os
import json
import re
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
        "Java Fullstack","Entwickler:in", "Senior Developer", "Python Developer", "Applikations-Entwickler", "Software Development", "Full Stack Entwickler",
    },
    
    "Data Engineer": {
        "Data Scientist", "Machine Learning Engineer", "AI Engineer",
        "Big Data Engineer", "Data Engineer", "Data Analyst",
        "Künstliche Intelligenz Ingenieur", "KI-Ingenieur", "Datenwissenschaftler", "Computer Vision",
        "Datenanalyst", "Dateningenieur", "Maschinelles Lernen Ingenieur", "Database Engineer", "Teamlead Data", "Data Architect", 
        "Data Warehouse Architect", "Data Warehouse Engineer","Data Integration Engineer","Datenbankarchitekt", "SQL-Entwickler", 
        "Datenbank Engineer", "Datenbank Spezialist", "Generative AI", "Computer Vision Engineer", "NLP Engineer", "Computer Vision Spezialist",
        "Natural Language Processing Engineer", "Robotik Ingenieur", "Data Science", "Data & AI"
    },
    
    "Cloud Engineer": {
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
        "Design Engineer", "UX Designer", "UI Designer", "Product Designer"
    },
    
    "Consulting & Management": {
        "IT-Consultant", "IT-Berater", "Technischer Berater", "Projektmanager IT",
        "IT-Projektleiter", "Technischer Produktmanager", "Technical Lead", 
        "Requirements Engineer", "Middleware Engineer", "Application-Manager", "IT-Security Manager", "Lead Architect", 
        "Solution Engineer", "Requirement Engineer", "Projektleiter:in", "IT Management", "Salesforce Engineer", "Cyber Security Consultant", "Cloud Consultant"
    }
}

# Define a set of CS-related keywords (can be extended)
programming_languages = {
    "Python", "Java", "C", "C#", "C++", "JavaScript", "TypeScript", "HTML", "CSS",
    "Go", "Rust", "Swift", "Kotlin", "Ruby", "PHP", "Scala", "Perl", "Lua", "Dart",
    "R", "MATLAB", "Shell", "Bash", "PowerShell", "Objective-C", "SQL"
}

frameworks = {
    "React", "Angular", "Vue", "Svelte", "Next.js", "Nuxt.js", "SolidJS",  # Web & Frontend Frameworks
    "Node.js", "Django", "Flask", "Spring", "FastAPI", "Express.js", "NestJS",  # Backend Frameworks
    "ASP.NET", "Ruby on Rails", "Laravel", "Symfony", "Ktor",  # Backend Frameworks
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

def extract_keywords(job, description_text):
    
    # Find all matching keywords
    job["programming_languages"] = sorted({kw for kw in programming_languages if re.search(rf"\b{kw}\b", description_text, re.IGNORECASE)})
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
    if any(term in description_text.lower() for term in ["mehrjährig", "langjährig", "viele jahre", "many years", "mehrere Jahre"]):
        years.add(2)  

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
    
    
    
    
    
    
parent_dir = os.path.dirname(os.path.dirname(__file__))


with open(parent_dir+'/data/jobs.json', 'r') as file:
    jobs = json.load(file)
with open(parent_dir+'/data/jobs2.json', 'r') as file:
    jobs2 = json.load(file)

# Total number of jobs to process
total_jobs = len(jobs) + len(jobs2)

jobs_processed = []
for i, job in enumerate(jobs+jobs2):
    print(f"Preprocessing {i}/{total_jobs}\n")
    descriptions_text = " ".join(
        " ".join(desc_list)
        for desc_dict in job["descriptions"]
        for desc_list in desc_dict.values()
    )
    if "job_title" in job:
        extract_job_name(job)
        extract_career_stage(job)
        
            
        extract_keywords(job, descriptions_text)
        extract_experience(job, descriptions_text)
        extract_education_stage(job, descriptions_text)
        
        jobs_processed.append(job)


with open(parent_dir+'/data/jobs_processed.json', "w", encoding="utf-8") as file:
    json.dump(jobs_processed, file, indent=4, ensure_ascii=False)
        
    