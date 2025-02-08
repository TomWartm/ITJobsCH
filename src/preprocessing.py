import os
import json
import re
# Define CS-related job keywords
cs_jobs = {
    # General Software & Web Development
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
    "Java Fullstack",
    
    # Data Science & AI
    "Data Scientist", "Machine Learning Engineer", "AI Engineer",
    "Big Data Engineer", "Data Engineer", "Data Analyst",
    "Künstliche Intelligenz Ingenieur", "KI-Ingenieur", "Datenwissenschaftler", "Computer Vision",
    "Datenanalyst", "Dateningenieur", "Maschinelles Lernen Ingenieur", "Database Engineer", "Teamlead Data", "Data Architect",
    
    # Cloud & DevOps
    "Cloud Engineer", "DevOps Engineer", "Cloud Architect", "Site Reliability Engineer",
    "System Engineer", "Systemadministrator", "Cloud-Architekt", "SRE",
    "Systemingenieur", "Netzwerkadministrator", "IT-Administrator", "ICT Architekt", "ICT-Architekt", "Network Engineer", 
    "Test Engineer", "Cloud Systems Engineer",
    
    # Security & Networking
    "Cybersecurity Engineer", "Security Engineer", "IT-Security Engineer",
    "Netzwerksicherheit Ingenieur", "IT-Sicherheitsspezialist",
    "Penetration Tester", "Ethical Hacker", "Netzwerktechniker", "Network Engineer",
    
    # Specialized Fields
    "Computer Vision Engineer", "NLP Engineer", "Computer Vision Spezialist",
    "Natural Language Processing Engineer", "Robotik Ingenieur", "Automatisierungsingenieur",
    
    # Database & Infrastructure
    "Database Administrator", "Datenbankadministrator", "Datenbankentwickler",
    "Datenbankarchitekt", "SQL-Entwickler", "IT-Architekt", "Plattformingenieur",
    "Platform Engineer", "Storage Engineer", "Systems Engineer", "Test Engineer",
    "Data Integration Engineer", "Data Warehouse Engineer", "Data Warehouse Architect","System-Engineer",
    "Datenbank Engineer", "Datenbank Spezialist", "Datenbank-Entwickler", "Datenbank-Administrator", 
    
    # Design
    "Design Engineer", "UX Designer", "UI Designer", "Product Designer",
    
    # Consulting & Management
    "IT-Consultant", "IT-Berater", "Technischer Berater", "Projektmanager IT",
    "IT-Projektleiter", "Technischer Produktmanager", "Technical Lead", "Senior Developer", "Entwickler:in", 
    "Requirements Engineer", "Middleware Engineer", "Application-Manager", "IT-Security Manager", "Lead Architect", 
    "Solution Engineer", "Requirement Engineer"
}

# Define a set of CS-related keywords (can be extended)
cs_keywords = {
    
    # Programming Languages
    "Python", "Java", "C", "C#", "C++", "JavaScript", "TypeScript", "HTML", "CSS",
    "Go", "Rust", "Swift", "Kotlin", "Ruby", "PHP", "Scala", "Perl", "Lua", "Dart",
    "R", "MATLAB", "Shell", "Bash", "PowerShell", "Objective-C",

    # Web & Frontend Frameworks
    "React", "Angular", "Vue", "Svelte", "Next.js", "Nuxt.js", "SolidJS",
    
    # Backend Frameworks
    "Node.js", "Django", "Flask", "Spring", "FastAPI", "Express.js", "NestJS",
    "ASP.NET", "Ruby on Rails", "Laravel", "Symfony", "Ktor",

    # Databases & Query Languages
    "SQL", "NoSQL", "PostgreSQL", "MongoDB", "MySQL", "SQLite", "Cassandra",
    "MariaDB", "CockroachDB", "DynamoDB", "OracleDB", "Redis", "Elasticsearch",
    "GraphQL", "CouchDB", "InfluxDB", "Neo4j", "Firebase", "Snowflake",

    # DevOps, Cloud & CI/CD
    "Docker", "Kubernetes", "Terraform", "Ansible", "Jenkins", "GitLab", "CI/CD",
    "CircleCI", "TravisCI", "AWS", "Azure", "GCP", "IBM Cloud", "DigitalOcean",
    "OpenShift", "CloudFormation", "Pulumi", "Helm", "ArgoCD",

    # Messaging & Streaming
    "Kafka", "RabbitMQ", "MQTT", "ActiveMQ", "ZeroMQ", "NATS", "Pulsar",

    # AI/ML & Data Science
    "TensorFlow", "PyTorch", "Scikit-Learn", "Keras", "XGBoost", "LightGBM",
    "OpenCV", "NLTK", "spaCy", "Pandas", "NumPy", "Matplotlib", "Seaborn",
    "Dask", "Hugging Face", "LangChain", "Ray", "MLflow", "Kubeflow",

    # Big Data & Distributed Computing
    "Apache Spark", "Apache Flink", "Hadoop", "Apache Beam", "Presto", "Dremio",

    # API & Microservices
    "REST", "GraphQL", "gRPC", "OpenAPI", "Swagger", "Postman", "OAuth", "JWT",

    # Security
    "OAuth", "OIDC", "JWT", "SSL/TLS", "OWASP", "SAST", "DAST", "SIEM", "SOC",
    "IAM", "PKI", "Zero Trust", "Cybersecurity", "Penetration Testing",

    # Blockchain & Web3
    "Ethereum", "Solidity", "Web3.js", "Hardhat", "Truffle", "IPFS", "Polygon",
    "Binance Smart Chain", "Rust (Solana)", "Smart Contracts",

    # Other
    "GraphQL", "WebSockets", "Reverse Proxy", "Nginx", "Apache", "Varnish",
    "Load Balancing", "CDN", "Edge Computing", "Linux", "Windows", "macOS", "Office 365",
    "Grafana"
}
# Education levels and their corresponding groups
education_levels = {
    "Bachelor": [r"\bBachelor\b", r"\bB\.Sc\.\b", r"\bBSc\b", r"\bB\.A\.\b", r"\bBA\b", r"\bB\.Eng\.\b", r"\bBEng\b", r"\bB\.Ed\.\b", r"\bBBA\b", r"\bBachelor of Science\b", r"\bBachelor of Arts\b", r"\bBachelor of Engineering\b", r"\bBachelor of Education\b", r"\bBachelor of Business Administration\b", r"\bBachelordiplom\b", r"\bFH-Diplom\b", r"\bDiplom \(FH\)\b", r"\bDipl\.-Ing\.\(FH\)\b", r"Hochschulstudium", r"Hochschulabschluss"],
    
    "Master": [r"\bMaster\b", r"\bM\.Sc\.\b", r"\bMSc\b", r"\bM\.A\.\b", r"\bMA\b", r"\bM\.Eng\.\b", r"\bMEng\b", r"\bMBA\b", r"\bMaster of Science\b", r"\bMaster of Arts\b", r"\bMaster of Engineering\b", r"\bMaster of Business Administration\b", r"\bMagister\b", r"\bDiplom\b", r"\bDipl\.-Ing\.\b"],
    
    "PhD": [r"\bPhD\b", r"\bDoctorate\b", r"\bDr\.\b", r"\bDr\.-Ing\.\b", r"\bDr\. rer\. nat\.\b", r"\bDr\. phil\.\b", r"\bDoktor\b", r"\bDoktortitel\b", r"\bHabilitation\b", r"\bPostdoc\b", r"\bPost Doc\b"],
    
    "Vocational": [r"\bAusbildung\b", r"\bBerufsausbildung\b", r"\bTechniker\b", r"\bMeister\b", r"\bAssociate Degree\b", r"\bAdvanced Diploma\b", r"\bInformatikausbildung\b", r"Höhere Berufsbildung"  ]
}

def extract_keywords(job, description_text):
    
    # Find all matching keywords
    found_keywords = sorted({kw for kw in cs_keywords if re.search(rf"\b{kw}\b", description_text, re.IGNORECASE)})


    job["cs_keywords"] = found_keywords
    
def extract_experience(job, description_text):
    years = set()  # Use set to store unique years

    experience_patterns = [
        r"(\d+)\+?\s*(?:years?|Jahre)",          # "5+ years", "3 Jahre"
        r"at least (\d+)\s*years?",              # "at least 3 years"
        r"mindestens (\d+)\s*Jahre?",            # "mindestens 3 Jahre"
        r"mehr als (\d+)\s*Jahre?",              # "mehr als 2 Jahre"
        r"more than (\d+)\s*years?",             # "more than 5 years"
        r"(\d+)-(\d+)\s*(?:years?|Jahre)"        # "3-5 years", "3-5 Jahre"
    ]
    

    if any(synonym in description_text.lower() for synonym in ["mehrjährig", "langjährig", "viele jahre", "mehr als", "many years"]):
        years.add(2)  

    for pattern in experience_patterns:
        matches = re.findall(pattern, description_text)
        for match in matches:
            if isinstance(match, tuple):  
                for num in match:
                    if num.isdigit():
                        years.add(int(num))
            elif match.isdigit():  
                years.add(int(match))
    
    job["years"] = sorted(years)  

def extract_job_name(job):
    job_title = job["job_title"].lower()

    job_title_cleaned = None
    for cs_job in cs_jobs:
        if cs_job.lower() in job_title:
            job_title_cleaned = cs_job
            
    job["job_title_cleaned"] = job_title_cleaned
    

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

jobs_processed = []
for i, job in enumerate(jobs):
    print(f"Preprocessing {i}/{len(jobs)}\n")
    descriptions_text = " ".join(
        " ".join(desc_list)
        for desc_dict in job["descriptions"]
        for desc_list in desc_dict.values()
    )
    if len(descriptions_text) > 0:
        
        extract_keywords(job, descriptions_text)
        extract_experience(job, descriptions_text)
        extract_job_name(job)
        extract_career_stage(job)
        extract_education_stage(job, descriptions_text)
        
        jobs_processed.append(job)


with open(parent_dir+'/data/jobs_processed.json', "w", encoding="utf-8") as file:
    json.dump(jobs_processed, file, indent=4, ensure_ascii=False)
        
    