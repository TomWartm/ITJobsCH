import json
import os
import pandas as pd
class JobReviewer:
    def __init__(self, jobs_file='data/jobs.json'):
        self.jobs = []
        self.load_jobs(jobs_file)

    def load_jobs(self, jobs_file):
        try:
            with open(jobs_file, 'r') as file:
                self.jobs = json.load(file)
        except FileNotFoundError:
            print(f"File {jobs_file} not found.")
        except json.JSONDecodeError:
            print(f"Error decoding JSON from file {jobs_file}.")
    def save_jobs(self, jobs_file='data/jobs.json'):
        try:
            with open(jobs_file, 'w') as file:
                json.dump(self.jobs, file, indent=4)
        except Exception as e:
            print(f"An error occurred while saving jobs to {jobs_file}: {e}")
            
    def show_job(self, job):
        print("\n" + "-" * 50)
        attributes = ['job_title', 'company', 'publication_date', 'place_of_work',  'job_category', 'contract_type', 'programming_languages','years', 'workload', 'salary', 'descriptions']
        for attr in attributes:
            if attr in job:
                if attr == 'descriptions' and len(job[attr]) > 0:
                    print("Descriptions:")
                    for description in job[attr]:
                        for key, value in description.items():
                            print(f"  Section {key}:")
                            for item in value:
                                print(f"    - {item}")
                else:
                    print(f"{attr.replace('_', ' ').title()}: {job[attr]}")
                    
    def review_job(self, job) -> bool:
        while True:
            print("Please rate the job between 0 and 9 (or enter '.' to exit): ", end='', flush=True)
            rating = input()
            if rating == '.':
                return False
            if rating.isdigit() and 0 <= int(rating) <= 9:
                job['rating'] = int(rating)
                job['reviewed'] = True
                
                return True
            else:
                print("Invalid input. Please enter a number between 0 and 9 or '.' to exit.")
    def review_random_jobs(self):
        # iterate over jobs with clean job_title
        unreviewed_jobs = [job for job in self.jobs if ('reviewed' not in job or not job['reviewed']) and 'publication_date' in  job ]
        unreviewed_jobs.sort(key=lambda x: pd.to_datetime(x['publication_date']).timestamp(), reverse=True)
        print("Jobs to review: {}".format(len(unreviewed_jobs)))
        for unreviewed_job in unreviewed_jobs:
            self.show_job(unreviewed_job)
            do_continue =self.review_job(unreviewed_job)
            self.save_jobs()            
            if not do_continue:
                break
        else:
            print("No unreviewed jobs available.")

if __name__ == "__main__":
    parent_dir = os.path.dirname(os.path.dirname(__file__))
    job_rater = JobReviewer()
    job_rater.review_random_jobs()
    