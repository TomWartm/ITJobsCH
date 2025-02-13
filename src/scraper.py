from selenium import webdriver
from selenium.webdriver.chrome.options import Options
import json
import os
import sys

sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), "..")))

from helpers.jobs import get_job_urls as get_job_urls_jobs_ch, scrape_website as scrape_website_jobs_ch
from helpers.itjobs import get_job_urls as get_job_urls_itjobs_ch, scrape_website as scrape_website_itjobs_ch


def scrape_jobs_ch(driver: webdriver, jobs_path: str) -> None:
    """
    Scrapes jobs from Jobs.ch and updates the local jobs JSON file.

    Args:
        driver (webdriver): The Selenium WebDriver instance used to interact with the website.
        jobs_path (str): The file path to the JSON file where job data is stored.
    Raises:
        json.decoder.JSONDecodeError: If the JSON file is empty or malformed.
    """
    
    try:
        with open(jobs_path, 'r') as file:
            jobs = json.load(file)
    except json.decoder.JSONDecodeError:
        print("Failed to load JSON, the file might be empty or malformed.")
        jobs = []
        
    existing_urls = [item['url'] for item in jobs if "url" in item]
    search_queries = ["software engineer", "data engineer", "data scientist"]
    for search_query in search_queries:
        new_job_urls = get_job_urls_jobs_ch(search_query, driver)
        
        with open(jobs_path, 'w') as file:
            for new_url in new_job_urls:
                if new_url not in existing_urls:
                    jobs.append({"url": new_url, "downloaded": False, "search_query": search_query, "website": "Jobs"})
            json.dump(jobs, file, indent=4)
    
    # download data
    for i, job in enumerate(jobs):
        if job['downloaded'] == False:
            print(f"Downloading job {i+1}/{len(jobs)}, {job}")
            scrape_website_jobs_ch(job, driver)
            # store updated jobs
            with open(jobs_path, 'w') as file:
                json.dump(jobs, file, indent=4)



def scrape_itjobs_ch(driver: webdriver, jobs_path: str) -> None:
    """
    Scrapes jobs from ITJobs.ch and updates the local jobs JSON file.

    Args:
        driver (webdriver): The Selenium WebDriver instance used to interact with the website.
        jobs_path (str): The file path to the JSON file where job data is stored.
    Raises:
        json.decoder.JSONDecodeError: If the JSON file is empty or malformed.
    """
    
    try:
        with open(jobs_path, 'r') as file:
            jobs = json.load(file)
    except json.decoder.JSONDecodeError:
        print("Failed to load JSON, the file might be empty or malformed.")
        jobs = []  

    existing_urls = [item['url'] for item in jobs if "url" in item]
    new_job_urls = get_job_urls_itjobs_ch(driver)
        
    with open(jobs_path, 'w') as file:
        for new_url in new_job_urls:
            if new_url not in existing_urls:
                jobs.append({"url": new_url, "downloaded": False, "search_query": "all jobs", "website": "ITJobs"})
        json.dump(jobs, file, indent=4)
    
    
    for i,job in enumerate(jobs): 
        if job['downloaded'] == False:
            print(f"Downloading job {i+1}/{len(jobs)}, {job}")
            scrape_website_itjobs_ch(job, driver)
            # store updated jobs
            with open(jobs_path, 'w') as file:
                json.dump(jobs, file, indent=4)  
    

if __name__ == "__main__":
    # configure webdriver
    options = Options()
    options.headless = True 
    options.add_argument("--window-size=1920,1080")
    options.add_argument("start-maximized")
    options.add_argument("--disable-logging")
    driver = webdriver.Chrome(options=options)

    parent_dir = os.path.dirname(os.path.dirname(__file__))

    # get user input
    user_input = input("Do you want to jobs from jobs.ch? (yes/no): ").strip().lower()

    download_jobs_ch = False

    if user_input in ['yes', 'y']:
        download_jobs_ch = True
    elif user_input in ['no', 'n']:
        download_jobs_ch = False
    else:
        print("Invalid response. Please answer with 'yes', 'no', 'y', or 'n'.")


    user_input = input("Do you want to jobs from itjobs.ch? (yes/no): ").strip().lower()

    if user_input in ['yes', 'y']:
        download_itjobs_ch = True
    elif user_input in ['no', 'n']:
        download_itjobs_ch = False
    else:
        print("Invalid response. Please answer with 'yes', 'no', 'y', or 'n'.")

    # scrape websites
    if download_jobs_ch:
        scrape_jobs_ch(driver, jobs_path=parent_dir+'/data/jobs.json')
    if download_itjobs_ch:
        scrape_itjobs_ch(driver, jobs_path = parent_dir+'/data/jobs.json')

        
    driver.quit()