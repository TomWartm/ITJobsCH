from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException
import time
import json

import os
import sys
sys.path.append("..") # Adds higher directory to python modules path.


def get_job_urls(search_term, driver):
    job_urls = []
    url = f"https://www.jobs.ch/en/vacancies/?term={search_term.replace(' ', '%20')}"
    

    driver.get(url)
    
    # get number of pages
    pagination_div = driver.find_element(By.CSS_SELECTOR, 'div.d_flex.ai_center.gap_s4')
    pagination_links = pagination_div.find_elements(By.TAG_NAME, "a")
    if not pagination_links[-2]:
        raise ValueError("less than 2 tabs")
    last_page_nr = pagination_links[-2].get_attribute("data-cy").split("-")[1]
    
    
    for i in range(1, int(last_page_nr)+2):
        url = f"https://www.jobs.ch/en/vacancies/?page={i}&term={search_term.replace(' ', '%20')}"
        time.sleep(2)
        driver.get(url)
        searched_jobs = driver.find_elements(By.CSS_SELECTOR, 'div[data-feat="searched_jobs"]')
    
            
        for job_preview in searched_jobs:
            job_website_link = job_preview.find_element(By.TAG_NAME, 'article').find_element(By.CSS_SELECTOR, 'a[data-cy="job-link"]').get_attribute("href")
            job_urls.append(job_website_link)
        
        
    
    return job_urls


def scrape_website(job, driver):
    
    driver.get(job['url'])
    time.sleep(2)
    try:
        key_information_element = driver.find_element(By.CSS_SELECTOR, '[data-cy="vacancy-info"]')

        for child in key_information_element.find_elements(By.TAG_NAME, 'li'):
            content = child.text.split(":")
            if len(content) >= 2:
                job["_".join(content[0].split(" ")).lower()] = content[1]
    except NoSuchElementException:
        print("Key information not found, continuing...")
    
    try:
        
        job_title_element = driver.find_element(By.CSS_SELECTOR, '[data-cy="vacancy-title"]')
        job["job_title"] = job_title_element.text
    except NoSuchElementException:
        print("Job title not found, continuing...")
    
    try:
        company_element = driver.find_element(By.CSS_SELECTOR, '[data-cy="company-link"]')
        if company_element:
            job["company"] = company_element.text
    except NoSuchElementException:
        print("Company link not found, continuing...")
    
    
    
    
    try:
        job["descriptions"] = []
        vacancy_description_div = driver.find_element(By.CSS_SELECTOR, '[data-cy="vacancy-description"]')

        # Extract sections: "Was dich erwartet", "Was du mitbringst", and "Was wir dir bieten"
        ul_elements = vacancy_description_div.find_elements(By.CLASS_NAME, "li-t_disc")
        for i, ul_element in enumerate(ul_elements):
            li_elements = ul_element.find_elements(By.TAG_NAME, "li")

            li_texts = [li.text for li in li_elements]

            job["descriptions"].append({i:li_texts})
    except NoSuchElementException:
        print("Vacancy description not found, continuing...")
        
        
    job["downloaded"] = True
    

    

if __name__ == "__main__":
    # configure webdriver
    options = Options()
    options.headless = True  # hide GUI
    options.add_argument("--window-size=1920,1080")  # set window size to native GUI size
    options.add_argument("start-maximized")  # ensure window is full-screen
    options.add_argument("--disable-logging") # Suppress logs related to performance metrics
    driver = webdriver.Chrome(options=options)
    
    parent_dir = os.path.dirname(os.path.dirname(__file__))
    jobs_cache_path = parent_dir+'/data/jobs.json'
    # get user input
    # Ask the user if they want to download the websites again
    user_input = input("Do you want to download the website links again? (yes/no): ").strip().lower()

    download_again = False
    # Check for different accepted responses
    if user_input in ['yes', 'y']:
        download_again = True
    elif user_input in ['no', 'n']:
        download_again = False
    else:
        print("Invalid response. Please answer with 'yes', 'no', 'y', or 'n'.")
    
    try:
        with open(jobs_cache_path, 'r') as file:
            jobs = json.load(file)
    except json.decoder.JSONDecodeError:
        print("Failed to load JSON, the file might be empty or malformed.")
        jobs = []  
    
    existing_urls = [item['url'] for item in jobs if "url" in item]
    if download_again:
        search_queries = ["software engineer", "data engineer", "data scientist"]
        for search_query in search_queries:
            new_job_urls = get_job_urls(search_query, driver)
            
            with open(jobs_cache_path, 'w') as file:
                for new_url in new_job_urls:
                    if new_url not in existing_urls:
                        jobs.append({"url": new_url, "downloaded": False, "search_query": search_query, "website": "Jobs"})
                json.dump(jobs, file, indent=4)
        
    # download data
    for i,job in enumerate(jobs):
        
        if job['downloaded'] == False:
            print(f"Downloading job {i+1}/{len(jobs)}, {job}")
            scrape_website(job, driver)
            # store updated jobs
            with open(jobs_cache_path, 'w') as file:
                json.dump(jobs, file, indent=4)
        
    
    driver.quit()