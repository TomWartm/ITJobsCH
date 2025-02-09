from selenium import webdriver
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.common.action_chains import ActionChains
import time
import json

import os
import sys
sys.path.append("..") # Adds higher directory to python modules path.


def get_job_urls(driver):
    job_urls = []
    url = "https://www.itjobs.ch/jobs/"
    

    driver.get(url)
    driver.execute_script("document.body.style.zoom='20%'")


    while True:
        try:
            # Wait for the button to be visible and clickable
            time.sleep(2)
            driver.execute_script("window.scrollTo(0, document.body.scrollHeight);")
            button = driver.find_element(By.XPATH, "//a[contains(@class, 'btn') and contains(text(), 'Mehr laden')]")
            # Scroll to the button and click it
            button.click()
            print("Clicked 'Mehr laden' button")


            
        except NoSuchElementException as e:
            print("No more 'Mehr laden' button found or it's not clickable.", e)
            break  # Exit loop if button is no longer found




    jobs = driver.find_elements(By.XPATH, "//*[contains(@class, 'sc-AHaJN') and contains(@class, 'htmdha')]")
    print(len(jobs))
    for job in jobs:
        job_link = job.find_element(By.XPATH, ".//a").get_attribute("href")
        if len(job_link) > len("https://www.itjobs.ch/jobs/"):
            job_urls.append(job_link)
            # Printing job details

            print(f"Job link: {job_link}")
            
    
    return job_urls


def scrape_website(job, driver):
    
    driver.get(job['url'])
    driver.execute_script("document.body.style.zoom='50%'")
    time.sleep(2)
    try:
        job['place_of_work'] = driver.find_element(By.XPATH, "/html/body/div[5]/div/div[2]/div/div/div[2]/div[2]/section[1]/div[1]/p/span[1]").text
        
    except NoSuchElementException:
        print("Location not found, continuing...")
        
        
    
    try:
        job['publication_date'] = driver.find_element(By.XPATH, "/html/body/div[5]/div/div[2]/div/div/div[2]/div[2]/section[1]/div[1]/p/span[2]").text

    except NoSuchElementException:
        print("Publication Date not found, continuing...")
        
        
    try:
        job['company'] = driver.find_element(By.XPATH, "/html/body/div[5]/div/div[2]/div/div/div[2]/div[2]/section[1]/div[1]/h2").text

    except NoSuchElementException:
        print("Company not found, continuing...")
        
    try:
        job['job_title'] = driver.find_element(By.XPATH, "/html/body/div[5]/div/div[2]/div/div/div[2]/div[1]/h1").text.replace('\nOriginalinserat', '')

    except NoSuchElementException:
        print("Job Title not found, continuing...")
    
    job['descriptions'] = []
    
    try:

        iframe = driver.find_element(By.TAG_NAME, 'iframe')  # You can also use other methods like ID, class, etc.
        driver.switch_to.frame(iframe)
        li_elements = driver.find_elements(By.TAG_NAME, 'li')

        # Get the text from each <li> element
        li_texts = [li.text for li in li_elements]
        job['descriptions'].append({1:li_texts})
        
        
        # TODO: only works for iframe
            
            
    except NoSuchElementException as e:
        print("Job Description not found, continuing...", e)
    
    
    
        
    job["downloaded"] = True
    

    

if __name__ == "__main__":
    # configure webdriver
    options = Options()
    options.headless = True  # hide GUI
    options.add_argument("--window-size=1220,840")  # set window size to native GUI size
    options.add_argument("start-maximized")  # ensure window is full-screen
    options.add_argument("--disable-logging") # Suppress logs related to performance metrics
    driver = webdriver.Chrome(options=options)
    
    parent_dir = os.path.dirname(os.path.dirname(__file__))
    jobs_cache_path = parent_dir+'/data/jobs2.json'
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

        new_job_urls = get_job_urls(driver)
        
        with open(jobs_cache_path, 'w') as file:
            for new_url in new_job_urls:
                if new_url not in existing_urls:
                    jobs.append({"url": new_url, "downloaded": False, "search_query": "all jobs", "website": "ITJobs"})
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