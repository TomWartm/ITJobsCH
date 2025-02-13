import time
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException
import pandas as pd
from selenium import webdriver
from typing import Dict, List, Union

def get_job_urls(driver: webdriver) -> List[str]:
    """
    Retrieve all job URLs from the ITJobs website.
    This function navigates to the ITJobs website, continuously clicks the "Mehr laden" 
    button to load more job listings, and then extracts the URLs of the all job postings.
    Args:
        driver (webdriver): The Selenium WebDriver instance used to interact with the web page.
    Returns:
        list: A list of job URLs extracted from the ITJobs website.
    """
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
    for job in jobs:
        job_link = job.find_element(By.XPATH, ".//a").get_attribute("href")
        if len(job_link) > len("https://www.itjobs.ch/jobs/"):
            job_urls.append(job_link)
            
    return job_urls


def scrape_website(job: Dict[str, Union[str, List[str]]], driver: webdriver) -> None:
    """
    Scrapes job details from a itjobs.ch job posting website using a Selenium WebDriver.
    Args:
        job (Dict[str, Union[str, List[str]]]): A dictionary containing job details, including the URL to scrape.
        driver (webdriver): A Selenium WebDriver instance used to interact with the web page.
    Updates the job dictionary with the following keys:
        - 'place_of_work': The location of the job.
        - 'publication_date': The publication date of the job posting, formatted as "dd Month YYYY".
        - 'company': The name of the company offering the job.
        - 'job_title': The title of the job.
        - 'descriptions': A list containing job descriptions found within an iframe.
        - 'downloaded': A boolean flag set to True indicating that the job details have been successfully scraped.
    Prints messages to the console if certain elements are not found on the web page.
    Raises:
        NoSuchElementException: If specific elements are not found on the web page.
    """
    
    driver.get(job['url'])
    driver.execute_script("document.body.style.zoom='50%'")
    time.sleep(2)
    try:
        job['place_of_work'] = driver.find_element(By.XPATH, "/html/body/div[5]/div/div[2]/div/div/div[2]/div[2]/section[1]/div[1]/p/span[1]").text
        
    except NoSuchElementException:
        print("Location not found, continuing...")
        
        
    try:
        raw_str = driver.find_element(By.XPATH, "/html/body/div[5]/div/div[2]/div/div/div[2]/div[2]/section[1]/div[1]/p/span[2]").text
        date_obj = pd.to_datetime(raw_str, format="%d.%m.%Y")
        job['publication_date'] = date_obj.strftime("%d %B %Y")
        
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
        li_texts = [li.text for li in li_elements if len(li.text) > 0]
        job['descriptions'].append({1:li_texts})
        
        
        # TODO: only works for iframe
            
            
    except NoSuchElementException as e:
        print("Job Description not found, continuing...", e)
        
    job["downloaded"] = True