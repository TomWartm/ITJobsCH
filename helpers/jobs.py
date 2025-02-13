import time
from selenium.webdriver.common.by import By
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.remote.webdriver import WebDriver
from selenium import webdriver
from typing import Dict, List, Union

def get_job_urls(search_term: str, driver: webdriver) -> List[str]:
    """
    Retrieves all job URLs from jobs.ch based on the search term.
    Args:
        search_term (str): The term to search for job vacancies.
        driver (webdriver): The Selenium WebDriver instance used to interact with the web page.
    Returns:
        List[str]: A list of URLs for all the job vacancies matching the search term.
    Raises:
        ValueError: If there are less than 2 pages of job listings.
    """
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


def scrape_website(job: Dict[str, Union[str, List[str]]], driver: webdriver) -> None:
    """
    Scrapes job information from a job posting on jobs.ch using a Selenium WebDriver.
    Args:
        job (Dict[str, Union[str, List[str]]]): A dictionary containing job information, including the URL to scrape.
        driver (WebDriver): A Selenium WebDriver instance used to navigate and scrape the website.
    Updates the job dictionary with the following keys:
        - key_information: Various key information extracted from the job listing.
        - job_title: The title of the job.
        - company: The company offering the job.
        - descriptions: A list of dictionaries containing sections of the job description.
        - downloaded: A boolean indicating whether the job information was successfully downloaded.
    """
    
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