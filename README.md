# Job Market Insights
This is a script that scrapes [jobs.ch](https://www.jobs.ch/en/) and [itjobs.ch](https://www.itjobs.ch/jobs/), based on keywords related to IT, to summarize, visualize, and filter interesting jobs.

**Last Data Downloaded:** 26 February, 2025

![Plot of Publication Date Distribution](src/analysis_files/figure-gfm/time-jobs-1.png)
* A line plot showing the daily count of publications since 1.1.2025.

For more analysis, see [analysis.md](src/analysis.md)


## ** Instructions**
### 1. Create and Activate the Conda Environment
Create a Conda environment called `itjobs` from the `environment.yml` file and install the required dependencies.

```bash
conda env create -f environment.yml
conda activate itjobs
```
### 2. Run the Scraper
If you want to download the latest data from jobs.ch and/or itjobs.ch run the [scraper](src/scraper.py).

Alternatively, you can skip this step and use the already downloaded data in `data/jobs.json`
```bash
python src/scraper.py
```

### 3. Preprocess the Data
To clean and extract important information from the raw file jobs.json, run the [preprocessing](src/preprocessing.py) script, which will create the `jobs_processed.json` file in the data folder.

Alternatively, you can skip this step and use the already preprocessed data in `data/jobs_preprocessed.json`
```bash
python src/preprocessing.py
```
Warning: if you change `preprocessing.py`, you should delete `jobs_preprocessed.json` and run it again. Howeer, you will loose your ratings on the jobs.

### 4. Review jobs
You can review the downloaded jobs by running [review](src/review.py). This each downloaded job (that is not rated so far) and the user can rate the job from 0-9.
```bash
python src/review.py
```


