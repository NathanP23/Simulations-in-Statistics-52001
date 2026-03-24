# Simulations-in-Statistics-52001

Content of the course [**"Simulations in Statistics (52001)"**](https://shnaton.huji.ac.il/index.php/NewSyl/52001/2/2026/) at The Hebrew University of Jerusalem, in the Department of Statistics and Data Science.

Taught by Prof. Benjamin Yakir.

## Running R Code in Jupyter Notebooks (.ipynb)

All lecture notebooks in this repo use **R as the kernel** inside `.ipynb` files. This setup requires a one-time installation of Jupyter and the R kernel (`IRkernel`).

### 1. Install Jupyter

**Option A - pip (any OS)**

On modern systems (macOS Homebrew Python, Linux distro Python) pip will refuse to install system-wide and suggest a venv. You can use a venv, but for a standalone tool like Jupyter it is simpler to install with `pipx` or allow the user-level install:

```bash
pip install --user jupyter
```

If your system blocks that too (externally managed environment), use Homebrew on macOS/Linux or `pipx` on any OS:

```bash
# macOS / Linux alternative via Homebrew
brew install jupyter

# any OS alternative via pipx
pipx install jupyter
```

**Windows**
```powershell
pip install jupyter
```

> A virtual environment (`python -m venv .venv`) is the "proper" approach for project isolation, but for a global tool like Jupyter it adds friction without much benefit - this repo skips it.

### 2. Install the R Kernel

Run the following inside a terminal (macOS/Linux) or the R console / RStudio (Windows):

```r
install.packages("IRkernel", repos = "https://cloud.r-project.org")
IRkernel::installspec()
```

Then restart VS Code. The kernel picker will show **R** as an option.

### 3. Open a Notebook

Open any `.ipynb` file in VS Code, click the kernel picker in the top-right corner, and select **R** (listed under `ir`).

### 4. Convert a Notebook back to R Markdown

[Quarto](https://quarto.org) handles the conversion. Install it from `quarto.org`, then run:

```bash
quarto convert path/to/notebook.ipynb
```

This produces a `.qmd` file. Rename it to `.Rmd` and replace the YAML header with standard R Markdown format:

```yaml
---
title: "Your Title"
output:
  html_document:
    toc: true
---
```

To render the `.Rmd` directly to HTML:

```bash
quarto render path/to/notebook.Rmd
```

---

## Official Syllabus

### Course/Module description:
The course presents statistical methods that are based on computerized simulations and applies these methods using the programming environment R.

The course is partitioned into 4 parts. Each part includes 10-13 per-recorded short lectures. A Graded Quiz is associated with each recorded lecture. At the end of the part, you are required to submit an Assignment. At the end of the course you will be required to submit a project in writting.

### Course Aims:
In the course we will learn different algorithms for computerized simulations. On the one hand, these algorithms will help us investigate the probabilistic properties of statistical procedures. On the other hand, we will present statistical procedures that rely on simulations for their implementation.

### Learning Outcome:
At the end of this course, students will be able to:

1. Write code in R that applies a statistical procedures or runs a computerized simulation.
2. To plan an apply an algorithm for the investigation the statistical properties of statistical procedures.
3. To program manually an algorithm that is applied in a R function and compare the outcome to the outcome obtained from the given function.
4. Identify the statistical procedure that fits analysis of a given data structure. Implement this procedure with R.
5. Characterize the statistical methods that are used in a given scientific investigation. Independently repeat these analyse and confirm them via simulations.

### Course Content:
1. Algorithms for simulation: random number generators, Monte Carlo, MCMC.
2. Procedures that are based on simulation: Bootstrap, Bayesian analysis based on MCMC, permutation tests, computation of the expectation using simulations (for example, in the E stap of the EM algorithm).
3. Validation of a statistical analysis: The investigation of power and robustness of ststistical procedures. Examples.

### Attendance requirements(%):
None

### Teaching Arrangement and Method: 
The course will be frontal in the classroom (a few lessons will be given through Zoom).
Attendance is mandatory.
The lessons will be recorded.

### Required Reading:
An Introduction to Statistical Computing (by Jochen Voss)

### Grading Scheme:
Essay / Project / Final Assignment / Referat 40 %
Submission assignments during the semester: Exercises / Essays / Audits / Reports / Forum / Simulation / others 60 %