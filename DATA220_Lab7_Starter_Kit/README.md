# DATA 220 · Labs 7–8 · Team repository

One Git repository per team, used for Labs 7 and 8 and then the final project. Inside it are
two RStudio projects: open `weather/weather.Rproj` or `project/project.Rproj`, never the
repository folder itself, so that each project's scripts find their files.

| Folder / file | What it is |
|---|---|
| `weather/` | The warm-up: a finished pipeline on classroom weather and trail-counter data (W1–W4). |
| `project/` | YOUR project's data: sources, samples, profiles, table plan, first scripts (P1–P6). |
| `R/` (inside each) | The toolkit: 52 functions, each with its purpose, a worked example and how its code works. |
| `DATA220_Project_Toolkit_Reference.pdf` | The toolkit as a document: find the function you need, project recipes, every function. |
| `SUBMISSION.md` | Team, members, who was at the lab, what each person did. |
| `check_lab7.R` | The self-check: an ESTIMATED score. The instructor reviews your work. |

## What goes into GitHub

Everything you write: scripts, `sql/`, `config.R`, your samples in `project/data/raw/`,
`SUBMISSION.md` and `check_lab7_result.txt`. Files the scripts rebuild (`data/interim/`,
`data/processed/`, `data/db/`, `output/`, `logs/`) stay out -- see `.gitignore`. The zip you
submit is GitHub's own "Code > Download ZIP" of the repository.

The weather and trail-counter files are classroom data made for teaching (the weather files
follow the layout of Environment and Climate Change Canada daily exports). They are not
official observations.
