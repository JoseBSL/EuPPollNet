[![DOI](https://zenodo.org/badge/000000000.svg)](https://zenodo.org/badge/latestdoi/000000000)

# Dynamic Punk Papers

This repo is a generic template to create Dynamic Papers. 

# Why?  

We believe that the scientific publishing system is outdated. Science is a continuous process. Publishing should not be a goal, but a mean to showcase snapshots of this process. A way to achieve that change is self-publishing dynamic documents that track how our knowledge change with time. This repo helps building such experience.

# How? 

This repo contains a couple of perks. First, a webpage build with `Quarto` to explain the scientific process. Second, a set of scripts to update the results periodically (e.g. as more data is available) using `github actions`. All versions can be stored to Zenodo so changes over time can be tracked.  

Once cloned, you should edit this `ReadMe.md` file, as well as add content to the `.qmd` webpages. Add or remove `.qmd` as you need. The webpage structure is specified in `_quarto.yml` file. You need to update this with your data. We already added a `.nojekyll` empty file in the root and add to `.gitignore` `/.quarto/` and `/_site/` as needed. We also added a MIT `LICENCE` file (which seems punk enough) and a  `News.md` you can use if you like. Finally, you need to edit the `/.github/workflows/publish.yml` github action to set when and how the document should be updated. We scheduled the github action to ran once a month using `cron` but it can be triggered manually from the webpage. Everything is licenced with a MIT licence (`LICENCE`), 
     
A few actions should be done to set up the repo. First, you need to use `Renv` package to ensure consistency in package versions (using `init()` to set up the R dependency management and `snapshot()` to update it). Second, to publish the webpage you need to create a gh-pages branch for deployment (`quarto publish gh-pages` in terminal; details [here](https://quarto.org/docs/publishing/github-pages.html)) and connect it to github (on github `Settings/Pages/Deploy from branch/gh-pages/(root)`). 

# Notes
If actions are triggered `on: push: branches: main` (they are not right now) you can skip continuous integration in the commit message by adding `[skip ci]`   