# PhD thesis

Title: **Exploiting emerging DNA sequencing technologies to study genomic rearrangements**

(c) Sascha Meiers, 2017-2018

European Molecular Biology Laboratory

Licensed under [Creative Commons Attribution 4.0](https://creativecommons.org/licenses/by/4.0/)

The final version can be found under [Releases](https://github.com/meiers/thesis/releases/tag/printed)

## Acknowledgments

The latex source code is based on [this template](https://github.com/EBI-predocs/latex-thesis) from the EBI students and heavily influenced by [Konrad Rudolphs' thesis design](https://github.com/klmr/thesis). 

I used the open fonts *Linux Libertine*, *Linux Biolinum O*, and *Inconsolata*.

People who supported me during the *actual work* are acknoledged in more depth within the text.

## Requirements
I switched to using [ltx2any](https://github.com/reitzig/ltx2any) to compile the PDF document because it writes nicer log files than the previously used [latexmk](http://mg.readthedocs.io/latexmk.html). In fact the log files are now redered into HTML using [pandoc](https://pandoc.org/) and filtered beforehands via the [silence](https://ctan.org/pkg/silence) package. 

 * [ltx2any](https://github.com/reitzig/ltx2any), which requires ruby >= 2.3.
 * TeX Live installation
 * XeTeX engine (came with TeX Live)
 * [biber](http://biblatex-biber.sourceforge.net/) for bibliography management (also came with TeX Live)
 * make
 * [pandoc](https://pandoc.org/) to make an HTML out of the log file



