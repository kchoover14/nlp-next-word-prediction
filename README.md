## Next-Word Prediction Shiny App

This project builds a Shiny app that predicts the next word from user text input using a Stupid Backoff n-gram model trained on the SwiftKey corpus — a 70-million-word English-language collection of blogs, news, and tweets. Originally built in 2021 as part of the Johns Hopkins Data Science Specialization capstone, the app was revised in 2026 with improved accuracy testing, updated models, and a redesigned interface. Six statistical methods were implemented before Stupid Backoff was confirmed as the best-performing approach, achieving 50.8% top-10 accuracy and 56.7% part-of-speech (POS) accuracy on the 50% training sample.

## Portfolio Page

The [portfolio page](https://kchoover14.github.io/nlp-next-word-prediction) includes a full project narrative, model comparisons, accuracy results, and app screenshots.

## Live App

[Next Word Prediction App](https://kchoover14.shinyapps.io/NextWord/)

## Tools & Technologies

**Languages:** R

**Tools:** Shiny | shinyapps.io

**Packages:** shiny | shinycssloaders | bslib | stringr | dplyr | tidyr | data.table | tidytext | quanteda | lexicon | hunspell | viridis | udpipe

## Environment

- `renv.lock` and `renv/` -- restore with `renv::restore()`

## Expertise

Demonstrates ability to systematically explore and evaluate competing methodological approaches, document iterative development transparently, and deploy a working NLP application from a messy real-world corpus -- skills directly applicable to any data pipeline or applied ML project requiring both technical rigor and honest documentation of what worked and what did not.

## License

- Code and scripts © Kara C. Hoover, licensed under the [MIT License](LICENSE).
- Data, figures, and written content © Kara C. Hoover, licensed under [CC BY-NC-SA 4.0](https://creativecommons.org/licenses/by-nc-sa/4.0/).