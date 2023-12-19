# Pluto R package

<img src="https://cdn.bfldr.com/2Q1IPX6I/at/hqwgpb3vqscf83j375mfn9gf/Integrations_-_R" height="500">

## Overview

`pluto` is the official R package for interacting with <a href="https://pluto.bio" class="pluto-link">Pluto</a>, the biological discovery platform. With a few lines of code, you can read data and results directly into your R scripts, and push custom plots back to Pluto where they remain interactive and collaborative.

Not using Pluto yet? We'd be happy to show you the platform in action - <a href="https://pluto.bio/get-info" class="pluto-link">schedule a personalized demo</a>.

## Install `pluto`

Install the latest version from Github:

```
# Install devtools if you don't already have it
install.packages("devtools")

# Install the pluto R package
remotes::install_github("pluto-biosciences/pluto-sdk-r")
```

Once installed, you can load the Pluto R package into your scripts with `library(pluto)`.

## Getting started

A typical, minimal workflow using `pluto` involves:

1. Authenticate with Pluto API token (see `vignette("authentication")`)
2. Fetch data from one or more experiments
3. Perform some custom analysis (see `vignette("rnaseq_recipes")` for inspiration :sparkles:)
4. Push your results back into the Pluto canvas

There are also numerous ways to use `pluto` at a larger scale, for example with multiple data sets or integrating with your in-house tools. A few examples include:

* Fetching all RNA-seq experiments in a project and performing a meta-analysis of their differential expression results
* Fetching four different NGS experiments (e.g. RNA-seq, ChIP-seq, CUT&RUN, and ATAC-seq) and overlapping differential regions to identify regions where both expression and binding is altered

## Working with an outsourced bioinformatics team?

With Pluto, your cross-functional team has the power to collaborate easily in one place regardless of coding ability. The same applies to your vendors as well! 

Instead of having your outsourced bioinformatics vendor send you results by email or in folders where critical findings are easily lost, **you can request that they deliver data and results directly back into your Pluto lab space**. No training necessary on your part - Pluto handles all onboarding and training for bioinformatics CROs collaborating with clients in Pluto as a part of the service. So you can sit back and watch the results come to you. Chat with our Customer Experience team to learn more. 

## Questions?

We're here to help! Feel free to reach out to <a href="mailto:support@pluto.bio" class="pluto-link">support@pluto.bio</a> or email your Pluto customer representative directly.


