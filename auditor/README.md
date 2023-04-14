# Auditor

This tool iterates over all of the GitHub repos for which I am a contributor - be they private or public, whether I own them or not - and determines the insertions and deletions on commits where I am an author or co-author, for all languages.

My motivation to build this project is to create a more honest representation of where my work lies as a developer. I've used various badges and cards showing my "most-used languages," but none of them are totally accurate since they typically only look at my public repositories. I spend nearly all of my time working in private repositories in organizations, so this cannot capture the bulk of my contributions.

So instead, this looks at every commit I've ever made and finds the sum total of insertions and deletions on a per-language basis. This will run on a nightly basis to keep the information reasonably up-to-date. Using this data, I intend to present more accurate data on myself as a developer, for whatever interested parties might be seeking it out.

