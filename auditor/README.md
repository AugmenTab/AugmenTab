# Auditor

This tool iterates over all of the GitHub repos for which I am a contributor - be they private or public, whether I own them or not - and determines the insertions and deletions on commits where I am an author or co-author, for all languages.

My motivation to build this project is to create a more honest representation of where my work lies as a developer. I've used various badges and cards showing my "most-used languages," but none of them are totally accurate since they typically only look at my public repositories. I spend nearly all of my time working in private repositories in organizations, so this cannot capture the bulk of my contributions.

So instead, this looks at every commit I've ever made and finds the sum total of insertions and deletions on a per-language basis. This will run on a nightly basis to keep the information reasonably up-to-date. Using this data, I intend to present more accurate data on myself as a developer, for whatever interested parties might be seeking it out.

To-Do:
- Use commit metrics to make a stats card to reference in the repo `README`.
- Introduce other cards to promote projects.
- Write script to run this job every night and commit/push the results at 2 am (local time).
- Figure out a way to persist data so only the last 24 hours of commits have to be read.
- Make a more targeted list of languages to restrict the Linguist list. This will hopefully help prevent weird languages being presented in the commit records, like Objective-C or GCC Machine Description.

Some improvement ideas:
- Write an example script for the cron job.
- If it ever sees any use, consider adding themes and arguments to fine-tune the presentation of and data on the card.
