## StravaR

StravaR is a Shiny app allowing you to explore your fitness data within R with all the flexibility you need!

![screenshot of home page](github_resources/calendar.png)

It maintains a local database of all your activities so you not only get full ownership of your data in an easy to use format, but you can analyze it to your heart's content outside of the Shiny app.

### Features

I've written the app to provide the numbers & plots that I most want to see from my running data and that I can't find easily on the Strava website.
You'll likely have different use-cases so feel free to modify this code - I'd be interested to see what interesting visualisations people make with their fitness data!

**NB: the app works with any type of activity (running, cycling, walking, etc...), I just talk about running as it's my main sport.**
In all of the tabs shown below, you can select which sports to be shown from the checkboxes on the left.
Your default sport can be changed through the settings cog in the top right.

I like to compare my year's cumulative distance with previous years so I know if I'm roughly running at my usual amount, although my number 1 training target is my weekly mileage so I also plot this (as a rolling average to smooth it out).

![screenshot of mileage](github_resources/mileage.png)

While I don't use it a huge amount, I like being aware of my heart-rate stress score based training levels, as I do tend to find that the Optimal phase tends to correlate with my peak training weeks.

![screenshot of training page](github_resources/training.png)

Finally, when I haven't got a specific race planned, my main motivation comes from exploring new places so I want to be able to quickly explore all my running routes on an interactive map.

![screenshot of routes](github_resources/routes.png)

### Installation

1.  Clone this repository to your computer (within RStudio, File -\> New Project -\> Version Control -\> Git, then enter `https://github.com/stulacy/fit-viz.git` as the Repository URL)
2.  Install any missing packages that are loaded in `server.R`. RStudio should highlight these.
3.  Download an archive of all your Strava data, which is found in the (deliberately?) confusingly named page [Delete Your Account (NB: this link WILL NOT delete your account!)](https://www.strava.com/athlete/delete_your_account). If you don't want to click that scary link, in a web browser click your Profile Pic in the top right, Settings, My Account, scroll down to Download or Delete Your Account and click Get Started. Your archive will be emailed to you shortly
4.  Open either of `server.R`, `ui.R`, or `app.R` in RStudio and click the down arrow next to Run App button in the top right of the editor and select 'Run External'
5.  Now Run App and it should redirect you to it open in a web browser where you will be prompted to enter some details about yourself and upload the archive you downloaded in Step 3. This can take a while, it takes just under an hour to upload 1,100 activities on my laptop.

### Setup API access to Strava

Once you've uploaded your first data archive, you can upload more activities in the future by repeating the export process and uploading the archive through clicking the Refresh button in the top right.
However, this is a tedious manual process, particularly if we only want to upload 4 activities from the last week.

If you don't mind a bit of setup, you can get your data instantly synced with your Strava account through their API.
You'll need to create an App through [Strava's developer scheme](https://www.strava.com/settings/api) and substitute the Client ID and Secret in the line `STRAVA_APP <- oauth_app(...)` in `server.R` as the `key` and `secret` arguments respectively.
Then you are good to go by clicking the Connect With Strava button in the top right!

**Why can't I just use your key and secret?**
Because the `oauth 2.0` authentication method used by the Strava API isn't ideal for Desktop Apps (as in those that get installed to each users' computer, rather than running a single instance in the Cloud), as there's no way of installing the app without making the key and secret accessible to the user.
These values are considered sensitive because anyone could use them to make you think you were authorising your Strava account to connect to my app, but in reality they get access to your data.
I didn't want to write a Cloud based app as I'd rather everyone be able to own and manage their own data, rather than hand it off to yet another third party!

If you have any questions or problems, please create an Issue in this repo.
