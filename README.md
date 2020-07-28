# IOM-Projects

# Notes

Google Trends provides an index of the volume of Google queries by geographic location and category.
Google Trends data does not report the raw level of queries for a given search term. Rather, it reports
a query index. The query index starts with the query share: the total query volume for search term in
a given geographic region divided by the total number of queries in that region at a point in time. The
query share numbers are then normalized so that they start at 0 in January 1, 2004. Numbers at later
dates indicated the percentage deviation from the query share on January 1, 2004.
https://static.googleusercontent.com/media/www.google.com/en//googleblogs/pdfs/google_predicting_the_present.pdf 

# How to PUSH and PULL to GITHUB
this is an example for our collaborative README.file for IOM-Projects. 
steps to push to Github, first set up Github, then RStudio


How to push/pull github repository to your computer via Rstudio
# step 1:
check if you have github set up. You can do this by typing 'git' in your shell. If so, they you are ready to get started
# step 2: 
start by creating a new project. file> new project> version control. press git, and copy and paste our repository URL which is https://github.com/imgesucet/IOM-Projects.git
# step 3:
after you have set your working directory you are able to view our current files on our repository. This includes, Rmd, README files, etc.

# step 4: push and pull

to already pull what we already have in the repository you must use the pull button on the top right hand of the console where it says 'Git'. this will update to what we have pushed into our repository. To push new files you must save the new work that you have updated, and click on 'Commit' which is located on the top legend under the 'Git'. 
after leaving a comment of what you have changed you must press

to already pull what we already have in the repository you must use the pull button on the top right hand of the console where it says 'Git'. this will update to what we have pushed into our repository. To push new files you must save the new work that you have updated, and click on 'Commit' which is located on the top legend under the 'Git'. after leaving a comment of what you have changed you must press 'commit' and it will upload to Github.

hope this helps!

# Resources
website that forecast google trends using prophet package
https://www.christopheryee.org/blog/mining-google-trends-data-with-r-featuring-gtrendsr/

