# Datasets for Zcalc

Instructors often want to provide student access to datafiles. For *MOSAIC Calculus*, there are two modes in which students can execute calculations:

1. Using the **Sandbox**, either via the web or on a local machine.
2. Using the R console, particularly in conjunction with the RStudio IDE.

To support both these modes, the `{Znotes}` package provides a special-purpose function that will read the data in to either the Sandbox or the current R session.

```{r eval=FALSE}
mydata <- Zdata("141/todays_data")
```

## Making your data available to `Zdata()`

As an instructor who wants to make data available, here's what to do.

1. Collect your data, formatting it as a CSV file. (We may add other file formats later.)
2. Remember that the data will be **available to the public**. If your data is confidential, turn back and find another way to provide access to your students. 
2. Go to the repository at <https://github.com/dtkaplan/Znotes>
3. Navigate to the `docs/` directory.

CREATE A UNIQUE ID FOR YOUR Data and make a directory (under `docs/`) named with that ID. Be nice, and put your own data in your own directory (under docs).

Go to your data's directory, the one named with the ID you created. Then, upload your data file, which will have a name like `[myname].csv`. We also encourage you to upload a similarly named documentation file, which can be in plain text or (if you know what the following mean) `{roxygen2}` or `.rd` format.

Press the ... to generate what's called a "pull request." In the text box, give a brief explanation of where the data came from. Your data will appear when the Znote package moderators have approved the pull request.

If you are an invited instructor, you can simply "commit" your new data to the site. That way you don't have to wait for approval.

7. Give your students the command to read in your data, typically something like 
```r
fancydat <- Zdata("macalester/fancydat")
```

The site moderators reserve the right to deny approval or remove any files which are deemed to be harmful to the site or the students using the data or anyone else. We have found that instructors are pretty responsible in this regard, but who knows what will happen in the future!

THE SOFTWARE AND DATA SERVICE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Make sure to check that your data is available via the `Zdata()` function before you have students work with it. 
