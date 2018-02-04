



### how different are two samples? : examine rank order correlation via non-metric multi-dimensional scaling tools in 

### test drive ggplot() to build the typical NMDS graphics from some imaginary ecological data 

### the data schema : random data with some realistic relationships and a hidden correlation : 

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/hvbSchema.jpg)]()

### the "random" ecological data: species ocurrances distributed along a transect that begins in the field margin and extends into the field 

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/quad.jpg)]()

### the typical dissimilarity plots; by species 

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/dissimSpecies.jpg)]()

### and dissimilarity by transect position

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/dissimObs.jpg)]()

### the mystery plot:  

### the 'ordination' plot: trying to visualize 


####
read data with R:

```R
	> library(RCurl)
	> source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb.csv")
	> dataframe.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)
```


### License
[Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/)

Data and Content distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.


### Author
Cord Phelps // [github](http://cordphelps.github.io)








 





