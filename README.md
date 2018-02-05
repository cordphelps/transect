



## how different are several multi-species transect samples? with some imaginary ecological data :
- demonstrate rank order correlations in two dimensions (dissimilarity plots)
- experiment with stressplot() 
- build typical NMDS (non-metric multi-dimensional scaling) graphics to demo clustering by factor 
- demo a distribution plot for multiple datasets



---

### the data schema : random data with some realistic relationships and a hidden correlation : 

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/hvbSchema.jpg)]()

--- 
### the 'random' ecological data: species ocurrances distributed along a transect that begins in the field margin and extends into the field 

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/quad.jpg)]()

---
### the typical dissimilarity plots; by species (suggesting which species distributions occuring in a transect are most similar/dissimilar)

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/dissimSpecies.jpg)]()

---
### and dissimilarity by transect position (suggesting which transect distributions, by gradient, of all species are most similar/dissimilar)
#### TODO: normalize the counts of individual species

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/dissimObs.jpg)]()

---
### the mystery stressplot():  

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/stressPlot.jpg)]()

---
### the 'ordination' plot: trying to visualize the magnitude of distribution variation in multi-dimensional space as a function of some factor. (if the ellipses appear parallel then the distributions of those factor observations are 'similar' (?) )

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/ordination.jpg)]()

---
### comparing multi-species transect samples

[![screen shot](https://raw.githubusercontent.com/cordphelps/transect/master/images/transectCompare.jpg)]()

---
####
read data with R:

```R
	> library(RCurl)
	> source.url <- c("https://raw.githubusercontent.com/cordphelps/transect/master/data/hvb.csv")
	> dataframe.df <- read.csv(text=getURLContent(source.url), header=TRUE, row.names=1)
```
---
---
### Acknowledgements
[oliviarata.wordpress.com](https://oliviarata.wordpress.com/2014/04/17/ordinations-in-ggplot2/)


### License
[Creative Commons Attribution 4.0 International](https://creativecommons.org/licenses/by/4.0/)

Data and Content distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.


### Author
Cord Phelps // [github](http://cordphelps.github.io)








 





