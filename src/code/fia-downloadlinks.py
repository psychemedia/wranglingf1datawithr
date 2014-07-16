#We need to identify the race for which we want to download the timing documents
url='http://www.fia.com/championship/fia-formula-1-world-championship/2013/2013-brazilian-grand-prix-event-information'
#Note: we could scrape these URLs from the year archive page for a
## more complete bulk downloader

#Running a simple scraper returns nothing from the FIA website...
#...which means we need to pretend to be a browser user agent...
import mechanize,re,urllib2
from time import sleep
br = mechanize.Browser()
br.addheaders = [('User-agent', 'Mozilla/5.0 (X11; U; Linux i686; en-US; rv:1.9.0.1) Gecko/2008071615 Fedora/3.0.1-1.fc9 Firefox/3.0.1')]

#Load the race archive page
br.open(url)


filetypes=[".pdf"]
myfiles=[]

links=re.findall(r'\'http[^\']+', str(br.response().read())) 
f=open('doclinks.txt','w')
for l in links:
	l=l.strip("'")
	for t in filetypes:
		if t in str(l):
			if l not in myfiles:
				myfiles.append(l)
f.write("\n".join(myfiles))				
f.close()