library(cgdsr)

# create connection to cancer genome data server
mycgds = CGDS('https://www.cbioportal.org/')
test(mycgds)

# get list of cancer studies at server and select one study
studies = getCancerStudies(mycgds)
mystudy = studies[2,1]

# get case list
mycaselist = getCaseLists(mycgds, mystudy)
mutationCaseList = mycaselist[6,1]

# get genetic profiles
mygeneticprofile = getGeneticProfiles(mycgds, mystudy)
mutationProfile = mygeneticprofile[4,1]

# get data slices for a specified list of genes, mutation profile, and caseList
mutationData = getProfileData(mycgds, c('BRCA1', 'BRCA2'), mutationProfile, mutationCaseList)

# get clinical data for the caselist
clinicalData = getClinicalData(mycgds, mutationCaseList)
