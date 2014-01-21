#Need to load statnet and associated packages
library(statnet)

#To Load in the Matrices and Attributes for W1
smatrix1=as.matrix(read.csv('SEHC_W1MatrixNoZero.csv',header=FALSE))
satts1=read.csv('SEHC_W1_Atts.csv',header=TRUE,na.strings='0')

sdyad1=as.matrix(read.csv('SEHC_W1_DYAD-Sym.csv',header=FALSE))
#Check Attribute Collums names
colnames(satts1)

#Provide IDs for the matrix1
rownames(smatrix1)=satts1$ID
colnames(smatrix1)=satts1$ID

#Provide ID's for the Dyads
rownames(sdyad1)=satts1$ID
colnames(sdyad1)=satts1$ID

#change matrix to a network object and check it
smat1=network(smatrix1,directed=TRUE)
smat1
summary(smat1)

#same for the dyadic covariates
sdyadic1=network(sdyad1,directed=FALSE)
sdyadic1
summary(sdyadic1)

#Now to add attributes to the network object
smat1%v%'ID'=satts1$ID
smat1%v%'Gender'=satts1$Gender
smat1%v%'Ethnic'=satts1$Ethnic
smat1%v%'Ethnic'=satts1$PTJob
smat1%v%'CigsOk'=satts1$CigsOk
smat1%v%'AlcOk'=satts1$AlcOk
smat1%v%'DrugsBad'=satts1$DrugsBad
smat1%v%'SchoolPercept'=satts1$SchoolPercep
smat1%v%'PhysActive'=satts1$PhysActive
smat1%v%'WorkerSmoke'=satts1$WorkerSmoke
smat1%v%'WorkerDrink'=satts1$WorkerDrink
smat1%v%'WorkQuitSmoke'=satts1$WorkQuitSmoke
smat1%v%'WorkQuitDrink'=satts1$WorkQuitDrink
smat1%v%'PersonSmoke'=satts1$PersonSmoke
smat1%v%'PersonDrink'=satts1$PersonDrink
smat1%v%'PosSmoke'=satts1$PosSmoke
smat1%v%'PosDrink'=satts1$PosDrink
smat1%v%'Adult.Smoke'=satts1$Adult.Smoke
smat1%v%'Sibling.Smoke'=satts1$Sibling.Smoke
smat1%v%'FemaleSmoke'=satts1$FemaleSmoke
smat1%v%'MaleSmoke'=satts1$MaleSmoke
smat1%v%'FemQuitSmok'=satts1$FemQuitSmok
smat1%v%'MaleQuitSmok'=satts1$MaleQuitSmok
smat1%v%'SibQuitSmoke'=satts1$SibQuitSmoke
smat1%v%'FemaleDrink'=satts1$FemaleDrink
smat1%v%'MaleDrink'=satts1$MaleDrink
smat1%v%'SiblingDrink'=satts1$SiblingDrink
smat1%v%'FemQuitDrink'=satts1$FemQuitDrink
smat1%v%'MaleQuitDrink'=satts1$MaleQuitDrink
smat1%v%'SibQuitDrink'=satts1$SibQuitDrink
