Off[CreateDirectory::ioerr];
Print[StyleForm["=====================================================","Section",FontSize->14]]
Print[StyleForm["PACKAGE:","Section",FontSize->14],StyleForm[" IMAGE CLASSIFIER","Section",FontSize->14] ]
Print[StyleForm["BY: Jorge Chávez Carlos, 2018","Section",FontSize->12]]
Print[StyleForm["=====================================================","Section",FontSize->14]]
Print[StyleForm["This analizer traing and learn with 50,000 images of size: 32x32,","Section",FontSize->12]]
Print[StyleForm["and it can classifier what kind of image is something or a lot of 10,000 test images, with 100 diferent classes.","Section",FontSize->12]]
Print[StyleForm["Link to download:","Section",FontSize->10]]
Print["https://www.dropbox.com/s/x51wzzjp1fo3fve/IB_JCHC.zip"]
Print[StyleForm["Trining process:","Section",FontSize->12]]
datTest=BinaryReadList["test.bin"];
datTrain=BinaryReadList["train.bin"];
coarse=Import["coarse_label_names.txt","Table"];
fine=Import["fine_label_names.txt","Table"];
dat=Partition[datTrain,3074];
l=Length[dat];
datRGB[j_]:=Table[{dat[[j,i]],dat[[j,i+1024]],dat[[j,i+2048]]},{i,3,1026}];
datp=Partition[datTest,3074];
lp=Length[datp];
datpRGB[j_]:=Table[{datp[[j,i]],datp[[j,i+1024]],datp[[j,i+2048]]},{i,3,1026}];
f[i_]:=Image[Partition[datRGB[i]/1024.0,32]];
cl[i_]:=fine[[dat[[i,{1,2}]][[2]]+1]][[1]];
Print["Trining with 50,000 images..."];
trainingset =Table[f[i]->cl[i],{i,1,l}];
c=Classify[trainingset];
Print["Trining complete!"];
Print["Trining time: ",ClassifierInformation[c, "TrainingTime"]];
Print[StyleForm["Command list:","Section",FontSize->12]]
Print["Using 10,000 test images, with n\[Element][1,10000]:"];
Print["IM[n], Show the n-th image and their class"];
Print["CL[n], Clasifies the n-th image, and give information about of probabilities"];
Print["PL[n], Returns a plot of the % of probability to do some image"];
Print["LS[n], Returns a 3-array elements {class, predintion, 1 or 0} where 1- is true, 0 is false"];
Print["TAB[n], Returns the same information that LS but in terms of # of image"];
Print["CSV, Create the results.csv file, whit the analysis for all images test."];
Print["Ready to identify images!"];
Print[StyleForm["=====================================================","Section",FontSize->14]]
Speak["El paquete fué cargado exitosamente"];
(*superclass=coarse[[datp[[a,{1,2}]][[1]]+1]][[1]];*)
IM[a_]:={
class=fine[[datp[[a,{1,2}]][[2]]+1]][[1]];
b=Image[Partition[datpRGB[a]/1024.0,32],ImageSize->Medium];
Print["Clase: ",class];
Show[b]};
CL[a_]:={
class=fine[[datp[[a,{1,2}]][[2]]+1]][[1]];
b=Image[Partition[datpRGB[a]/1024.0,32]];
(*Print["Superclass: ",superclass, ", Class: ",class];*)
Print["Class: ",class];
Print["Prediction: ",c[b], ", with a percentage of probability ",100c[b, "TopProbabilities"][[1,2]], "%"];Print["The prediction is: ",class==c[b]];
Print["The percentage of probability to be a ", class, " is:", 100c[Image[Partition[datpRGB[a]/1024.0,32]], "Probabilities"][[Position[fine,c[Image[Partition[datpRGB[a]/1024.0,32]]]][[1,1]]]]];};
PL[a_]:=ListPlot[100Table[c[Image[Partition[datpRGB[a]/1024.0,32]], "Probabilities"][[i]],{i,1,100}],PlotRange-> All,Frame-> True,ImageSize->800,PlotStyle->Black, Filling->Axis,FrameLabel->{Style[" # image  ",25],Style[" % of coincidence ",25]},LabelStyle->{Directive[15],Black}];
LS[a_]:={fine[[datp[[a,{1,2}]][[2]]+1]][[1]],
c[Image[Partition[datpRGB[a]/1024.0,32]]],Boole[fine[[datp[[a,{1,2}]][[2]]+1]][[1]]==c[Image[Partition[datpRGB[a]/1024.0,32]]]]};
TAB[a_]:={Position[fine,fine[[datp[[a,{1,2}]][[2]]+1]]][[1,1]],
Position[fine,c[Image[Partition[datpRGB[a]/1024.0,32]]]][[1,1]],100c[Image[Partition[datpRGB[a]/1024.0,32]], "Probabilities"][[Position[fine,c[Image[Partition[datpRGB[a]/1024.0,32]]]][[1,1]]]],Boole[fine[[datp[[a,{1,2}]][[2]]+1]][[1]]==c[Image[Partition[datpRGB[a]/1024.0,32]]]]};
CSV:={res=ParallelTable[Flatten[{i,TAB[i]}],{i,1,lp}];
Export["results.csv",res];
Print["Ths results was made saved in the file results.csv"];
Speak["El análisis de las 10000, imágenes se ha realizado."];};
