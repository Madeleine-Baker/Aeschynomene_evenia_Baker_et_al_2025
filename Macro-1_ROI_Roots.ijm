//To be done once per set of images of same plants, to delimit aera with each root, number them. ROI for each root can be adjusted at the beginning of Macro-2 for each image
//open macro
//open image
//roughly delimit roots pree of "t" for each
//Run macro

waitForUser("- open image\n - surround all root one by one with drawing tool and clic t for each\n - then run this macro");

nom=File.nameWithoutExtension;
dirr=File.directory();

nbroi=roiManager("count");
for(i=0;i<nbroi;i++)
{
roiManager("Select", i);
roiManager("Rename", nom+"_"+i+1);
}

roiManager("show all");
roiManager("save", dirr+"//"+nom+"_INDIV_ROOT.zip");
