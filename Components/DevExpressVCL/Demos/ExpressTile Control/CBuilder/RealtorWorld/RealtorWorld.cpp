// ---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop
#include <tchar.h>
// ---------------------------------------------------------------------------
USEFORM("RealtorWorldResearch.cpp", frmResearch); /* TFrame: File Type */
USEFORM("RealtorWorldDM.cpp", DMRealtorWorld);
USEFORM("RealtorWorldUnderConstruction.cpp", frmUnderConstruction);
USEFORM("RealtorWorldBaseFrame.cpp", frmBase); /* TFrame: File Type */
USEFORM("RealtorWorldMortgageRate.cpp", frmMortgageRate); /* TFrame: File Type */
USEFORM("RealtorWorldAgents.cpp", frmAgents); /* TFrame: File Type */
USEFORM("RealtorWorldMain.cpp", frmRealtorWorld);
USEFORM("RealtorWorldListing.cpp", frmListing); /* TFrame: File Type */
USEFORM("RealtorWorldStatistic.cpp", frmStatistic); /* TFrame: File Type */
USEFORM("RealtorWorldLoanCalculator.cpp", frmLoanCalculator); /* TFrame: File Type */
USEFORM("RealtorWorldHomePhotosBase.cpp", frmHomePhotosBase); /* TFrame: File Type */
USEFORM("RealtorWorldSystemInformation.cpp", frmSystemInformation); /* TFrame: File Type */
//---------------------------------------------------------------------------
int WINAPI _tWinMain(HINSTANCE, HINSTANCE, LPTSTR, int) {
	try {
		Application->Initialize();
		Application->MainFormOnTaskBar = true;
		Application->CreateForm(__classid(TDMRealtorWorld), &DMRealtorWorld);
		Application->CreateForm(__classid(TfrmRealtorWorld), &frmRealtorWorld);
		Application->Run();
	}
	catch(Exception & exception) {
		Application->ShowException(&exception);
	}
	catch(...) {
		try {
			throw Exception("");
		}
		catch(Exception & exception) {
			Application->ShowException(&exception);
		}
	}
	return 0;
}
// ---------------------------------------------------------------------------
