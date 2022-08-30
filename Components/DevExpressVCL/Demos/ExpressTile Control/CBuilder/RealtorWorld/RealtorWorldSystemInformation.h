//---------------------------------------------------------------------------

#ifndef RealtorWorldSystemInformationH
#define RealtorWorldSystemInformationH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxImage.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxGDIPlusClasses.hpp"
#include "RealtorWorldBaseFrame.h"
#include "cxClasses.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxFilter.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxProgressBar.hpp"
#include "cxSpinEdit.hpp"
#include "cxStyles.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TfrmSystemInformation : public TfrmBase
{
__published:	// IDE-managed Components
	TcxGrid *cxGrid1;
	TcxGridLevel *cxGrid1Level1;
	TcxGridTableView *cxGrid1TableView1;
	TcxGridColumn *cxGrid1TableView1Column1;
	TcxGridColumn *cxGrid1TableView1Column2;
	TcxGridColumn *cxGrid1TableView1Column3;
	TTimer *tmRefesh;
	TcxGroupBox *cxGroupBox1;
	TcxImage *Image1;
	TcxLabel *lbComputerName;
	TcxLabel *lbMemory;
	TcxProgressBar *pbMemoryUsage;
	TcxLabel *lbWindowsInfo;
	TcxLabel *lbProcessorInfo;
	void __fastcall tmRefeshTimer(TObject *Sender);
private:	// User declarations
	String GetMachineName();
	DWORD GetProcessMemoryUsage(THandle AProcessID);
	void QueryMemoryUsage();
	void QuerySystemInfo();
public:		// User declarations
	__fastcall TfrmSystemInformation(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmSystemInformation *frmSystemInformation;
//---------------------------------------------------------------------------
#endif
