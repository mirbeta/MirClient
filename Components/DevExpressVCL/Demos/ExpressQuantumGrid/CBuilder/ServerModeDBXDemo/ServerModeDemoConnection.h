//---------------------------------------------------------------------------

#ifndef ServerModeDemoConnectionH
#define ServerModeDemoConnectionH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxGridCardView.hpp"
#include "cxGridTableView.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxProgressBar.hpp"
#include "cxRadioGroup.hpp"
#include "cxSpinEdit.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include <ComCtrls.hpp>
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
typedef void __fastcall (__closure *TdxProgressEvent)(System::TObject* Sender, Double Value);
//---------------------------------------------------------------------------
class TServerModeDemoConnectionForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TPanel *Panel1;
	TcxLabel *lbSQLServer;
	TcxLabel *lbDatabase;
	TcxLabel *lbLoginName;
	TcxLabel *lbPassword;
	TcxLabel *lbRecordCount;
	TcxTextEdit *edSQLServer;
	TcxRadioGroup *rgConnectUsing;
	TcxTextEdit *edLoginName;
	TcxTextEdit *edPassword;
	TcxButton *btAddRecordsAndStartDemo;
	TcxButton *btStartDemo;
	TcxSpinEdit *seCount;
	TcxProgressBar *ProgressBar;
	TcxMemo *mDescription;
	TcxLabel *lbTableName;
	TcxTextEdit *edDatabase;
	TcxTextEdit *edTableName;
	TcxButton *btTestConnection;
	TcxLabel *lbCurrentCount;
	void __fastcall btAddRecordsAndStartDemoClick(TObject *Sender);
	void __fastcall btStartDemoClick(TObject *Sender);
	void __fastcall btTestConnectionClick(TObject *Sender);
	void __fastcall rgConnectUsingPropertiesChange(TObject *Sender);
private:	// User declarations
	void ButtonsEnabled(Boolean AValue);
	void __fastcall SetProgressBarPosition(TObject* Sender, double Value);
public:		// User declarations
	__fastcall TServerModeDemoConnectionForm(TComponent* Owner);
	void __fastcall AfterConstruction();
	void Connect(String ADatabaseName);
	void CreateDatabaseAndConnect();
	void CreateTable();
	void StartDemo();
};
//---------------------------------------------------------------------------
extern PACKAGE TServerModeDemoConnectionForm *ServerModeDemoConnectionForm;
//---------------------------------------------------------------------------
#endif
