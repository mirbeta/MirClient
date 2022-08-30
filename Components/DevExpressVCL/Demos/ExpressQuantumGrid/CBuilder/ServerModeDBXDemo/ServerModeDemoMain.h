//---------------------------------------------------------------------------

#ifndef ServerModeDemoMainH
#define ServerModeDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxCalendar.hpp"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCardView.hpp"
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxGridServerModeTableView.hpp"
#include "cxGridTableView.hpp"
#include "cxImageComboBox.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxStyles.hpp"
#include "cxNavigator.hpp"
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxGridBandedTableView.hpp"
#include "cxGridServerModeBandedTableView.hpp"
//---------------------------------------------------------------------------
class TServerModeDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGrid *cxGrid1;
	TcxGridServerModeTableView *cxGrid1ServerModeTableView1;
	TcxGridServerModeColumn *cxGrid1ServerModeTableView1OID;
	TcxGridServerModeColumn *cxGrid1ServerModeTableView1Subject;
	TcxGridServerModeColumn *cxGrid1ServerModeTableView1From;
	TcxGridServerModeColumn *cxGrid1ServerModeTableView1Sent;
	TcxGridServerModeColumn *cxGrid1ServerModeTableView1Size;
	TcxGridServerModeColumn *cxGrid1ServerModeTableView1HasAttachment;
	TcxGridServerModeColumn *cxGrid1ServerModeTableView1Priority;
	TcxGridLevel *cxGrid1Level1;
	TcxGridPopupMenu *cxGridPopupMenu1;
	TcxImageList *ilImages;
	TMenuItem *mOptions;
	TMenuItem *mCancelOnExit;
	TMenuItem *mDeleting;
	TMenuItem *mDeletingConfirmation;
	TMenuItem *mEditing;
	TMenuItem *mInserting;
	TcxGridLevel *cxGrid1Level2;
	TcxGridServerModeBandedTableView *cxGrid1ServerModeBandedTableView1;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1OID;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Subject;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1From;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Sent;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Size;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1HasAttachment;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Priority;
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall UpdateOptionsDataView(TObject *Sender);
	void __fastcall cxGrid1ActiveTabChanged(TcxCustomGrid *Sender, TcxGridLevel &ALevel);
private:	// User declarations
public:		// User declarations
	__fastcall TServerModeDemoMainForm(TComponent* Owner);
	void Initialize(String ATableName);
};
//---------------------------------------------------------------------------
extern PACKAGE TServerModeDemoMainForm *ServerModeDemoMainForm;
//---------------------------------------------------------------------------
#endif
