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
#include "cxCheckBox.hpp"
#include "cxSpinEdit.hpp"
//---------------------------------------------------------------------------
class TServerModeDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
    TcxGridPopupMenu *cxGridPopupMenu1;
    TcxGrid *cxGrid1;
    TcxGridServerModeTableView *cxGrid1ServerModeTableView1;
    TcxGridLevel *cxGrid1Level1;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1OrderDate;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Trademark;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Model;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1HP;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1TransmissSpeedCount;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1TransmissAutomatic;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Category;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Price;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1FirstName;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1LastName;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Company;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Prefix;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Title;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Address;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1City;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1State;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Source;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Customer;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1HomePhone;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Description;
    TcxGridServerModeColumn *cxGrid1ServerModeTableView1Email;
    TImageList *ImageList;
    TcxGridLevel *cxGrid1Level2;
    TcxGridServerModeBandedTableView *cxGrid1ServerModeBandedTableView1;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1OrderDate;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Trademark;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Model;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1HP;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Category;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Price;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1FirstName;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1LastName;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Company;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Prefix;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Title;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Address;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1City;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1State;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Source;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1HomePhone;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Description;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Email;
    TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1TransmissAutomatic;
	TcxGridServerModeBandedColumn *cxGrid1ServerModeBandedTableView1Customer;
	void __fastcall FormClose(TObject *Sender, TCloseAction &Action);
	void __fastcall cxGrid1ActiveTabChanged(TcxCustomGrid *Sender, TcxGridLevel &ALevel);
private:	// User declarations
public:		// User declarations
	__fastcall TServerModeDemoMainForm(TComponent* Owner);
	void Initialize();
};
//---------------------------------------------------------------------------
extern PACKAGE TServerModeDemoMainForm *ServerModeDemoMainForm;
//---------------------------------------------------------------------------
#endif
