//---------------------------------------------------------------------------

#ifndef LayoutViewDemoGroupScrollingMainH
#define LayoutViewDemoGroupScrollingMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBLayoutView.hpp"
#include "cxGridLayoutView.hpp"
#include "cxGridLevel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxStyles.hpp"
#include "dxLayoutContainer.hpp"
#include "dxmdaset.hpp"
#include <DB.hpp>
#include <Menus.hpp>
#include <ExtCtrls.hpp>
#include "BaseForm.h"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxGridCardView.hpp"
#include "cxGridTableView.hpp"
#include "cxGroupBox.hpp"
#include "cxRadioGroup.hpp"
#include "cxLabel.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGridViewLayoutContainer.hpp"
#include <ComCtrls.hpp>
#include "CarsData.h"
#include "CarsDataForGrid.h"
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
    TcxGrid *Grid;
    TcxGridLevel *GridLevel1;
    TdxLayoutGroup *LayoutViewGroup_Root;
    TcxGridDBLayoutView *LayoutView;
    TdxLayoutGroup *LayoutViewGroup1;
    TdxLayoutGroup *LayoutViewGroup2;
    TdxLayoutGroup *LayoutViewGroup3;
    TcxGridLayoutItem *LayoutViewLayoutItem1;
    TcxGridDBLayoutViewItem *LayoutViewRecId;
    TcxGridLayoutItem *LayoutViewLayoutItem2;
    TcxGridDBLayoutViewItem *LayoutViewID;
    TcxGridLayoutItem *LayoutViewLayoutItem3;
    TcxGridDBLayoutViewItem *LayoutViewTrademark;
    TcxGridLayoutItem *LayoutViewLayoutItem4;
    TcxGridDBLayoutViewItem *LayoutViewModel;
    TcxGridLayoutItem *LayoutViewLayoutItem5;
    TcxGridDBLayoutViewItem *LayoutViewHP;
    TcxGridLayoutItem *LayoutViewLayoutItem7;
    TcxGridDBLayoutViewItem *LayoutViewCyl;
    TcxGridLayoutItem *LayoutViewLayoutItem8;
    TcxGridDBLayoutViewItem *LayoutViewTransmissSpeedCount;
    TcxGridLayoutItem *LayoutViewLayoutItem9;
    TcxGridDBLayoutViewItem *LayoutViewTransmissAutomatic;
    TcxGridLayoutItem *LayoutViewLayoutItem10;
    TcxGridDBLayoutViewItem *LayoutViewMPG_City;
    TcxGridLayoutItem *LayoutViewLayoutItem11;
    TcxGridDBLayoutViewItem *LayoutViewMPG_Highway;
    TcxGridLayoutItem *LayoutViewLayoutItem12;
    TcxGridDBLayoutViewItem *LayoutViewCategory;
    TcxGridLayoutItem *LayoutViewLayoutItem13;
    TcxGridDBLayoutViewItem *LayoutViewDescription;
    TcxGridLayoutItem *LayoutViewLayoutItem14;
    TcxGridDBLayoutViewItem *LayoutViewHyperlink;
    TcxGridLayoutItem *LayoutViewLayoutItem15;
    TcxGridDBLayoutViewItem *LayoutViewPicture;
    TcxGridLayoutItem *LayoutViewLayoutItem16;
    TcxGridDBLayoutViewItem *LayoutViewPrice;
    TdxLayoutGroup *LayoutViewGroup4;
    TdxLayoutGroup *LayoutViewGroup5;
    TdxLayoutGroup *LayoutViewGroup7;
    TdxLayoutGroup *LayoutViewGroup8;
    TdxLayoutEmptySpaceItem *LayoutViewSpaceItem1;
    TcxStyle *stValues;
    TcxStyle *stItems;
    TdxLayoutGroup *LayoutViewGroup10;
    TdxLayoutSeparatorItem *LayoutViewSeparatorItem1;
    TdxLayoutGroup *LayoutViewGroup9;
    TdxLayoutEmptySpaceItem *LayoutViewSpaceItem2;
    TdxLayoutEmptySpaceItem *LayoutViewSpaceItem3;
    TdxLayoutEmptySpaceItem *LayoutViewSpaceItem4;
    TdxLayoutEmptySpaceItem *LayoutViewSpaceItem5;
    TdxLayoutGroup *LayoutViewGroup11;
    TcxStyle *stHeader;
    TcxStyle *stRecordCaption;
    TcxImageList *Images;
    TcxStyle *stRecordSelected;
    TdxLayoutGroup *LayoutViewGroup12;
    TdxLayoutAutoCreatedGroup *LayoutViewGroup13;
    TcxGroupBox* cxGroupBox1;
    TcxSpinEdit* seRecordWidth;
    TcxLabel* lbRecordWidth;
    TcxLabel* lbrecordHeight;
    TcxSpinEdit* seRecordHeight;
    void __fastcall seRecordHeightPropertiesChange(TObject* Sender);
    void __fastcall seRecordWidthPropertiesChange(TObject* Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
