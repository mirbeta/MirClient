//---------------------------------------------------------------------------

#ifndef LayoutViewCarouselModeDemoMainH
#define LayoutViewCarouselModeDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
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
#include "cxNavigator.hpp"
#include "cxStyles.hpp"
#include "dxLayoutContainer.hpp"
#include <DB.hpp>
#include "dxLayoutLookAndFeels.hpp"
#include "dxmdaset.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxExtEditRepositoryItems.hpp"
#include "dxLayoutControl.hpp"
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxDropDownEdit.hpp"
#include "cxGridCardView.hpp"
#include "cxGridTableView.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxTrackBar.hpp"
#include "dxLayoutControlAdapters.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include <ComCtrls.hpp>
#include <Menus.hpp>
#include <math.h>
#include "dxToggleSwitch.hpp"
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
	TDataSource *dsHomes;
	TMenuItem *miView;
	TcxEditRepository *EditRepository;
	TcxEditRepositoryImageItem *EditRepositoryImage;
	TcxEditRepositoryMemoItem *EditRepositoryMemo;
	TcxEditRepositoryCurrencyItem *EditRepositoryPrice;
	TMenuItem *miCustomize;
	TcxButton *btnCustomize;
	TcxCheckBox *cbMultiSelectRecords;
	TcxCheckBox *cbRecordCaptions;
	TcxCheckBox *cbExpandableRecords;
	TdxMemData *mdHomes;
	TdxLayoutGroup *lcMainGroup_Root;
	TdxLayoutControl *lcMain;
	TdxLayoutItem *lcMainItem1;
	TdxLayoutItem *lcMainItem2;
	TdxLayoutItem *lcMainItem3;
	TdxLayoutItem *lcMainItem4;
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList1;
	TdxLayoutSkinLookAndFeel *dxLayoutSkinLookAndFeel1;
	TdxLayoutGroup *lcMainGroup1;
	TcxTrackBar *tbPitchAngle;
	TdxLayoutItem *liPitchAngle;
	TcxTrackBar *tbEndRecordScale;
	TdxLayoutItem *lcMainItem6;
	TcxTrackBar *tbStartRecordScale;
	TdxLayoutItem *lcMainItem7;
	TcxTrackBar *tbRollAngle;
	TdxLayoutItem *lcMainItem8;
	TdxLayoutGroup *lcMainGroup3;
	TdxLayoutItem *lcMainItem9;
	TcxTrackBar *tbBackgroundAlphaLevel;
	TdxLayoutItem *lcMainItem10;
	TcxTrackBar *tbRecordCount;
	TcxComboBox *cbInterpolationMode;
	TdxLayoutItem *lcMainItem11;
	TdxLayoutGroup *lcMainGroup7;
	TcxGrid *Grid;
	TcxGridDBLayoutView *LayoutView;
	TdxLayoutGroup *dxLayoutGroup1;
	TcxGridLevel *GridLevel1;
	TdxLayoutGroup *lcMainGroup5;
	TMemoField *mdHomesAddress;
	TSmallintField *mdHomesBeds;
	TSmallintField *mdHomesBaths;
	TFloatField *mdHomesHouseSize;
	TFloatField *mdHomesPrice;
	TMemoField *mdHomesFeatures;
	TMemoField *mdHomesYearBuilt;
	TBlobField *mdHomesPhoto;
	TcxGridLayoutItem *LayoutViewLayoutItem1;
	TcxGridDBLayoutViewItem *LayoutViewRecId;
	TcxGridLayoutItem *LayoutViewLayoutItem3;
	TcxGridDBLayoutViewItem *LayoutViewAddress;
	TcxGridLayoutItem *LayoutViewLayoutItem4;
	TcxGridDBLayoutViewItem *LayoutViewBeds;
	TcxGridLayoutItem *LayoutViewLayoutItem5;
	TcxGridDBLayoutViewItem *LayoutViewBaths;
	TcxGridLayoutItem *LayoutViewLayoutItem6;
	TcxGridDBLayoutViewItem *LayoutViewHouseSize;
	TcxGridLayoutItem *LayoutViewLayoutItem8;
	TcxGridDBLayoutViewItem *LayoutViewPrice;
	TcxGridLayoutItem *LayoutViewLayoutItem9;
	TcxGridDBLayoutViewItem *LayoutViewFeatures;
	TcxGridLayoutItem *LayoutViewLayoutItem10;
	TcxGridDBLayoutViewItem *LayoutViewYearBuilt;
	TcxGridLayoutItem *LayoutViewLayoutItem13;
	TcxGridDBLayoutViewItem *LayoutViewPhoto;
	TdxLayoutGroup *LayoutViewGroup4;
	TdxLayoutGroup *LayoutViewGroup13;
	TcxEditRepositorySpinItem *EditRepositorySpinItem;
	TdxLayoutGroup *LayoutViewGroup3;
	TdxToggleSwitch *tsAutoPitchAngle;
	TdxLayoutItem *lcMainItem5;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall FormShow(TObject *Sender);
	void __fastcall btnCustomizeClick(TObject *Sender);
	void __fastcall cbExpandableRecordsClick(TObject *Sender);
	void __fastcall cbRecordCaptionsClick(TObject *Sender);
	void __fastcall cbMultiSelectRecordsClick(TObject *Sender);
	void __fastcall CarouselModePropertiesChange(TObject *Sender);
	void __fastcall miCustomizeClick(TObject *Sender);
private:	// User declarations
	Integer FLockCount;
public:		// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
