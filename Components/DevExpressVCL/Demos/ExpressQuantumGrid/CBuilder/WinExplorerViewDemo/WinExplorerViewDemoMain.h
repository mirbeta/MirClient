//---------------------------------------------------------------------------
#ifndef WinExplorerViewDemoMainH
#define WinExplorerViewDemoMainH
//---------------------------------------------------------------------------
  #include <Windows.hpp>
  #include <Messages.hpp>
  #include <SysUtils.hpp>
  #include <Variants.hpp>
  #include <Classes.hpp>
  #include <Graphics.hpp>
  #include <Controls.hpp>
  #include <Forms.hpp>
  #include <Dialogs.hpp>
  #include <cxLookAndFeels.hpp>
  #include <cxGridCardView.hpp>
  #include <cxStyles.hpp>
  #include <cxGridTableView.hpp>
  #include <cxClasses.hpp>
  #include <Menus.hpp>
  #include <ComCtrls.hpp>
  #include <StdCtrls.hpp>
  #include <cxGraphics.hpp>
  #include <cxControls.hpp>
  #include <dxCore.hpp>
  #include <cxLookAndFeelPainters.hpp>
  #include <cxCustomData.hpp>
  #include <cxFilter.hpp>
  #include <cxData.hpp>
  #include <cxDataStorage.hpp>
  #include <cxEdit.hpp>
  #include <cxNavigator.hpp>
  #include <cxGridCustomView.hpp>
  #include <cxGridCustomTableView.hpp>
  #include <cxGridWinExplorerView.hpp>
  #include <cxGridDBWinExplorerView.hpp>
  #include <cxGridLevel.hpp>
  #include <cxGrid.hpp>
  #include <CarsDataForGrid.h>
  #include <DB.hpp>
  #include <cxDBData.hpp>
  #include <cxGridDBTableView.hpp>
  #include <cxImage.hpp>
  #include <cxContainer.hpp>
  #include <cxGroupBox.hpp>
  #include <dxGalleryControl.hpp>
  #include <dxGallery.hpp>
  #include <cxLabel.hpp>
  #include <cxTextEdit.hpp>
  #include <cxMaskEdit.hpp>
  #include <cxDropDownEdit.hpp>
  #include <cxCheckBox.hpp>
  #include <BaseForm.h>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGridLevel* Level;
	TcxGrid* Grid;
	TcxGridDBWinExplorerView* WinExplorerView;
	TcxGridDBWinExplorerViewItem* WinExplorerViewTrademark;
	TcxGridDBWinExplorerViewItem* WinExplorerViewName;
	TcxGridDBWinExplorerViewItem* WinExplorerViewCategory;
	TcxGridDBWinExplorerViewItem* WinExplorerViewBodyStyle;
	TcxGridDBWinExplorerViewItem* WinExplorerViewTransmissionTypeName;
	TcxGridDBWinExplorerViewItem* WinExplorerViewDescription;
	TcxGridDBWinExplorerViewItem* WinExplorerViewImage;
	TcxGridDBWinExplorerViewItem* WinExplorerViewPhoto;
	TcxGridDBWinExplorerViewItem* WinExplorerViewInStock;
	TcxGroupBox* GroupBox;
	TdxGalleryControl* gcDisplayModes;
	TdxGalleryControlGroup* gcgGroup;
	TdxGalleryControlItem* gciExtraLargeIcons;
	TdxGalleryControlItem* gciLargeIcons;
	TdxGalleryControlItem* gciMediumIcons;
	TdxGalleryControlItem* gciSmallIcons;
	TdxGalleryControlItem* gciList;
	TdxGalleryControlItem* gciTiles;
	TdxGalleryControlItem* gciContent;
	TcxComboBox* cbSortBy;
	TcxLabel* lbSortBy;
	TcxLabel* lbGroupBy;
	TcxComboBox* cbGroupBy;
	TcxCheckBox* cbHotTrack;
	TcxCheckBox* cbMultiSelect;
	TcxCheckBox* cbShowCheckBoxes;
	TcxCheckBox* cbShowExpandButtons;
	void __fastcall cbSortByPropertiesEditValueChanged(TObject* Sender);
	void __fastcall cbGroupByPropertiesEditValueChanged(TObject* Sender);
	void __fastcall cbHotTrackPropertiesEditValueChanged(TObject* Sender);
	void __fastcall cbMultiSelectPropertiesEditValueChanged(TObject* Sender);
	void __fastcall cbShowCheckBoxesPropertiesEditValueChanged(TObject* Sender);
	void __fastcall cbShowExpandButtonPropertiesEditValueChanged(TObject* Sender);
	void __fastcall gcDisplayModesItemClick(TObject* Sender, TdxGalleryControlItem* AItem);
private:
	TcxGridWinExplorerViewItem* GetGroupItemByTag(int AValue);
	TdxSortOrder GetSortOrderByText(UnicodeString AValue);
	TcxGridWinExplorerViewDisplayMode GetDisplayModeByTag(int AValue);
	void SetGroupItem(TcxGridWinExplorerViewItem* AValue);
	void SetGroupItemSortOrder(TdxSortOrder ASortOrder);
	void SetTextItemSortOrder(TdxSortOrder ASortOrder);
	void SetDisplayMode(TcxGridWinExplorerViewDisplayMode AValue);
public:		// User declarations
	__fastcall TfrmMain(TComponent* Owner);

	void ShowCheckBoxes(bool AValue);
	void ShowExpandButtons(bool AValue);
	void UpdateGroup();
	void UpdateSortOrder();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
