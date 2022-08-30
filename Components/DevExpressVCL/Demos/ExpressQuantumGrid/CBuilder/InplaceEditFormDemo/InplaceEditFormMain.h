//---------------------------------------------------------------------------

#ifndef InplaceEditFormMainH
#define InplaceEditFormMainH
//---------------------------------------------------------------------------

  #include <Windows.hpp>
  #include <Messages.hpp>
  #include <SysUtils.hpp>
  #include <Classes.hpp>
  #include <Graphics.hpp>
  #include <Controls.hpp>
  #include <Forms.hpp>
  #include <ActnList.hpp>
  #include <Dialogs.hpp>
  #include <cxStyles.hpp>
  #include <cxCustomData.hpp>
  #include <cxGraphics.hpp>
  #include <cxFilter.hpp>
  #include <cxData.hpp>
  #include <cxDataStorage.hpp>
  #include <cxEdit.hpp>
  #include <DB.hpp>
  #include <cxDBData.hpp>
  #include <cxControls.hpp>
  #include <cxGridCustomView.hpp>
  #include <cxGridCustomTableView.hpp>
  #include <cxClasses.hpp>
  #include <cxGridLevel.hpp>
  #include <cxGrid.hpp>
  #include <StdCtrls.hpp>
  #include <Menus.hpp>
  #include <cxMemo.hpp>
  #include <cxImage.hpp>
  #include <cxCurrencyEdit.hpp>
  #include <cxHyperLinkEdit.hpp>
  #include <cxTextEdit.hpp>
  #include <cxEditRepositoryItems.hpp>
  #include <cxLookAndFeels.hpp>
  #include <cxLookAndFeelPainters.hpp>
  #include <dxLayoutContainer.hpp>
  #include <cxGridLayoutView.hpp>
  #include <cxGridDBLayoutView.hpp>
  #include <cxGridCustomLayoutView.hpp>
  #include <cxContainer.hpp>
  #include <cxGroupBox.hpp>
  #include <dxLayoutLookAndFeels.hpp>
  #include <ExtCtrls.hpp>
  #include <cxButtons.hpp>
  #include <dxmdaset.hpp>
  #include <BaseForm.h>
  #include <cxGridTableView.hpp>
  #include <cxRadioGroup.hpp>
  #include <cxCheckBox.hpp>
  #include <cxGridCardView.hpp>
  #include <ComCtrls.hpp>
  #include <ImgList.hpp>
  #include <cxLabel.hpp>
  #include <cxMaskEdit.hpp>
  #include <cxDropDownEdit.hpp>
  #include <cxNavigator.hpp>
  #include <cxGridDBTableView.hpp>
  #include <cxSpinEdit.hpp>
  #include <cxGridViewLayoutContainer.hpp>
  #include <cxGridInplaceEditForm.hpp>
  #include <CarsDataForGrid.h>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGrid* Grid;
	TcxEditRepository* EditRepository;
	TcxEditRepositoryImageItem* EditRepositoryImage;
	TcxEditRepositoryMemoItem* EditRepositoryMemo;
	TcxEditRepositoryHyperLinkItem* EditRepositoryHyperLink;
	TcxEditRepositoryCurrencyItem* EditRepositoryPrice;
	TcxEditRepositoryCheckBoxItem* EditRepositoryAutomatic;
	TMenuItem* miCustomize;
	TcxGridLevel* GridLevel1;
	TcxStyle* stValues;
	TcxStyle* stItems;
	TcxStyle* stHeader;
	TcxStyle* stRecordCaption;
	TcxImageList* Images;
	TcxStyle* stRecordSelected;
	TcxGridDBTableView* TableView;
	TcxGridDBColumn* TableViewRecId;
	TcxGridDBColumn* TableViewID;
	TcxGridDBColumn* TableViewTrademark;
	TcxGridDBColumn* TableViewModel;
	TcxGridDBColumn* TableViewHP;
	TcxGridDBColumn* TableViewCyl;
	TcxGridDBColumn* TableViewTransmissSpeedCount;
	TcxGridDBColumn* TableViewTransmissAutomatic;
	TcxGridDBColumn* TableViewMPG_City;
	TcxGridDBColumn* TableViewMPG_Highway;
	TcxGridDBColumn* TableViewCategory;
	TcxGridDBColumn* TableViewDescription;
	TcxGridDBColumn* TableViewHyperlink;
	TcxGridDBColumn* TableViewPicture;
	TcxGridDBColumn* TableViewPrice;
	TMenuItem* miEditMode;
	TMenuItem* miInplace;
	TMenuItem* miInplaceEditForm;
	TMenuItem* miInplaceEditFormHideCurrentRow;
	TActionList* alAction;
	TAction* actCustomizeEditForm;
	TcxGroupBox* gbOptions;
	TcxButton* btnCustomizeEditForm;
	TAction* actInplace;
	TAction* actInplaceEditForm;
	TAction* actInplaceEditFormHCR;
	TcxGroupBox* cxGroupBox1;
	TcxRadioButton* rbInplace;
	TcxRadioButton* rbInplaceEditForm;
	TcxRadioButton* rbInplaceEditFormHideCurrentRow;
	TMenuItem* miHotTrack;
	TAction* actHotTrack;
	void __fastcall FormShow(TObject *Sender);
	void __fastcall actCustomizeEditFormExecute(TObject *Sender);
	void __fastcall actEditModeChange(TObject *Sender);
	void __fastcall actHotTrackExecute(TObject *Sender);
public:		// User declarations
		__fastcall TfrmMain(TComponent* Owner);
};

//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
