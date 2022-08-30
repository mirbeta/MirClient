//---------------------------------------------------------------------------
#ifndef RatingControlDemoMainH
#define RatingControlDemoMainH
//---------------------------------------------------------------------------
  #include <cxGraphics.hpp>
  #include <cxControls.hpp>
  #include <cxLookAndFeels.hpp>
  #include <cxLookAndFeelPainters.hpp>
  #include <cxStyles.hpp>
  #include <cxCustomData.hpp>
  #include <DB.hpp>
  #include <cxDBData.hpp>
  #include <cxFilter.hpp>
  #include <cxData.hpp>
  #include <cxDataStorage.hpp>
  #include <cxEdit.hpp>
  #include <cxNavigator.hpp>
  #include <cxContainer.hpp>
  #include <ImgList.hpp>
  #include <Controls.hpp>
  #include <dxmdaset.hpp>
  #include <cxExtEditRepositoryItems.hpp>
  #include <cxEditRepositoryItems.hpp>
  #include <cxDropDownEdit.hpp>
  #include <cxCheckBox.hpp>
  #include <StdCtrls.hpp>
  #include <cxRadioGroup.hpp>
  #include <cxSpinEdit.hpp>
  #include <cxTextEdit.hpp>
  #include <cxMaskEdit.hpp>
  #include <cxLabel.hpp>
  #include <cxGroupBox.hpp>
  #include <cxGridLevel.hpp>
  #include <dxLayoutContainer.hpp>
  #include <cxGridViewLayoutContainer.hpp>
  #include <cxGridLayoutView.hpp>
  #include <cxGridCustomTableView.hpp>
  #include <cxGridDBLayoutView.hpp>
  #include <cxGridCustomView.hpp>
  #include <cxGridCustomLayoutView.hpp>
  #include <cxGrid.hpp>
  #include <cxGridCardView.hpp>
  #include <cxGridTableView.hpp>
  #include <cxClasses.hpp>
  #include <Menus.hpp>
  #include <ComCtrls.hpp>
  #include <Classes.hpp>
  #include <cxDBLookupComboBox.hpp>
  #include "RatingControlDemoImagePicker.h"
  #include <BaseForm.h>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGrid* Grid;
	TDataSource* dsCars;
	TcxEditRepository* EditRepository;
	TcxEditRepositoryImageItem* EditRepositoryImage;
	TcxEditRepositoryCurrencyItem* EditRepositoryPrice;
	TcxGridLevel* GridLevel1;
	TdxLayoutGroup* LayoutViewGroup_Root;
	TcxGridDBLayoutView* LayoutView;
	TcxGridLayoutItem* LayoutViewLayoutItem1;
	TcxGridDBLayoutViewItem* LayoutViewRecId;
	TcxGridLayoutItem* LayoutViewLayoutItem2;
	TcxGridDBLayoutViewItem* LayoutViewID;
	TcxGridLayoutItem* LayoutViewLayoutItem3;
	TcxGridDBLayoutViewItem* LayoutViewTrademark;
	TcxGridLayoutItem* LayoutViewLayoutItem4;
	TcxGridDBLayoutViewItem* LayoutViewModel;
	TcxGridLayoutItem* LayoutViewLayoutItem12;
	TcxGridDBLayoutViewItem* LayoutViewCategory;
	TcxGridLayoutItem* LayoutViewLayoutItem15;
	TcxGridDBLayoutViewItem* LayoutViewPicture;
	TcxGridLayoutItem* LayoutViewLayoutItem16;
	TcxGridDBLayoutViewItem* LayoutViewPrice;
	TdxLayoutGroup* LayoutViewGroup4;
	TdxLayoutGroup* LayoutViewGroup5;
	TdxLayoutEmptySpaceItem* LayoutViewSpaceItem1;
	TcxStyle* stValues;
	TcxStyle* stItems;
	TdxLayoutSeparatorItem* LayoutViewSeparatorItem1;
	TdxLayoutGroup* LayoutViewGroup11;
	TcxStyle* stHeader;
	TcxStyle* stRecordCaption;
	TdxMemData* mdCars;
	TcxGroupBox* GroupBox;
	TcxImageList* Images;
	TcxStyle* stRecordSelected;
	TcxGridLayoutItem* LayoutViewLayoutItem17;
	TcxGridDBLayoutViewItem* LayoutViewRating;
	TcxEditRepositoryRatingControl* EditRepositoryRating;
	TcxLabel* lbOrientation;
	TcxLabel* lbItemCount;
	TcxLabel* lbStep;
	TcxLabel* lbCustomImages;
	TcxComboBox* cmbOrientation;
	TcxSpinEdit* seItemCount;
	TcxGroupBox* gbFillPrecision;
	TcxRadioButton* rbFull;
	TcxRadioButton* rbHalf;
	TcxRadioButton* rbExact;
	TcxSpinEdit* seStep;
	TcxCheckBox* cbReverseDirection;
	TcxCheckBox* cbAllowHover;
	TcxPopupEdit* peChooseImage;
	TIntegerField* mdCarsID;
	TIntegerField* mdCarsTrademarkID;
	TWideStringField* mdCarsName;
	TWideStringField* mdCarsModification;
	TIntegerField* mdCarsCategoryID;
	TBCDField* mdCarsPrice;
	TIntegerField* mdCarsMPG_City;
	TIntegerField* mdCarsMPG_Highway;
	TIntegerField* mdCarsDoors;
	TIntegerField* mdCarsBodyStyleID;
	TIntegerField* mdCarsCilinders;
	TWideStringField* mdCarsHorsepower;
	TWideStringField* mdCarsTorque;
	TWideStringField* mdCarsTransmission_Speeds;
	TIntegerField* mdCarsTransmission_Type;
	TWideMemoField* mdCarsDescription;
	TBlobField* mdCarsImage;
	TdxMemData* mdCategory;
	TIntegerField* mdCategoryID;
	TWideStringField* mdCategoryName;
	TBlobField* mdCategoryPicture;
	TdxMemData* mdTrademark;
	TIntegerField* mdTrademarkID;
	TWideStringField* mdTrademarkName;
	TWideStringField* mdTrademarkSite;
	TBlobField* mdTrademarkLogo;
	TWideMemoField* mdTrademarkDescription;
	TDataSource* dsCategory;
	TDataSource* dsTrademark;
	TFloatField* mdCarsRating;
	void __fastcall AllowHoverChange(TObject* Sender);
	void __fastcall ChooseImageCloseUp(TObject* Sender);
	void __fastcall ChooseImageInitPopup(TObject* Sender);
	void __fastcall FillPrecisionChange(TObject* Sender);
	void __fastcall ItemCountChange(TObject* Sender);
	void __fastcall OrientationChange(TObject* Sender);
	void __fastcall ReverseDirectionChange(TObject* Sender);
	void __fastcall StepChange(TObject* Sender);
private:
	TfrmImagePicker* FPopupWindow;
public:		// User declarations
	__fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
