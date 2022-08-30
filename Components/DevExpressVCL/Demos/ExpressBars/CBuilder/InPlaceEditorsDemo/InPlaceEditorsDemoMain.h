//---------------------------------------------------------------------------
#ifndef InPlaceEditorsDemoMainH
#define InPlaceEditorsDemoMainH
//---------------------------------------------------------------------------

#include <Windows.hpp>
#include <Messages.hpp>
#include <SysUtils.hpp>
#include <Classes.hpp>
#include <Graphics.hpp>
#include <Controls.hpp>
#include <Forms.hpp>
#include <Dialogs.hpp>
#include <StdCtrls.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>

#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include "dxBar.hpp"
#include "dxBarExtItems.hpp"
#include "cxBarEditItem.hpp"
#include "dxRibbon.hpp"
#include "dxRibbonForm.hpp"
#include "dxRibbonFormCaptionHelper.hpp"
#include "cxEdit.hpp"
#include "cxShellComboBox.hpp"
#include "cxDropDownEdit.hpp"
#include "cxBlobEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxCheckBox.hpp"
#include "cxLabel.hpp"
#include "cxTextEdit.hpp"
#include "cxColorComboBox.hpp"
#include "cxImageComboBox.hpp"
#include "cxFontNameComboBox.hpp"
#include "cxCheckComboBox.hpp"
#include "cxSpinEdit.hpp"
#include "cxProgressBar.hpp"
#include "cxTrackBar.hpp"
#include "cxTimeEdit.hpp"
#include "cxCalendar.hpp"
#include "cxCheckGroup.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxCalc.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxButtonEdit.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxMaskEdit.hpp"
#include "cxImage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxMRUEdit.hpp"
#include "cxDateUtils.hpp"
#include "Db.hpp"
#include "EBarsUtils.h"
#include "DB.hpp"
#include "dxmdaset.hpp"
#include "dxToggleSwitch.hpp"
//---------------------------------------------------------------------------

enum TcxBaseFrameCategory {bfcValueEditors, bfcMultilineTextEditors, bfcTextEditors,
  bfcImageEditors, bfcComboBoxes, bfcCheckBoxes, bfcStyles, bfcRibbonOptions};

class TfrmMain : public TdxCustomRibbonForm
{
__published:	// IDE-managed Components
  TdxBarManager *BarManager;
  TdxBarStatic *lblSelectEditorsBarHeader;
  TdxBarButton *btnEditorStyles;
  TdxBarButton *btnTextEdit;
  TdxBarButton *btnMultilineTextEdits;
  TdxBarButton *btnValueEdits;
  TdxBar *tlbValueEdits;
  TdxBar *tlbMultilineTextEdits;
  TdxBar *tlbTextEdits;
  TdxBar *tlbEditorStyles;
  TcxBarEditItem *cbStyle;
  TcxBarEditItem *cbStyleEdit;
  TcxBarEditItem *cbSelectEditType;
  TcxBarEditItem *edtPreviewItem;
  TcxStyleRepository *cxStyleRepository1;
  TcxStyle *Standard;
  TcxStyle *Italicized;
  TcxStyle *Colored;
  TcxImageList *ilSmall;
  TPanel *Panel2;
  TdxBar *tlbRibbonOptions;
  TcxBarEditItem *fncRibbonFontName;
  TcxBarEditItem *cbRibbonFont;
  TcxBarEditItem *ccbAssignedRibbonFonts;
  TdxBarButton *btnRibbonStyle;
  TcxBarEditItem *seFontSize;
  TcxBarEditItem *prbFontSize;
  TcxBarEditItem *trbFontSize;
  TcxBarEditItem *edtTime;
  TcxBarEditItem *edtDate;
  TcxBarEditItem *cbFontSize;
  TdxBarDockControl *dxBarDockControl1;
  TPanel *Panel1;
  TcxBarEditItem *clcFontColor;
  TcxBarEditItem *fncPathFontName;
  TcxBarEditItem *scbSelectPath;
  TdxBarButton *btnImageEditors;
  TdxBarButton *btnComboBoxes;
  TdxBarButton *btnCheckBoxes;
  TcxBarEditItem *edtMoney;
  TcxBarEditItem *edtCalculate;
  TcxBarEditItem *edtBlob;
  TcxBarEditItem *memMemo;
  TcxBarEditItem *reRich;
  TcxBarEditItem *beCompanyName;
  TcxBarEditItem *edtSite;
  TcxBarEditItem *lblCompanyName;
  TcxBarEditItem *mePhoneNum;
  TcxBarEditItem *edtCompanyName;
  TdxBar *tlbImageEditors;
  TcxBarEditItem *edtImage;
  TcxBarEditItem *edtBlobImage;
  TdxBar *tlbComboBoxes;
  TcxBarEditItem *imcImages;
  TcxBarEditItem *cbLookUp;
  TcxBarEditItem *edtLastPath;
  TdxBar *tlbCheckBoxes;
  TcxBarEditItem *chbMonochrome;
  TcxBarEditItem *cbSelectColor;
  TcxBarEditItem *chgSelectColor;
  TcxBarEditItem *rgSelectColor;
  TImage *Image1;
  TcxStyle *Bold;
  TDataSource *DataSource;
  TdxBarSubItem *siValueEditors;
  TdxBarButton *btnExit;
  TdxBar *tlbMainMenu;
  TdxBarSubItem *siFile;
  TdxBarSubItem *siView;
  TdxBarSubItem *siHelp;
  TdxBarSubItem *siBarStyles;
  TdxBarButton *btnShowDescription;
  TdxBarButton *btnToolBarDescriptions;
  TdxBarButton *btnBarsHelp;
  TdxBarButton *btnDockingHelp;
  TdxBarButton *btnDownloads;
  TdxBarButton *btnRateDemo;
  TdxBarButton *btnDXOnTheWeb;
  TdxBarButton *btnSupport;
  TdxBarButton *btnProducts;
  TdxBarButton *btnMyDX;
  TdxBar *tlbHelp;
  TLabel *lblDemoDescription;
  TdxBarSubItem *siCheckBoxes;
  TdxBarSubItem *siTextEditors;
  TdxBarSubItem *siMultilineTextEditors;
  TdxBarSubItem *siImageEditors;
  TdxBarSubItem *siComboBoxes;
  TdxBar *tlbEditorType;
  TdxBar *tlbPreview;
  TdxBarButton *btnStandard;
  TdxBarButton *btnEnhanced;
  TdxBarButton *btnFlat;
  TdxBarButton *btnXP;
  TdxBarButton *btnOffice11;
  TdxBar *tlbTextEditorsInSubMenu;
  TdxBar *tlbValueEditorsInSubMenu;
  TdxBar *tlbMultiLineTextEditorsInSubMenu;
  TdxBar *tlbImageEditorsInSubMenu;
  TdxBar *tlbComboBoxesInSubMenu;
  TdxBar *tlbCheckBoxesInSubMenu;
  TcxImageList *ilLarge;
  TdxBarLargeButton *btnAbout;
  TdxBar *tlbDateTimeValues;
  TdxBar *tlbScaleValues;
  TdxBar *tlbShellMRUCombo;
  TdxBar *tlbImageLookUpCombo;
  TdxBar *tlbColorScheme;
  TdxBarLargeButton *btnBlueRibbonScheme;
  TdxBarLargeButton *BlackRibbonScheme;
  TdxBarLargeButton *SilverRibbonSheme;
  TcxBarEditItem *edtPopup;
  TMemo *memPopup;
  TdxBarSubItem *siGroup;
	TdxMemData *mdContacts;
	TAutoIncField *mdContactsID;
	TIntegerField *mdContactsProductID;
	TStringField *mdContactsFirstName;
	TStringField *mdContactsLastName;
	TStringField *mdContactsCompany;
	TStringField *mdContactsAddress;
	TStringField *mdContactsCity;
	TStringField *mdContactsState;
	TDateField *mdContactsPurchaseDate;
	TStringField *mdContactsPaymentType;
	TBCDField *mdContactsPaymentAmount;
	TStringField *mdContactsFullName;
	TcxBarEditItem *tsMonochrome;
  void __fastcall btnEditorStylesClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall cbSelectEditTypeChange(TObject *Sender);
  void __fastcall cbStyleChange(TObject *Sender);
  void __fastcall FormDestroy(TObject *Sender);
  void __fastcall cbRibbonFontChange(TObject *Sender);
  void __fastcall ccbAssignedRibbonFontsChange(TObject *Sender);
  void __fastcall btnRibbonStyleClick(TObject *Sender);
  void __fastcall seFontSizeChange(TObject *Sender);
  void __fastcall edtCompanyNameChange(TObject *Sender);
  void __fastcall edtMoneyChange(TObject *Sender);
  void __fastcall clcFontColorChange(TObject *Sender);
  void __fastcall edtSiteChange(TObject *Sender);
  void __fastcall chbMonochromeChange(TObject *Sender);
  void __fastcall scbSelectPathChange(TObject *Sender);
  void __fastcall edtLastPathChange(TObject *Sender);
  void __fastcall edtImageChange(TObject *Sender);
  void __fastcall reRichChange(TObject *Sender);
  void __fastcall memMemoChange(TObject *Sender);
  void __fastcall btnExitClick(TObject *Sender);
  void __fastcall btnShowDescriptionClick(TObject *Sender);
  void __fastcall btnToolBarDescriptionsClick(TObject *Sender);
  void __fastcall beCompanyNamePropertiesButtonClick(TObject *Sender,
    int AButtonIndex);
  void __fastcall cbStyleEditChange(TObject *Sender);
  void __fastcall imcImagesChange(TObject *Sender);
  void __fastcall fncRibbonFontNameChange(TObject *Sender);
  void __fastcall btnStandardClick(TObject *Sender);
  void __fastcall btnBlueRibbonSchemeClick(TObject *Sender);
  void __fastcall edtDateChange(TObject *Sender);
  void __fastcall mdContactsCalcFields(TDataSet *DataSet);
  void __fastcall edtDatePropertiesGetDayOfWeekState(TObject *Sender, TDay ADayOfWeek,
	TCustomDrawState AState, TFont *AFont, TColor &ABackgroundColor);
  void __fastcall edtDatePropertiesGetDayState(TObject *Sender, TDateTime ADate,
	TCustomDrawState AState, TFont *AFont, TColor &ABackgroundColor);
  void __fastcall edtPopupPropertiesCloseUp(TObject *Sender);
  void __fastcall cbLookUpChange(TObject *Sender);
  void __fastcall chgSelectColorChange(TObject *Sender);
  void __fastcall cbFontSizePropertiesValidate(TObject *Sender,
    Variant &DisplayValue, TCaption &ErrorText, bool &Error);
	void __fastcall tsMonochromeChange(TObject *Sender);
private:
  TdxRibbon *FRibbon;
  bool FUpdateLock;

  void __fastcall DockAllDemoToolBars(TdxBarDockControl *ADockControl);
  void __fastcall RibbonTabChanged(TdxCustomRibbon* Sender);

  TdxRibbonTab* __fastcall AddTab(TdxBar* AToolBar);
  void __fastcall AddDemoToolBars();
  void __fastcall AddToolBar(TdxBar* AToolBar);
  void __fastcall InitializeStylesCombo(TcxBarEditItem* ABarEditItem, TStrings* AStyles);
  bool __fastcall IsItemControlSelected(TdxBarItem* ABarItem, int ALink);
  void __fastcall ReadTextFile(TcxBarEditItem* AEditor, AnsiString AFileName);
  void __fastcall CreateFrames();

  void __fastcall UpdateCheckBoxesView();
  void __fastcall UpdateComboBoxesView();
  void __fastcall UpdateImageEditorsView();
  void __fastcall UpdateMultilineTextEditorsView();
  void __fastcall UpdateValueEditorsView();
  void __fastcall UpdateTextEditView();

  void __fastcall InitializeComboBoxes();
  void __fastcall InitializeCheckBoxes();
  void __fastcall InitializeImageEditors();
  void __fastcall InitializeMultilineTextEditors();
  void __fastcall InitializeTextEditors();
  void __fastcall InitializeValueEditors();
  void __fastcall InitializeEditorStyles();
  void __fastcall InitializeRibbon();

  void __fastcall InitializeFrames();

  void __fastcall SynchronizeCalcValueEditors(Variant AValue);
  void __fastcall SynchronizeCheckGroupEditors(Variant AValue);
  void __fastcall SynchronizeImageEditors(Variant AValue);
  void __fastcall SynchronizeMultilineTextEditors(Variant AValue);
  void __fastcall SynchronizePathEditors(Variant AValue);
  void __fastcall SynchronizeRibbonFontEditors();
  void __fastcall SynchronizeValueEditors(Variant AValue);
  void __fastcall SynchronizeTextEditors(Variant AValue);

  void __fastcall SetEditorStyle(AnsiString APropName, TcxBarEditItem *AStyleSource);
  void __fastcall UpdateActionsImages();

  AnsiString __fastcall TfrmMain::GetDateValue();
public:
  __fastcall TfrmMain (TComponent* Owner);
  void __fastcall HideAllFrames();
  void __fastcall ShowAllDemoToolbars(bool AVisible);
  void __fastcall SelectNonRibbonTab(int ATabIndex);
  void __fastcall SelectTab(int ATabIndex);
};
//TfrmMain *frmMain;
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------
#endif
