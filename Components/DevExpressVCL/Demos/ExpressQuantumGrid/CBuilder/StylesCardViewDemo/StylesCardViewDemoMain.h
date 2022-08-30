//---------------------------------------------------------------------------

#ifndef StylesCardViewDemoMainH
#define StylesCardViewDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxContainer.hpp"
#include "cxGridCardView.hpp"
#include "cxGridDBCardView.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxStyleSheetEditor.hpp"
#include "cxTextEdit.hpp"
#include <ExtCtrls.hpp>
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxDataStorage.hpp"
#include "cxGridCustomLayoutView.hpp"
//---------------------------------------------------------------------------
class TStylesCardViewDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *cxGrid;
  TcxGridDBCardView *cvDevExpress;
  TcxGridDBCardViewRow *cvDevExpressFullname;
  TcxGridDBCardViewRow *cvDevExpressID;
  TcxGridDBCardViewRow *cvDevExpressFIRSTNAME;
  TcxGridDBCardViewRow *cvDevExpressSECONDNAME;
  TcxGridDBCardViewRow *cvDevExpressGENDER;
  TcxGridDBCardViewRow *cvDevExpressBIRTHNAME;
  TcxGridDBCardViewRow *cvDevExpressDATEOFBIRTH;
  TcxGridDBCardViewRow *cvDevExpressBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvDevExpressLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvDevExpressNICKNAME;
  TcxGridDBCardViewRow *cvDevExpressBIOGRAPHY;
  TcxGridDBCardView *cvSlate;
  TcxGridDBCardViewRow *cvSlateFullName;
  TcxGridDBCardViewRow *cvSlateID;
  TcxGridDBCardViewRow *cvSlateFIRSTNAME;
  TcxGridDBCardViewRow *cvSlateSECONDNAME;
  TcxGridDBCardViewRow *cvSlateGENDER;
  TcxGridDBCardViewRow *cvSlateBIRTHNAME;
  TcxGridDBCardViewRow *cvSlateDATEOFBIRTH;
  TcxGridDBCardViewRow *cvSlateBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvSlateLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvSlateBIOGRAPHY;
  TcxGridDBCardViewRow *cvSlateNICKNAME;
  TcxGridDBCardView *cvHighContrast;
  TcxGridDBCardViewRow *cvHighContrastFullName;
  TcxGridDBCardViewRow *cvHighContrastID;
  TcxGridDBCardViewRow *cvHighContrastFIRSTNAME;
  TcxGridDBCardViewRow *cvHighContrastSECONDNAME;
  TcxGridDBCardViewRow *cvHighContrastGENDER;
  TcxGridDBCardViewRow *cvHighContrastBIRTHNAME;
  TcxGridDBCardViewRow *cvHighContrastDATEOFBIRTH;
  TcxGridDBCardViewRow *cvHighContrastBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvHighContrastLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvHighContrastBIOGRAPHY;
  TcxGridDBCardViewRow *cvHighContrastNICKNAME;
  TcxGridDBCardView *cvUserDefined;
  TcxGridDBCardViewRow *cvUserDefinedFullName;
  TcxGridDBCardViewRow *cvUserDefinedID;
  TcxGridDBCardViewRow *cvUserDefinedFIRSTNAME;
  TcxGridDBCardViewRow *cvUserDefinedSECONDNAME;
  TcxGridDBCardViewRow *cvUserDefinedGENDER;
  TcxGridDBCardViewRow *cvUserDefinedBIRTHNAME;
  TcxGridDBCardViewRow *cvUserDefinedDATEOFBIRTH;
  TcxGridDBCardViewRow *cvUserDefinedBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvUserDefinedLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvUserDefinedBIOGRAPHY;
  TcxGridDBCardViewRow *cvUserDefinedNICKNAME;
  TcxGridLevel *lvDevExpress;
  TcxGridLevel *lvSlate;
  TcxGridLevel *lvHighContrast;
  TcxGridLevel *lvUserDefined;
  TPanel *pnlLeft;
  TGroupBox *GroupBox1;
  TLabel *Label1;
  TLabel *Label2;
  TcxSpinEdit *spedCardWidth;
  TcxSpinEdit *spedCardBorderWidth;
  TcxCheckBox *cbCellAutoHeight;
  TPanel *Panel2;
  TcxButton *btnEdit;
  void __fastcall spedCardWidthPropertiesChange(TObject *Sender);
  void __fastcall cxSpinEdit2PropertiesChange(TObject *Sender);
  void __fastcall cbSellAutoHeightPropertiesChange(TObject *Sender);
  void __fastcall spedCardWidthKeyPress(TObject *Sender, char &Key);
  void __fastcall cxGridActiveTabChanged(TcxCustomGrid *Sender, TcxGridLevel *ALevel);
  void __fastcall btnEditClick(TObject *Sender);
  void __fastcall cxGridLayoutChanged(TcxCustomGrid *Sender, TcxCustomGridView *AGridView);
  void __fastcall FormCreate(TObject *Sender);
private:  // User declarations
  void GetViewOptions(TcxGridDBCardView *AView);
public:   // User declarations
  __fastcall TStylesCardViewDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TStylesCardViewDemoMainForm *StylesCardViewDemoMainForm;
//---------------------------------------------------------------------------
#endif
