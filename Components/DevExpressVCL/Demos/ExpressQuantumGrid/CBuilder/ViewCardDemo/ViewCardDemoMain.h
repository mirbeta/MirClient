//---------------------------------------------------------------------------

#ifndef ViewCardDemoMainH
#define ViewCardDemoMainH
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
#include "cxTextEdit.hpp"
#include "BaseForm.h"
#include <ExtCtrls.hpp>
#include "cxLookAndFeels.hpp"
#include "cxDataStorage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImageComboBox.hpp"
#include "cxMemo.hpp"
//---------------------------------------------------------------------------
class TViewCardDemoMainForm : public TfmBaseForm
{
__published:  // IDE-managed Components
  TcxGrid *cxGrid;
  TcxGridDBCardView *cvPersons;
  TcxGridDBCardViewRow *cvPersonsFullname;
  TcxGridDBCardViewRow *cvPersonsID;
  TcxGridDBCardViewRow *cvPersonsFIRSTNAME;
  TcxGridDBCardViewRow *cvPersonsSECONDNAME;
  TcxGridDBCardViewRow *cvPersonsGENDER;
  TcxGridDBCardViewRow *cvPersonsBIRTHNAME;
  TcxGridDBCardViewRow *cvPersonsDATEOFBIRTH;
  TcxGridDBCardViewRow *cvPersonsBIRTHCOUNTRY;
  TcxGridDBCardViewRow *cvPersonsLOCATIONOFBIRTH;
  TcxGridDBCardViewRow *cvPersonsNICKNAME;
  TcxGridDBCardViewRow *cvPersonsHOMEPAGE;
  TcxGridDBCardViewRow *cvPersonsBIOGRAPHY;
  TcxGridLevel *lvPersons;
  TMenuItem *miOptions;
  TMenuItem *miShowEmptyRows;
  TMenuItem *miFiltering;
  TMenuItem *miExpandingCollapsing;
  TMenuItem *N1;
  TMenuItem *miRowsCustomization;
  void __fastcall miShowEmptyRowsClick(TObject *Sender);
  void __fastcall miFilteringClick(TObject *Sender);
  void __fastcall miExpandingCollapsingClick(TObject *Sender);
  void __fastcall miRowsCustomizationClick(TObject *Sender);
private:  // User declarations
public:   // User declarations
  __fastcall TViewCardDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TViewCardDemoMainForm *ViewCardDemoMainForm;
//---------------------------------------------------------------------------
#endif
