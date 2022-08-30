//---------------------------------------------------------------------------

#ifndef BandedFixedDemoMainH
#define BandedFixedDemoMainH
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
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "cxCalc.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
//---------------------------------------------------------------------------
class TBandedFixedDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxEditRepository *cxEditRepository1;
  TcxEditRepositoryMaskItem *eriTelephoneMaskEdit;
  TcxEditRepositoryTextItem *cxEditRepository1TextItem1;
  TcxEditRepositoryCalcItem *cxEditRepositoryCalcItem;
  TcxDBTreeList *cxDBTreeList;
  TcxDBTreeListColumn *cxDBTreeListID;
  TcxDBTreeListColumn *cxDBTreeListPROJECTID;
  TcxDBTreeListColumn *cxDBTreeListProjectManagerID;
  TcxDBTreeListColumn *cxDBTreeListEmployee;
  TcxDBTreeListColumn *cxDBTreeListEmployeePhone;
  TcxDBTreeListColumn *cxDBTreeListEmployeeEmail;
  TcxDBTreeListColumn *cxDBTreeListSUNDAY;
  TcxDBTreeListColumn *cxDBTreeListMONDAY;
  TcxDBTreeListColumn *cxDBTreeListTUESDAY;
  TcxDBTreeListColumn *cxDBTreeListWEDNESDAY;
  TcxDBTreeListColumn *cxDBTreeListTHURSDAY;
  TcxDBTreeListColumn *cxDBTreeListFRIDAY;
  TcxDBTreeListColumn *cxDBTreeListSATURDAY;
  TcxDBTreeListColumn *cxDBTreeListWeekSum;
  TcxDBTreeListColumn *cxDBTreeListWeekAVG;
  TPopupMenu *mnuNodeOptions;
  TMenuItem *miFixBand;
  TMenuItem *miFixBandNone;
  TMenuItem *miFixBandLeft;
  TMenuItem *miFixBandRight;
  TMenuItem *miBandHide;
  TMenuItem *CustomisationForm1;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall  actCustomizationFormExecute(TObject *Sender);
  void __fastcall  miBandHorzSizingClick(TObject *Sender);
  void __fastcall  miBandVertSizingClick(TObject *Sender);
  void __fastcall  miBandMovingClick(TObject *Sender);
  void __fastcall  mnuNodeOptionsPopup(TObject *Sender);
  void __fastcall  miBandHideClick(TObject *Sender);
  void __fastcall cxDBTreeListStylesGetContentStyle(TcxCustomTreeList *Sender,
          TcxTreeListColumn *AColumn, TcxTreeListNode *ANode, TcxStyle *&AStyle);
  void __fastcall  cxDBTreeListEmployeeGetDisplayText(TcxTreeListColumn *Sender,
      TcxTreeListNode *ANode, String &Value);
  void __fastcall  cxDBTreeListEmployeeGetEditProperties(
      TcxTreeListColumn *Sender, TcxTreeListNode *ANode,
      TcxCustomEditProperties *&EditProperties);
  void __fastcall  miShowBandsClick(TObject *Sender);
  void __fastcall  miShowHeadersClick(TObject *Sender);
  void __fastcall  miFixBandClick(TObject *Sender);
private:
  TcxTreeListBand *FHitBand;
public:		// User declarations
  __fastcall TBandedFixedDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TBandedFixedDemoMainForm *BandedFixedDemoMainForm;
//---------------------------------------------------------------------------
#endif
