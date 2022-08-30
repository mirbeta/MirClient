//---------------------------------------------------------------------------

#ifndef BandedDemoMainH
#define BandedDemoMainH
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
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
//---------------------------------------------------------------------------
class TBandedDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBTreeList *cxDBTreeList;
  TcxDBTreeListColumn *cxDBTreeListID;
  TcxDBTreeListColumn *cxDBTreeListPARENTID;
  TcxDBTreeListColumn *cxDBTreeListNAME;
  TcxDBTreeListColumn *cxDBTreeListPHONE;
  TcxDBTreeListColumn *cxDBTreeListFAX;
  TcxDBTreeListColumn *cxDBTreeListBUDGET;
  TcxDBTreeListColumn *cxDBTreeListVACANCY;
  TcxDBTreeListColumn *cxDBTreeListManager;
  TcxDBTreeListColumn *cxDBTreeListManagerPhone;
  TcxDBTreeListColumn *cxDBTreeListManagerEmail;
  TPopupMenu *mnuNodeOptions;
  TMenuItem *miBandDelete;
  TMenuItem *miBandHide;
  TMenuItem *miCustomisationForm;
  TcxEditRepository *cxEditRepository1;
  TcxEditRepositoryMaskItem *eriTelephoneMaskEdit;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall miAddBandClick(TObject *Sender);
  void __fastcall miRemoveBandsClick(TObject *Sender);
  void __fastcall actCustomizationFormExecute(TObject *Sender);
  void __fastcall miBandHorzSizingClick(TObject *Sender);
  void __fastcall miBandVertSizingClick(TObject *Sender);
  void __fastcall miBandMovingClick(TObject *Sender);
  void __fastcall mnuNodeOptionsPopup(TObject *Sender);
  void __fastcall miBandDeleteClick(TObject *Sender);
  void __fastcall miBandHideClick(TObject *Sender);
  void __fastcall cxDBTreeListInitInsertingRecord(TcxCustomDBTreeList *Sender,
      TcxDBTreeListNode *AFocusedNode, bool &AHandled);
private:
  TcxTreeListBand *FHitBand;
  TcxTreeListBand* GetBandByCaption(String ABandCaption);
  void AddBands(TStrings *AStringList);
  void RemoveBands(TcxListBox *AListBox);
public:
  __fastcall TBandedDemoMainForm(TComponent* Owner);
  Variant GetFocusedNodeParentValue();
};
//---------------------------------------------------------------------------
extern PACKAGE TBandedDemoMainForm *BandedDemoMainForm;
//---------------------------------------------------------------------------
#endif
