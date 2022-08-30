//---------------------------------------------------------------------------

#ifndef SimpleListDemoMainH
#define SimpleListDemoMainH
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
#include "cxBlobEdit.hpp"
#include "cxCheckBox.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDBTL.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "CarsData.h"
//---------------------------------------------------------------------------
class TSimpleListDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TcxDBTreeList *cxDBTreeList;
  TcxDBTreeListColumn *cxDBTreeListID;
  TcxDBTreeListColumn *cxDBTreeListTrademark;
  TcxDBTreeListColumn *cxDBTreeListModel;
  TcxDBTreeListColumn *cxDBTreeListPicture;
  TcxDBTreeListColumn *cxDBTreeListPrice;
  TcxDBTreeListColumn *cxDBTreeListHP;
  TcxDBTreeListColumn *cxDBTreeListTorque;
  TcxDBTreeListColumn *cxDBTreeListCyl;
  TcxDBTreeListColumn *cxDBTreeListTransmissSpeedCount;
  TcxDBTreeListColumn *cxDBTreeListTransmissAutomatic;
  TcxDBTreeListColumn *cxDBTreeListMPG_City;
  TcxDBTreeListColumn *cxDBTreeListMPG_Highway;
  TcxDBTreeListColumn *cxDBTreeListCategory;
  TcxDBTreeListColumn *cxDBTreeListHyperlink;
  TcxDBTreeListColumn *cxDBTreeListDescription;
  TcxEditRepository *cxEditRepository;
  TcxEditRepositorySpinItem *cxEditRepositorySpinItem;
  TcxEditRepositoryBlobItem *eriPicture;
  TcxEditRepositorySpinItem *eriHP;
  TcxEditRepositoryBlobItem *eriDescription;
  TcxEditRepositoryHyperLinkItem *eriURL;
  void __fastcall FormShow(TObject *Sender);
  void __fastcall miBandsClick(TObject *Sender);
  void __fastcall miHeadersClick(TObject *Sender);
  void __fastcall miGridLinesClick(TObject *Sender);
  void __fastcall miIncSearchClick(TObject *Sender);
  void __fastcall miFocusCellOnCycleClick(TObject *Sender);
  void __fastcall miImmediateEditorClick(TObject *Sender);
  void __fastcall miMultiSelectClick(TObject *Sender);
private:	// User declarations
public:		// User declarations
  __fastcall TSimpleListDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TSimpleListDemoMainForm *SimpleListDemoMainForm;
//---------------------------------------------------------------------------
#endif
