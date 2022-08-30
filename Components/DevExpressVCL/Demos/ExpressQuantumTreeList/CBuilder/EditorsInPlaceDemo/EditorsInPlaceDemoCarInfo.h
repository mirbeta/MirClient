//---------------------------------------------------------------------------

#ifndef EditorsInPlaceDemoCarInfoH
#define EditorsInPlaceDemoCarInfoH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxBlobEdit.hpp"
#include "cxButtons.hpp"
#include "cxCheckBox.hpp"
#include "cxControls.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxCustomData.hpp"
#include "cxDBTL.hpp"
#include "cxGraphics.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxDBLookupComboBox.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxMRUEdit.hpp"
#include "cxRadioGroup.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include <ExtCtrls.hpp>
#include "CarsData.h"
//---------------------------------------------------------------------------
class TEditorsInPlaceDemoCarInfoForm : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlCarInfo;
  TcxDBTreeList *tlCarInfo;
  TcxDBTreeListColumn *tlCarInfoTrademark;
  TcxDBTreeListColumn *tlCarInfoModel;
  TcxDBTreeListColumn *tlCarInfoPrice;
  TcxDBTreeListColumn *tlCarInfoCategory;
  TcxDBTreeListColumn *tlCarInfoPicture;
  TcxDBTreeListColumn *tlCarInfoBlobImage;
  TcxDBTreeListColumn *tlCarInfoHP;
  TcxDBTreeListColumn *tlCarInfoTorque;
  TcxDBTreeListColumn *tlCarInfoCyl;
  TcxDBTreeListColumn *tlCarInfoTransmissSpeedCount;
  TcxDBTreeListColumn *tlCarInfoTransmissAutomatic;
  TcxDBTreeListColumn *tlCarInfoMPG_City;
  TcxDBTreeListColumn *tlCarInfoMPG_Highway;
  TcxDBTreeListColumn *tlCarInfoDescription;
  TcxDBTreeListColumn *tlCarInfoHyperlink;
  TcxDBTreeListColumn *tlCarInfoCategoryCaption;
  TcxDBTreeListColumn *tlCarInfoCarCaption;
  TcxDBTreeListColumn *tlCarInfoLargePictureCaption;
  TcxDBTreeListColumn *tlCarInfoEngineCaption;
  TcxDBTreeListColumn *tlCarInfoTransmissCaption;
  TcxDBTreeListColumn *tlCarInfoMPG;
  TcxDBTreeListColumn *tlCarInfoPriceCaption;
  TcxDBTreeListColumn *tlCarInfoTransmissAutomatCaption;
  TPanel *pnlButtons;
  TcxButton *btnOK;
  TcxButton *btnCancel;
  void __fastcall btnCancelClick(TObject *Sender);
  void __fastcall btnOKClick(TObject *Sender);
  void __fastcall tlCarInfoTopRecordIndexChanged(TObject *Sender);
  void __fastcall tlCarInfoTrademarkPropertiesButtonClick(TObject *Sender);
  void __fastcall tlCarInfoGetCaptionDisplayText(TcxTreeListColumn *Sender,
    TcxTreeListNode *ANode, String &Value);
  void __fastcall FormShow(TObject *Sender);
private:
  TcxPopupEdit *FPopupEdit;
  bool FAccepted;
  Variant FEditValue;
  void ClosePopup(bool AAccepted);
public:
__fastcall TEditorsInPlaceDemoCarInfoForm(TComponent* Owner);
  void InitPopupPanel(Variant ACarID);
  __property TcxPopupEdit *PopupEdit = {read=FPopupEdit, write=FPopupEdit};
  __property bool Accepted = {read=FAccepted, write=FAccepted};
  __property Variant EditValue = {read=FEditValue};
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoCarInfoForm *EditorsInPlaceDemoCarInfoForm;
//---------------------------------------------------------------------------
#endif
