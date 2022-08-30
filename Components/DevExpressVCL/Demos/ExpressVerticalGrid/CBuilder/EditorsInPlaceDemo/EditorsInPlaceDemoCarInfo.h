//---------------------------------------------------------------------------

#ifndef EditorsInPlaceDemoCarInfoH
#define EditorsInPlaceDemoCarInfoH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxControls.hpp"
#include "cxDBVGrid.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxStyles.hpp"
#include "cxVGrid.hpp"
#include <ExtCtrls.hpp>
//---------------------------------------------------------------------------
class TEditorsInPlaceDemoCarInfoForm : public TForm
{
__published:	// IDE-managed Components
  TPanel *pnlCarInfo;
  TcxDBVerticalGrid *vgCarInfo;
  TcxDBMultiEditorRow *vgCarInfoCar;
  TcxDBMultiEditorRow *vgCarInfoCategory;
  TcxDBEditorRow *vgCarInfoImage;
  TcxDBMultiEditorRow *vgCarInfoLargeImageAndPrice;
  TcxDBMultiEditorRow *vgCarInfoEngine;
  TcxDBMultiEditorRow *vgCarInfoTransmission;
  TcxDBMultiEditorRow *vgCarInfoMPG;
  TcxDBMultiEditorRow *vgCarInfoWebSite;
  TcxDBEditorRow *vgCarInfoDescription;
  TcxDBEditorRow *vgCarInfoID;
  TPanel *Panel2;
  TcxButton *btnOk;
  TcxButton *btnCancel;
  void __fastcall GetDisplayText(TcxCustomEditorRowProperties *Sender,
    int ARecord, String &AText);
  void __fastcall vgCarInfoStylesGetContentStyle(TObject *Sender,
    TcxCustomEditorRowProperties *AEditProp, bool AFocused, int ARecordIndex,
    TcxStyle *&AStyle);
  void __fastcall vgCarInfoLeftVisibleRecordIndexChanged(TObject *Sender);
  void __fastcall cxButtonClick(TObject *Sender);
  void __fastcall OnEditPropertiesButtonClick(TObject *Sender);
private:
  TcxPopupEdit *FPopupEdit;
  bool FAccepted;
  Variant FEditValue;
  void __fastcall ClosePopup(bool AAccepted);
public:
  __fastcall TEditorsInPlaceDemoCarInfoForm(TComponent* Owner);
  void __fastcall InitPopupPanel(Variant ACarID);
  __property TcxPopupEdit *PopupEdit = {read=FPopupEdit, write=FPopupEdit};
  __property bool Accepted = {read=FAccepted, write=FAccepted};
  __property Variant EditValue = {read=FEditValue};
};
//---------------------------------------------------------------------------
extern PACKAGE TEditorsInPlaceDemoCarInfoForm *EditorsInPlaceDemoCarInfoForm;
//---------------------------------------------------------------------------
#endif
