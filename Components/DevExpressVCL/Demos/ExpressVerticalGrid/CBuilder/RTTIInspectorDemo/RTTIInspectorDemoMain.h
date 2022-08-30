//---------------------------------------------------------------------------

#ifndef RTTIInspectorDemoMainH
#define RTTIInspectorDemoMainH
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
#include "cxDBVGrid.hpp"
#include "cxInplaceContainer.hpp"
#include "cxOI.hpp"
#include "cxVGrid.hpp"
#include <ExtCtrls.hpp>
#include "cxLookAndFeelPainters.hpp"
//---------------------------------------------------------------------------
class TRTTIInspectorDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
  TPanel *Panel1;
  TLabel *lbInspectedObject;
  TcxRTTIInspector *cxRTTIInspector;
  TcxDBVerticalGrid *cxDBVerticalGrid;
  TcxDBEditorRow *cxDBVerticalGridID;
  TcxDBEditorRow *cxDBVerticalGridCustomerID;
  TcxDBEditorRow *cxDBVerticalGridProductID;
  TcxDBEditorRow *cxDBVerticalGridPurchaseDate;
  TcxDBEditorRow *cxDBVerticalGridPaymentType;
  TcxDBEditorRow *cxDBVerticalGridTime;
  TcxDBEditorRow *cxDBVerticalGridPaymentAmount;
  TcxDBEditorRow *cxDBVerticalGridDescription;
  TcxDBEditorRow *cxDBVerticalGridQuantity;
  TcxDBEditorRow *cxDBVerticalGridCustomerEmail;
  TPopupMenu *PopupMenu;
  TMenuItem *VerticalGridEdit1;
  TMenuItem *VerticalGridLayoutEditor1;
  TcxStyleRepository *cxStyleRepository1;
  TcxStyle *cxStyle1;
  TcxStyle *cxStyle2;
  TcxStyle *cxStyle3;
  TcxStyle *cxStyle4;
  TcxStyle *cxStyle5;
  TcxStyle *cxStyle6;
  TcxStyle *cxStyle7;
  TcxVerticalGridStyleSheet *cxVerticalGridStyleSheetDevExpress;
  TImageList *ImageList;
  TSplitter *Splitter1;
  void __fastcall actVGEditExecute(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
  void __fastcall cxRTTIInspectorPropertyChanged(TObject *Sender);
  void __fastcall cxVerticalGridClick(TObject *Sender);
  void __fastcall actExportVGToHTMLExecute(TObject *Sender);
  void __fastcall actExportVGToExcelExecute(TObject *Sender);
  void __fastcall cxDBVerticalGridLayoutChanged(TObject *Sender);
  void __fastcall actShowCustomizeExecute(TObject *Sender);
  void __fastcall actAboutExecute(TObject *Sender);
private:	// User declarations
  TNotifyEvent FSelectedObjectEvent;
  void __fastcall ObjectSelected(TObject *Sender);
  void __fastcall UpdateLabel(TComponent *AObject);
public:		// User declarations
  __fastcall TRTTIInspectorDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TRTTIInspectorDemoMainForm *RTTIInspectorDemoMainForm;
//---------------------------------------------------------------------------
#endif
