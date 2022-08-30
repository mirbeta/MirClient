//---------------------------------------------------------------------------

#ifndef SparklinesDemoMainH
#define SparklinesDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include <ExtCtrls.hpp>
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
#include "dxmdaset.hpp"
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxDataStorage.hpp"
#include "cxGridCustomLayoutView.hpp"
#include "cxHyperLinkEdit.hpp"
#include "cxImage.hpp"
#include "cxNavigator.hpp"
#include "dxDBSparkline.hpp"
//---------------------------------------------------------------------------
class TfmSparklines : public TfmBaseForm
{ 
__published:  // IDE-managed Components
  TMenuItem *miOptions;
  TMenuItem *miPaymentAmountTotalByTrademark;
  TcxGrid *cxgCarOrders;
  TcxGridDBTableView *cxgCarOrdersTableView;
  TcxGridDBColumn *cxgCarOrdersTableViewRecId;
  TcxGridDBColumn *cxgCarOrdersTableViewTrademark;
  TcxGridDBColumn *cxgCarOrdersTableViewProductName;
  TcxGridDBColumn *cxgCarOrdersTableViewTrademarkSite;
  TcxGridDBColumn *cxgCarOrdersTableViewPhoto;
  TcxGridDBColumn *cxgCarOrdersTableViewPrice;
  TcxGridDBColumn *cxgCarOrdersTableViewPayments;
  TcxGridLevel *cxgCarOrdersLevel1;
  TdxMemData *mdsCarOrders;
  TIntegerField *mdsCarOrdersID;
  TStringField *mdsCarOrdersTrademark;
  TStringField *mdsCarOrdersModel;
  TStringField *mdsCarOrdersTrademark_Site;
  TBlobField *mdsCarOrdersPhoto;
  TCurrencyField *mdsCarOrdersPrice;
  TDataSource *dsCarOrders;
  TdxMemData *mdsOrderDetails;
  TIntegerField *mdsOrderDetailsProductID;
  TIntegerField *mdsOrderDetailsTrademarkID;
  TDateTimeField *mdsOrderDetailsPurchaseData;
  TIntegerField *mdsOrderDetailsCount;
  TCurrencyField *mdsOrderDetailsPrice;
  TCurrencyField *mdsOrderDetailsSales;
  TCurrencyField *mdsOrderDetailsSalesByTrademark;
  TDataSource *dsOrderDetails;
  TMenuItem *Markers1;
  TMenuItem *miShowMinMarkers;
  TMenuItem *miMaxvaluemarkers;
  TMenuItem *miShowValueMarker;
  TMenuItem *miSyle;
  TMenuItem *miArea;
  TMenuItem *miLine;
  TMenuItem *miBar;
  TMenuItem *N1;
  TMenuItem *N2;
  TcxImageList *cxImageList1;
  void __fastcall miPaymentAmountTotalByTrademarkClick(TObject *Sender);
  void __fastcall miSeriesStyleClick(TObject *Sender);
  void __fastcall miMarkersClick(TObject *Sender);
  void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:   // User declarations
  __fastcall TfmSparklines(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmSparklines *fmSparklines;
//---------------------------------------------------------------------------
#endif